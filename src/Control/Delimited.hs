{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Control.Delimited
-- Copyright   : (c) Oleg Kiselyov 2007-2013,
--               (c) Austin Seipp 2012-2013,
--               (c) Leon P Smith 2012-2013
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : Rank-2 types required
--
-- Delimited continuations featuring answer-type polymorphism, via
-- indexed monads.
--
-- There is a tutorial available in "Control.Delimited.Tutorial".
--
module Control.Delimited
       ( -- * Delimited continuations
         Delim     -- :: * -> * -> * -> *
       , (:~>)     -- :: * -> * -> *
       , (:=>)     -- :: * -> * -> *
       , reset     -- :: Delim s t s -> Delim s' s' t

         -- ** A family of shift operators
         -- $shiftfamily
       , shift0    -- :: ((b -> s) -> t) -> Delim s t b
       , shift1    -- :: ((b -> s) -> Delim a t a) -> Delim s t b
       , shift2    -- :: ((b -> forall a'.  Delim a' a' s) -> Delim a t a) -> Delim s t b
       , shift3    -- :: ((forall a'. Delim a' a' b -> Delim a' a' s) -> Delim a t a) -> Delim s t b

         -- ** Traditional @call/cc@
       , callCC    -- :: ((b -> Delim u s a) -> Delim s t b) -> Delim s t b

         -- ** Executing delimited computations
       , runDelim  -- :: Delim t t t -> t

         -- * Re-exports for convenience
       , module Data.Indexed.Functor
       , module Control.Indexed.Applicative
       , module Control.Indexed.Monad
       ) where

import Data.Indexed.Functor
import Control.Indexed.Applicative
import Control.Indexed.Monad

-- | The type of a delimited continuation, which is answer-type
-- polymorphic.
--
-- Functions of type @a -> @'Delim'@ s t b@ can be thought of as
-- functions of type @a \/ s -> b \/ t@, which means given an @a@ we
-- return @b@, changing the /answer type/ of the delimited computation
-- from @s@ to @t@. The take away from this is that the @s@ and @t@
-- variables representing the input and output answer type,
-- respectively. (This notation is used in both OchaCaml and the
-- definition of Asai's lambda/shift calculus.)
--
-- If a @'Delim'@ operation does not capture the computation using one
-- of the various @shift@ operators (or @shift@ does not change the
-- answer type,) then the term is /polymorphic/ in the answer type.
--
-- Consider a term like:
--
-- @
-- 'reset' $ [...] '!>>=' \\r -> 'returnI' $ r ++ \" world\"
-- @
--
-- Here, the answer type of the enclosing @'reset'@ is @'String'@. If
-- the hole (specified by @[...]@) does not have a @shift@ operator,
-- or the @shift@ operator does not change the answer type, then the
-- type of the hole is answer type polymorphic: the hole cannot change
-- the answer type of the enclosing 'reset'.
--
-- To make this clearer, consider using a @shift@ operator to return
-- the delimited continuation to the outer @'reset'@ (which does not
-- modify the answer type, since we can only invoke @k@ to return an
-- 'Int'):
--
-- >>> :t runDelim (reset $ shift2 (\k -> returnI k) !>>= returnI . (+1))
-- runDelim (reset $ shift2 (\k -> returnI k) !>>= returnI . (+1))
--   :: Num t => t -> Delim s s t
--
-- >>> let f = runDelim (reset $ shift2 (\k -> returnI k) !>>= returnI . (+1))
-- >>> runDelim (f 5)
-- 6
--
-- Note how the quantified variable @a'@ is both the input and output
-- answer type of @k@: thus, it cannot change the answer type (here we
-- use @'shift2'@ which features a rank-2 type, perfectly matching the
-- same kind of type you would see in OchaCaml.)
newtype Delim s t b
  = Delim { unDelim :: (b -> s) -> t }

-- | This is a simple alias for a delimited continuation which is
-- polymorphic in its answer type. This corresponds directly to the
-- definition of @'shift2'@.
type (:~>) a b = forall s. a -> Delim s s b

-- | This is a simple alias for a delimited continuation which is
-- polymorphic in its answer type, but where the continuation is
-- monadic even in the input. This corresponds directly to the
-- definition of @'shift3'@.
type (:=>) a b = forall s. Delim s s a -> Delim s s b

-- | Delimited continuations form an @'IxFunctor'@.
instance IxFunctor Delim where
  mapI f (Delim g) = Delim (\k -> g (k . f))
  {-# INLINE mapI #-}

-- | Delimited continuations form an @'IxPointed'@.
instance IxPointed Delim where
  returnI x = Delim (\k -> k x)
  {-# INLINE returnI #-}

-- | Delimited continuations form an @'IxApplicative'@.
instance IxApplicative Delim where
  (Delim f) <!*> (Delim h) = Delim (\k -> h (\s -> f (\t -> k $ t s)))
  {-# INLINE (<!*>) #-}

-- | Delimited continuations form an @'IxMonad'@.
instance IxMonad Delim where
  Delim f !>>= g = Delim (\k -> f (\s -> unDelim (g s) k))
  {-# INLINE (!>>=) #-}

-- | Delimit a computation. The type variable @a@ indicates that
-- @'reset'@ is polymorphic in its answer type.
--
-- >>> runDelim (reset $ returnI "hello")
-- "hello"
--
-- >>> runDelim (returnI 5)
-- 5
reset :: Delim s t s :~> t
reset (Delim f) = Delim (\k -> k (f id))
{-# INLINE reset #-}

{- $shiftfamily

There exists not one, but a family of /equivalent/ @shift@ operators
for delimited continuations, which range in the purity of their
constituent continuation parameters and output types, from most pure
to most impure. This family of operators can be used to define each
other in a stepwise manner.

Briefly, @'shift0'@ is the \"most pure @shift@ of all\" in that the
delimited computation is completely pure. We may then use 'shift0' to
build the definition of @'shift1'@, and use @'shift1'@ to build the
definition of @'shift2'@, and so on until @'shift3'@.

We may then use @'shift3'@, the \"most impure @shift@ of all\", to
walk back downards and define @'shift2'@, and use @'shift2'@ to
describe @'shift1'@ and so on and so forth.

We offer the full family of @shift@ operators here, and where
appropriate we use rank-2 typing to ensure the answer types of a
computation are polymorphic (strictly speaking this likely isn't
necessary, but it makes the type signature far more clear and is
fairly non-controversial.)

(Many thanks go to Leon P Smith who showed me notes describing this
family and \'zipper\' of @shift@ operators.)

-}

-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
--
-- This is the most pure definition of @shift@: both the continuation
-- @k@ and the enclosed body are pure, and may be composed with
-- @'Prelude..'@.
--
-- This is defined in terms of @'Delim'@.
--
-- >>> runDelim $ reset $ shift0 (const "hello") !>>= returnI . (+1)
-- "hello"
--
-- >>> runDelim $ reset $ shift0 (\k -> (k . k) 3) !>>= returnI . (+1)
-- 5
shift0 :: ((b -> s) -> t) -> Delim s t b
shift0 f = Delim f
{-# INLINE shift0 #-}

-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
--
-- This definition of @shift@ prohibits the continuation @k@ from
-- being monadic, while allowing the body to be monadic (this means
-- @k@ is trivially polymorphic in its answer type, while remaining
-- Haskell98.) For this reason it is less pure than @'shift0'@ but more
-- pure than @'shift2'@.
--
-- This is defined in terms of @'shift0'@.
--
-- >>> runDelim $ reset $ shift1 (const $ returnI "hello") !>>= returnI . (+1)
-- "hello"
--
-- >>> runDelim $ reset $ shift1 (\k -> returnI $ (k . k) 3) !>>= returnI . (+1)
-- 5
shift1 :: ((b -> s) -> Delim a t a) -> Delim s t b
shift1 f = shift0 (\k -> unDelim (f k) id)
{-# INLINE shift1 #-}

-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
--
-- This definition of @shift@ is the most \"genuine\" as it perfectly
-- encapsulates the typing rules of Asai's lambda/shift calculus using
-- rank-2 polymorphism: the continuation's answer type is fully
-- polymorphic.
--
-- This is defined in terms of @'shift1'@.
--
-- >>> runDelim $ reset $ shift2 (const $ returnI "hello") !>>= returnI . (+1)
-- "hello"
--
-- >>> runDelim $ reset $ shift2 (\k -> (k !>=> k) 3) !>>= returnI . (+1)
-- 5
shift2 :: ((b :~> s) -> Delim a t a) -> Delim s t b
shift2 f = shift1 (\k -> f (returnI . k))
{-# INLINE shift2 #-}

-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
--
-- This is the most impure definition of @shift@ in that all
-- components of the delimited computation are monadic. It is akin to
-- the definition of @withSubCont@ in Amr Sabry's paper \"A Monadic
-- Framework for Delimited Continuations\", available in the
-- @CC-delcont@ package.
--
-- Like @'shift2'@, this uses rank-2 polymorphism to ensure that the
-- continuation @k@ is polymorphic in its answer type.
--
-- This is defined in terms of @'shift2'@.
--
-- >>> runDelim $ reset $ shift3 (const $ returnI "hello") !>>= returnI . (+1)
-- "hello"
--
-- >>> runDelim $ reset $ shift3 (\k -> k $ returnI 3) !>>= returnI . (+1)
-- 4
shift3 :: ((b :=> s) -> Delim a t a) -> Delim s t b
shift3 f = shift2 (\k -> f (!>>= k))
{-# INLINE shift3 #-}

{--

-- The 'reverse' family of shift operators can also be defined:

shift2' :: ((b :~> s) -> Delim a t a) -> Delim s t b
shift2' f = shift3 (\k -> f (k . returnI))

shift1' :: ((b -> s) -> Delim a t a) -> Delim s t b
shift1' f = shift2' (\k -> f (runDelim . k))

shift0' :: ((b -> s) -> t) -> Delim s t b
shift0' f = shift1' (returnI . f)

--}

-- | Traditional @call/cc@ operator.
--
-- This is defined in terms of @'shift0'@ and @'Delim'@ internally.
callCC :: ((b -> Delim u s a) -> Delim s t b) -> Delim s t b
callCC f = shift0 (\k -> unDelim (f (\a -> shift0 $ \_ -> k a)) k)
{-# INLINE callCC #-}

-- | Run a delimited computation.
--
-- >>> runDelim (reset $ returnI "hello")
-- "hello"
--
-- >>> runDelim (returnI 5)
-- 5
runDelim :: Delim t t t -> t
runDelim (Delim f) = f id
{-# INLINE runDelim #-}
