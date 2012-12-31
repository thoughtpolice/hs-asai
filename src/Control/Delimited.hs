{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Control.Delimited
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : Rank-2 types required
--
-- Delimited continuations featuring answer-type polymorphism, via
-- parameterized monads.
--
-- There is a tutorial available in "Control.Delimited.Tutorial".
--
module Control.Delimited
       ( -- * Delimited continuations
         Delim     -- :: * -> * -> * -> *
       , reset     -- :: Delim s t s -> Delim s' s' t

         -- ** A family of shift operators
       , shift0    -- :: ((b -> s) -> t) -> Delim s t b
       , shift1    -- :: ((b -> s) -> Delim a t a) -> Delim s t b
       , shift2    -- :: ((b -> forall a'.  Delim a' a' s) -> Delim a t a) -> Delim s t b

         -- ** Executing delimited computations
       , runDelim  -- :: Delim t t t -> t

         -- * Re-exports for convenience
       , module Control.Indexed.Monad
       ) where

import Control.Indexed.Monad

-- | The type of a delimited continuation, which is answer-type polymorphic.
--
-- Functions of type @a -> 'Delim' s t b@ can be thought of as
-- functions of type @a \/ s -> b \/ t@, which means given an @a@ we
-- return a @b@, changing the /answer type/ of the continuation from
-- @s@ to @t@.
--
-- If a 'Delim' does not capture the computation using one of the
-- various @shift@ operators (or @shift@ does not change the answer
-- type,) then the term is /polymorphic/ in the answer type.
--
-- >>> :t runDelim $ reset $ shift2 (\k -> ret k) !>>= \r -> ret (r + (1::Int))
-- runDelim $ reset $ shift2 (\k -> ret k) !>>= \r -> ret (r + (1::Int))
--   :: Int -> Delim a' a' Int
--
-- Note how the quantified variable @a'@ is both the input and output
-- answer type: thus, @k@ will work with any answer type and is
-- polymorphic (in this case, the answer type of the enclosing 'reset'
-- is 'Int'.)
newtype Delim s t b
  = Delim { unDelim :: (b -> s) -> t }

-- | Delimited continuations form a parameterized 'Monad''.
instance Monad' Delim where
  ret x            = Delim (\k -> k x)
  bind (Delim f) h = Delim (\k -> f (\s -> unDelim (h s) k))

-- | Delimit a computation.
reset :: Delim s t s -> Delim s' s' t
reset (Delim f) = Delim (\k -> k (f id))

-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
--
-- This is the most pure definition of @shift@.
shift0 :: ((b -> s) -> t) -> Delim s t b
shift0 f = Delim f

-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
--
-- This is a simple Haskell98 definition of @shift@ that does not enforce
-- true answer type polymorphism by abstracting over the internal 's' and
-- 't' type variables.
shift1 :: ((b -> s) -> Delim a t a) -> Delim s t b
shift1 f = Delim (\k -> unDelim (f k) id)

-- | Clear the current continuation and invoke our handler with it
-- bound as a paramter.
--
-- This definition of @shift@ uses Rank-2 types to ensure the answer
-- type is in fact polymorphic: note the type of the captured continuation
-- is @Delim t' t' s@.
shift2 :: ((b -> forall a'.  Delim a' a' s) -> Delim a t a) -> Delim s t b
shift2 f = Delim (\k -> unDelim (f $ \t -> ret (k t)) id)

-- | Run a delimited computation.
runDelim :: Delim t t t -> t
runDelim (Delim f) = f id
