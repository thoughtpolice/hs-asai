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
       , shift     -- :: ((b -> s) -> Delim a t a) -> Delim s t b
       , shift'    -- :: ((b -> forall a'.  Delim a' a' s) -> Delim a t a) -> Delim s t b
       , runDelim  -- :: Delim t t t -> t

         -- * Re-exports for convenience
       , module Control.Indexed.Monad
       ) where

import Control.Indexed.Monad

--------------------------------------------------------------------------------
-- Delimited continuations.

-- | The type of a delimited continuation, which is answer-type polymorphic.
--
-- Functions of type @a -> C s t b@ can be thought of as functions
-- of type @a / s -> b / t@, which means given an @a@ we return a @b@,
-- changing the answer type from @s@ to @t@.
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
shift :: ((b -> s) -> Delim a t a) -> Delim s t b
shift f = Delim (\k -> unDelim (f k) id)
{- Haskell-98 'shift' definition. -}

-- | Clear the current continuation and invoke our handler with it
-- bound as a paramter.
--
-- This definition of @shift@ uses Rank-2 types to ensure the answer
-- type is in fact polymorphic.
shift' :: ((b -> forall a'.  Delim a' a' s) -> Delim a t a) -> Delim s t b
shift' f = Delim (\k -> unDelim (f $ \t -> ret (k t)) id)
{- Rank-2 based 'shift' definition. -}

-- | Run a delimited computation.
runDelim :: Delim t t t -> t
runDelim (Delim f) = f id
