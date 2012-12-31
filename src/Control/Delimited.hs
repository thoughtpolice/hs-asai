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
       , reset     -- :: Delim s t s -> Delim a a t
       , shift     -- :: ((t -> a) -> Delim s b s) -> Delim a b t
       , shift'    -- :: ((t -> forall t' -> Delim t' t' a) -> Delim s b s) -> Delim a b t
       , runDelim  -- :: Delim t t t -> t

         -- * Re-exports for convenience
       , module Control.Indexed.Monad
       ) where

import Control.Indexed.Monad

--------------------------------------------------------------------------------
-- Delimited continuations.

-- | The type of a delimited continuation, which is answer-type polymorphic.
newtype Delim a b t
  = Delim { unDelim :: (t -> a) -> b }

-- | Delimited continuations form a parameterized 'Monad''.
instance Monad' Delim where
  ret x            = Delim (\k -> k x)
  bind (Delim f) h = Delim (\k -> f (\s -> unDelim (h s) k))

-- | Delimit a computation.
reset :: Delim s t s -> Delim a a t
reset (Delim f) = Delim (\k -> k (f id))

{-- Haskell-98 'shift' definition. -}
-- | Clear the current continuation and invoke our handler with it
-- bound as a parameter.
shift :: ((t -> a) -> Delim s b s) -> Delim a b t
shift f = Delim (\k -> unDelim (f k) id)
--}

{-- Rank-2 based 'shift' definition. -}
-- | Clear the current continuation and invoke our handler with it
-- bound as a paramter.
--
-- This definition of @shift@ uses Rank-2 types to ensure the answer
-- type is in fact polymorphic.
shift' :: ((t -> forall t'. Delim t' t' a) -> Delim s b s) -> Delim a b t
shift' f = Delim (\k -> unDelim (f $ \t -> ret (k t)) id)
--}

-- | Run a delimited computation.
runDelim :: Delim t t t -> t
runDelim (Delim f) = f id
