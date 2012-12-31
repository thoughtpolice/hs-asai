{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Control.Monad.Cont.Delimited
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Delimited, answer-type-polymorphic continuations.
--
module Control.Monad.Cont.Delimited
       ( -- * Parameterized monads
         Monad'(..)
       , (!>>=)
       , (!+>>)
         -- * Delimited continuations
       , Delim
       , reset
       , shift
       , runDelim
       ) where

--------------------------------------------------------------------------------
-- Parameterized monads.

-- | Parameterized monads.
class Monad' m where
  -- | Parameterized 'Prelude.return'.
  ret  :: t -> m a a t
  -- | Parameterized 'Prelude.>>='.
  bind :: m b g s -> (s -> m a b t) -> m a g t

-- | This newtype lifts any regular monad into a parameterized monad.
newtype MW m p q a = MW { unMW :: m a }

instance Monad m => Monad' (MW m) where
  ret         x = MW (return x)
  bind (MW m) f = MW (m >>= unMW . f)

-- | Infix synonym for 'bind'.
(!>>=) :: Monad' m => m b g s -> (s -> m a b t) -> m a g t
m !>>= f = bind m f
infixl 1 !>>=

-- | Defined as:
--
-- @m1 !+>> m2 = bind m1 (const m2)@
(!+>>) :: Monad' m => m b g s -> m a b t -> m a g t
m1 !+>> m2 = bind m1 (const m2)
infixl 1 !+>>

--------------------------------------------------------------------------------
-- Delimited continuations.

-- | The type of a delimited continuation, which is answer-type polymorphic.
newtype Delim a b t
  = Delim { unDelim :: (t -> a) -> b }

instance Monad' Delim where
  ret x            = Delim (\k -> k x)
  bind (Delim f) h = Delim (\k -> f (\s -> unDelim (h s) k))

-- | Delimit a computation.
reset :: Delim s t s -> Delim a a t
reset (Delim f) = Delim (\k -> k (f id))

{-- Rank-2 based 'shift' definition.
-- | Clear the current continuation and invoke our handler with it bound
-- as a paramter.
shift :: ((t -> forall t'. Delim t' t' a) -> Delim s b s) -> Delim a b t
shift f = Delim (\k -> unDelim (f $ \t -> ret (k t)) id)
--}

{-- Haskell-98 'shift' definition. -}
-- | Clear the current continuation and invoke our handler with it bound
-- as a paramter.
shift :: ((t -> a) -> Delim s b s) -> Delim a b t
shift f = Delim (\k -> unDelim (f k) id)
--}

-- | Run a delimited computation.
runDelim :: Delim t t t -> t
runDelim (Delim f) = f id
