{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Control.Monad.Cont.Delimited
-- Copyright   : (c) Austin Seipp 2012
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
         Monadish(..)
       , (!>>=)
       , (!+>>)
         -- * Delimited continuations
       , CC
       , reset
       , shift
       , run
       ) where

--------------------------------------------------------------------------------
-- Parameterized monads.

-- | Parameterized monads.
class Monadish m where
  -- | Parameterized 'Prelude.return'.
  ret  :: t -> m a a t
  -- | Parameterized 'Prelude.>>='.
  bind :: m b g s -> (s -> m a b t) -> m a g t

-- | This newtype lifts any regular monad into a parameterized monad.
newtype MW m p q a = MW { unMW :: m a }

instance Monad m => Monadish (MW m) where
  ret         x = MW (return x)
  bind (MW m) f = MW (m >>= unMW . f)

-- | Infix synonym for 'bind'.
(!>>=) :: Monadish m => m b g s -> (s -> m a b t) -> m a g t
m !>>= f = bind m f
infixl 1 !>>=

-- | Defined as:
--
-- @m1 !+>> m2 = bind m1 (const m2)@
(!+>>) :: Monadish m => m b g s -> m a b t -> m a g t
m1 !+>> m2 = bind m1 (const m2)
infixl 1 !+>>

--------------------------------------------------------------------------------
-- Delimited continuations.

-- | The type of a delimited continuation, which is answer-type polymorphic.
newtype CC a b t = CC { unCC :: (t -> a) -> b }

instance Monadish CC where
  ret x         = CC (\k -> k x)
  bind (CC f) h = CC (\k -> f (\s -> unCC (h s) k))

-- | Delimit a computation.
reset :: CC s t s -> CC a a t
reset (CC f) = CC (\k -> k (f id))

{-- Rank-2 based 'shift' definition.
-- | Clear the current continuation and invoke our handler with it bound
-- as a paramter.
shift :: ((t -> forall t'. CC t' t' a) -> CC s b s) -> CC a b t
shift f = CC (\k -> unCC (f $ \t -> ret (k t)) id)
--}

{-- Haskell-98 'shift' definition. -}
-- | Clear the current continuation and invoke our handler with it bound
-- as a paramter.
shift :: ((t -> a) -> CC s b s) -> CC a b t
shift f = CC (\k -> unCC (f k) id)
--}

-- | Run a delimited computation.
run :: CC t t t -> t
run (CC f) = f id
