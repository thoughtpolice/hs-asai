-- |
-- Module      : Control.Indexed.Monad
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Parameterized monads.
--
-- When using @RebindableSyntax@ with GHC, it's possible to overload
-- regular @do@-notation to use the 'Monad'' typeclass. See the
-- section \"Using @do@-notation\" in the tutorial module
-- "Control.Delimited.Tutorial".
--
module Control.Indexed.Monad
       ( -- * Parameterized monads
         Monad'(..) -- :: (* -> * -> * -> *) -> Constraint
       , MW         -- :: (* -> *) -> * -> * -> * -> *

         -- ** Lifting ordinary monads
       , lift       -- :: Monad m => m a -> MW m p q a
       , runI       -- :: Monad m => MW m p q a -> m a

         -- * Operators
       , (!>>=)     -- :: Monad' m => m b g s -> (s -> m a b t) -> m a g t
       , (!+>>)     -- :: Monad' m => m b g s -> m a b t -> m a g t
       ) where

--------------------------------------------------------------------------------
-- Parameterized monads.

-- | Parameterized monads.
--
-- Regular monads can be lifted into this type class using 'lift'.
class Monad' m where
  -- | Parameterized 'Prelude.return'.
  ret  :: t -> m a a t
  -- | Parameterized 'Prelude.>>='.
  bind :: m b g s -> (s -> m a b t) -> m a g t

-- | This type lifts any regular monad into a parameterized monad.
newtype MW m p q a = MW { unMW :: m a }

-- | This method \'lifts\' a regular monad into a parameterized monad
-- 'MW' which is an instance of 'Monad''.
lift :: Monad m => m a -> MW m p q a
lift = MW

-- | This demotes a parameterized monad into a regular monad. Useful
-- for when you're using e.g. @RebindableSyntax@ and want to do IO.
runI :: Monad m => MW m p q a -> m a
runI = unMW

-- | This instances simply lifts regular instances of 'Monad'
-- into instances of 'Monad''.
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
