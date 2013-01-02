-- |
-- Module      : Control.Indexed.Monad
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Indexed monads.
--
-- When using @RebindableSyntax@ with GHC, it's possible to overload
-- regular @do@-notation to use the 'IxMonad' typeclass. See the
-- section \"Using @do@-notation\" in the tutorial module
-- "Control.Delimited.Tutorial".
--
module Control.Indexed.Monad
       ( -- * Indexed monads
         IxMonad(..) -- :: (* -> * -> * -> *) -> Constraint

         -- ** Lifting ordinary monads
       , MW          -- :: (* -> *) -> * -> * -> * -> *
       , lift        -- :: IxMonad m => m a -> MW m s t a
       , runI        -- :: IxMonad m => MW m s t a -> m a

         -- * Operators
       , (!>=>)      -- :: IxMonad m => (a -> m t u b) -> (b -> m s t c) -> (a -> m s u c)
       , (<=<!)      -- :: IxMonad m => (b -> m s t c) -> (a -> m t u b) -> (a -> m s u c)
       , (=<<!)      -- :: IxMonad m => (a -> m s t b) -> m t u a -> m s u b
       ) where

--------------------------------------------------------------------------------
-- Indexed monads.

-- | Indexed monads.
--
-- Regular monads can be lifted into this type class using 'lift'.
class IxMonad m where
  -- | Indexed 'Prelude.>>='.
  (!>>=) :: m t u a -> (a -> m s t b) -> m s u b
  -- | Indexed 'Prelude.>>'.
  (!>>) ::m t u a -> m s t b -> m s u b
  f !>> g = f !>>= const g
  -- | Indexed 'Prelude.return'.
  ret  :: a -> m s s a
  -- | Indexed 'Prelude.fail'.
  fail' :: String -> m s s a
  fail' s = error s

infixl 1 !>>=, !>>

-- | This type lifts any regular monad into a indexed monad.
newtype MW m s t a = MW { unMW :: m a }

-- | This method \'lifts\' a regular monad into a indexed monad
-- 'MW' which is an instance of 'IxMonad'.
lift :: Monad m => m a -> MW m s t a
lift = MW

-- | This demotes a indexed monad into a regular monad. Useful
-- for when you're using e.g. @RebindableSyntax@ and want to do IO.
runI :: Monad m => MW m s t a -> m a
runI = unMW

-- | This instances simply lifts regular instances of 'Monad'
-- into instances of 'IxMonad'.
instance Monad m => IxMonad (MW m) where
  ret       x = MW (return x)
  MW m !>>= f = MW (m >>= unMW . f)

-- | Left-to-right Kleisli composition of indexed monads.
(!>=>) :: IxMonad m => (a -> m t u b) -> (b -> m s t c) -> (a -> m s u c)
f !>=> g = \x -> f x !>>= g

-- | Right-to-left Kleisli composition of indexed monads.
(<=<!) :: IxMonad m => (b -> m s t c) -> (a -> m t u b) -> (a -> m s u c)
(<=<!) = flip (!>=>)

-- | Right-to-left version of '!>>='.
(=<<!) :: IxMonad m => (a -> m s t b) -> m t u a -> m s u b
(=<<!) = flip (!>>=)

infixr 1 <=<!, !>=>, =<<!
