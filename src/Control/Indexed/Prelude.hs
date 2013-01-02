-- |
-- Module      : Control.Indexed.Prelude
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- When using @RebindableSyntax@ with GHC, it's possible to overload
-- regular @do@-notation to use the 'IxMonad' typeclass. See the
-- section \"Using @do@-notation\" in the tutorial module
-- "Control.Delimited.Tutorial".
--
module Control.Indexed.Prelude
       ( -- * Redefined prelude operators
         (>>=)  -- :: IxMonad m => m t u a -> (a -> m s t b) -> m s u b
       , (>>)   -- :: IxMonad m => m t u a -> m s t b -> m s u b
       , (=<<)  -- :: IxMonad m => (a -> m s t b) -> m t u a -> m s u b
       , return -- :: IxMonad m => a -> m s s a
       , fail   -- :: IxMonad m => Prelude.String -> m s s a

         -- * The prelude itself.
       , module Prelude
       ) where

import Prelude hiding (return, fail, (>>=), (=<<), (>>))
import Control.Delimited

-- | Indexed Prelude 'Prelude.>>='.
(>>=) :: IxMonad m => m t u a -> (a -> m s t b) -> m s u b
(>>=)    = (!>>=)

-- | Indexed Prelude 'Prelude.>>'.
(>>) :: IxMonad m => m t u a -> m s t b -> m s u b
(>>)     = (!>>)

-- | Indexed Prelude 'Prelude.=<<'.
(=<<) :: IxMonad m => (a -> m s t b) -> m t u a -> m s u b
(=<<)    = (=<<!)

-- | Indexed Prelude 'Prelude.return'.
return :: IxMonad m => a -> m s s a
return x = ret x

-- | Indexed Prelude 'Prelude.fail'.
fail :: IxMonad m => Prelude.String -> m s s a
fail   x = fail' x
