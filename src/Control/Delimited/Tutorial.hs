{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module      : Control.Delimited.Tutorial
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides a brief introductory tutorial in the
-- \"Introduction\" section followed by some discussion behind the
-- theory, with references.
--
module Control.Delimited.Tutorial
       ( -- * Introduction
         -- $intro

       ) where
import Control.Delimited

{- $intro

Lorem ipsum...

>>> runDelim $ reset (shift (\_ -> ret "hello") !>>= \r -> ret (r + 1))
"hello"

-}
