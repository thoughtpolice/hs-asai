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
-- \"Introduction\" section, followed by some discussion behind the
-- theory of the library with references.
--
module Control.Delimited.Tutorial
       ( -- * Introduction
         -- $intro

         -- * References
         -- $refs
       ) where
import Control.Delimited

{- $intro

Lorem ipsum...

>>> runDelim $ reset (shift (\_ -> ret "hello") !>>= \r -> ret (r + 1))
"hello"

-}

{- $refs

  * /Polymorphic Delimited Continuations/, by Asai, Kameyama in /APLAS '07/:
    <http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf>

  * /Genuine shift\reset in Haskell98/, by Kiselyov, on /haskell-cafe/:
    <http://okmij.org/ftp/continuations/implementations.html#genuine-shift>

  * /Introduction to programming with shift and reset/, by Kiselyov, Asai, in /CW2011/:
    <http://okmij.org/ftp/continuations/index.html#tutorial>

-}
