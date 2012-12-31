{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RebindableSyntax #-}
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

         -- * Notes on @do@-notation
         -- $donotation

         -- * References
         -- $refs
       ) where
import Prelude hiding (return, fail, (>>=), (=<<))
import Control.Delimited

--
-- Aspects of RebindableSyntax
--

m >>= f  = m !>>= f
return x = ret x
fail x   = error x
f =<< m  = m !>>= f

{- $intro

Lorem ipsum...

>>> runDelim $ reset $ (shift1 (\_ -> return "hello") >>= \r -> return (r + 1))
"hello"

-}

{- $donotation

Lorem ipsum...

-}

{- $refs

  * /Polymorphic Delimited Continuations/, by Asai, Kameyama in /APLAS '07/:
    <http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf>

  * /Genuine shift\reset in Haskell98/, by Kiselyov, on /haskell-cafe/:
    <http://okmij.org/ftp/continuations/implementations.html#genuine-shift>

  * /Introduction to programming with shift and reset/, by Kiselyov, Asai, in /CW2011/:
    <http://okmij.org/ftp/continuations/index.html#tutorial>

-}
