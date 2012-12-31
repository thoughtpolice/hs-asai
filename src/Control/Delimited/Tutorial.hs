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

         -- * Using @do@-notation
         -- $donotation

         -- * Other notes
         -- $othernotes

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

It's possible to use GHC's @RebindableSyntax@ extension to re-define
@do@ notation to use the 'Monad'' type class.

Begin your module by hiding the regular 'Monad' methods, and then
redefine 'Prelude.>>=' and 'Prelude.return'. Feel free to redefine
other operators too. Here's an example (you'll need to fix the
@LANGUAGE@ pragma yourself on the first line, since Haddock eats it
otherwise):

> [-# LANGUAGE RebindableSyntax #-]
> module Foo where
> import Prelude hiding (return, fail, (>>=), (=<<))
> import Control.Delimited
>
> -- Aspects of RebindableSyntax
> m >>= f  = m !>>= f
> return x = ret x
> f =<< m  = m !>>= f
>
> -- Now use 'do' notation instead of the indexed bind/return
> -- functions.
>
> io1 :: IO ()
> io1 = do
>   putStrLn "hi!"
>
> test1 :: String
> test1 = runDelim $ reset $ do
>   r <- shift1 (\_ -> return "hello")
>   return (r + 1)
> -- This is equivalent to the OchaCaml term:
> --   reset (fun () -> 1 + shift (fun _ -> "hello")) ;;

See @examples/Simple.hs@ (included in the distribution) for several
more examples.

-}

{- $othernotes

This package requires GHC's @RankNTypes@ extension, as it uses it for
the definition of 'Control.Delimited.shift2'. The original
implementation by Kiselyov is Haskell98. You do not need to enable
@RankNTypes@ to use this package.

-}

{- $refs

  * /Polymorphic Delimited Continuations/, by Asai, Kameyama in /APLAS '07/:
    <http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf>

  * /Genuine shift\reset in Haskell98/, by Kiselyov, on /haskell-cafe/:
    <http://okmij.org/ftp/continuations/implementations.html#genuine-shift>

  * /Introduction to programming with shift and reset/, by Kiselyov, Asai, in /CW2011/:
    <http://okmij.org/ftp/continuations/index.html#tutorial>

-}
