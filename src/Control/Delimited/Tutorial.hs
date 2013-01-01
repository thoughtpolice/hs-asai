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

         -- * Delimited continuations
         -- $delimcc

         -- ** Part 1
         -- $pt1

         -- ** Part 2
         -- $pt2

         -- * Other notes
         -- $othernotes

         -- ** Parameterized monads
         -- $pmonads

         -- ** Using @do@-notation
         -- $donotation

         -- ** Rank-2 typing
         -- $rankntypes

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

Continuations are a well known abstraction for 'picking up where you
left off.' When you use @call/cc@ traditionally in something like
scheme, you pass it a function @f@ which receives a function @k@, and
when @k@ is invoked you 'call the continuation' and return where
@call/cc@ left off. In this sense, @k@ is a reification of the state
you were in when you invoked @f@.

But @call/cc@ is, as some say, 'overrated.' For one, it is not
possible to return a value from the continuation; you merely pick up
where you left off. By fixing this we get delimited continuations: an
abstraction that allows us to slice up a continuation and compose
them. But at this point, we suffer in a statically typed language: the
type of @shift@ and @reset@ are not polymorphic in their return.

In the same way that continuations form a monad, so do delimited
continuations. This package provides a delimited continuation monad
which implements truly polymorphic @shift@ and @reset@ operators, via
/answer type polymorphism/, which allows the results of a delimited
computation to vary.

This implementation (using parameterized monads) was first implemented
by - and this package derived from - Oleg Kiselyov [1]. It directly
implements the typing rules of Kenichi Asai's lambda/shift calculus
[2], featuring answer type polymorphism and modification.

A general tutorial on delimited continuations in OchaCaml (with code
in Haskell and OCaml) from Asai/Kiselyov is available [3]. A more
traditional delimited continuation monad is available in the
@CC-delcont@ package [4].

-}

{- $delimcc

Lorem ipsum...

-}

{- $pt1

Lorem ipsum...

>>> runDelim $ reset $ shift1 (\_ -> ret "hello") !>>= \r -> ret (r + 1)
"hello"

-}

{- $pt2

Lorem ipsum...

>>> runDelim $ reset $ shift1 (\_ -> return "hello") >>= \r -> return (r + 1)
"hello"

-}

{- $othernotes

Here we discuss some of the design aspects of the library,
particularly for those wondering why we need parameterized (or
/indexed/) monads, and how we can reconcile this with @do@-notation.

-}

{- $pmonads

While reading this tutorial, you may wonder why we use special
operators (like '!>>=' that mimmick 'Prelude.>>=') for delimited
computations, instead of regular operators from the 'Monad'
typeclass. The reason for this is that in order to ensure the answer
type of delimited computation is polymorphic, we need the monad to
\'carry\' the answer types around.

Consider the vanilla 'Monad' typeclass. It is defined like this (with
explicit kind signatures):

> class Monad (m :: * -> *) where ...

The @m@ type constructor abstracts over a single type variable. It is
possible to make types with multiple type variables an instance of
'Monad' of course, but their non-abstracted type variables must be
fixed. As an example, considering the instance for @'Either' e@:

> instance Monad (Either e) where ...

Note the type variable @e@ is fixed over the definition of a term of
type @Either e :: * -> *@. If you have something like:

> thing :: a -> Either String a
> thing a = do
>   ...

Then in the body we may say:

> x <- Left "oh no!"

But we can never say:

> x <- Left False

because @e@ is fixed to 'String'.

Parameterized (or /indexed/) monads solve this problem by
\'expanding\' the kind of @m@ in the 'Monad' typeclass. It is defined
as:

> class Monad' (m :: * -> * -> * -> *) where
>   ret  :: t -> m a a t
>   bind :: m b g s -> (s -> m a b t) -> m a g t

Note the new type variables: these represent the input and output
answer types of a delimited computation. We can see that 'ret' is
fully polymorphic in its answer type: a statement of @ret foo@ in a
'Delim' simply does not change the answer type at all. Note the answer
types present in 'Control.Indexed.Monad.bind': we have @a b@ and @b
g@, meaning we can get an overall answer type of @a g@.

This polymorphism in the definition of 'Control.Indexed.Monad.bind' is
what gives us answer type polymorphism: it means delimited
computations may change the output answer type.

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
> import Prelude hiding (return, fail, (>>=), (=<<), (>>))
> import Control.Delimited
>
> -- Aspects of RebindableSyntax
> return x = ret x
> fail s   = error s
> m >>= f  = m !>>= f
> f =<< m  = m !>>= f
> f >> k   = m !>>= \_ -> k
>
> -- Now use 'do' notation instead of the indexed bind/return
> -- functions.
>
> -- You can lift regular monads into parameterized monads using
> -- 'lift' and 'runI'
> io1 :: IO ()
> io1 = runI $ do
>   lift $ putStrLn "hi!"
>   lift $ putStrLn "hi!"
>   return ()
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

{- $rankntypes

This package requires GHC's @RankNTypes@ extension, as it uses a
rank-2 type for the definition of 'Control.Delimited.shift2'. The
original implementation by Kiselyov is Haskell98. You do not need to
enable @RankNTypes@ to use this package.

-}

{- $refs

  1. /Genuine shift\reset in Haskell98/, by Kiselyov, on /haskell-cafe/:
     <http://okmij.org/ftp/continuations/implementations.html#genuine-shift>

  2. /Polymorphic Delimited Continuations/, by Asai, Kameyama in /APLAS '07/:
     <http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf>

  3. /Introduction to programming with shift and reset/, by Kiselyov, Asai, in /CW2011/:
      <http://okmij.org/ftp/continuations/index.html#tutorial>

  4. /CC-delcont: Delimited continuations and dynamically scoped variables/: <http://hackage.haskell.org/package/CC-delcont>

-}
