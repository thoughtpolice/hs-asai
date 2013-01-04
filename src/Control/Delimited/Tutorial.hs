{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
-- |
-- Module      : Control.Delimited.Tutorial
-- Copyright   : (c) Oleg Kiselyov 2007-2012, (c) Austin Seipp 2012
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides an introductory tutorial in the
-- \"Introduction\" section and beyond, followed by examples, and by
-- some discussion behind the theory of the library with references.
--
module Control.Delimited.Tutorial
       ( -- * Introduction
         -- $intro

         -- * An primer on continuations
         -- $callcc-primer

         -- ** Undelimited
         -- $primer-callcc

         -- ** Delimited
         -- $primer-delimcc

         -- * Examples
         -- $examples

         -- ** Simple answer-type modification
         -- $example-simple

         -- ** First-class @printf@
         -- $example-printf

         -- ** Walking binary trees
         -- $example-btrees

         -- * Other notes
         -- $othernotes

         -- ** Indexed monads
         -- $pmonads

         -- ** Using @do@-notation
         -- $donotation

         -- ** Rank-2 typing
         -- $rankntypes

         -- * Further reading
         -- $morereading

         -- * References
         -- $refs
       ) where

import Control.Indexed.Prelude
import Control.Delimited

{- $intro

@asai@ is a minimal library for /delimited continuations/, a \'slice\'
of a continuation that can be invoked as a function and composed.

-}

{- $callcc-primer

Continuations are a well known abstraction for 'picking up where you
left off.' When you use @call/cc@ traditionally in something like
scheme, you pass it a function @f@ which receives a function @k@, and
when @k@ is invoked you 'call the continuation' and return where
@call/cc@ left off. In this sense, @k@ is a reification of the state
you were in when you invoked @f@.

But there is another type of continuation: a delimited one. With
delimited continuations, it is possible to exert control over the
exact /frame/ of the computation that is captured. By being able to
slice just small execution contexts, we can compose delimited
continuations very easily.

In the same way that continuations form a monad, so do delimited
continuations. This package provides a delimited continuation monad
which implements truly polymorphic @shift@ and @reset@ operators, via
/answer type polymorphism/, which allows the results of a delimited
computation to vary.

This implementation (using indexed monads) was first implemented
by - and this package derived from - Oleg Kiselyov [1]. It directly
implements the typing rules of Kenichi Asai's lambda/shift calculus
[2], featuring answer type polymorphism and modification.

A general tutorial on delimited continuations in OchaCaml (with code
in Haskell and OCaml) from Asai/Kiselyov is available [3]. A more
traditional delimited continuation monad is available in the
@CC-delcont@ package [4]. The @CC-delcont@ tutorial [5] serves
as a basis for this tutorial (thanks to Dan Doel!)

-}

{- $primer-callcc

Lorem ipsum...

-}

{- $primer-delimcc

Lorem ipsum...

>>> runDelim $ reset $ shift1 (\_ -> ret "hello") !>>= \r -> ret (r + 1)
"hello"

-}

{- $examples

In the subsequent sections, we'll cover some examples of using this
library in a few simple forms, which should form a basis for reasoning
about how to use it. The same ideas should apply to other delimited
continuation libraries or interfaces (i.e. scheme, ocaml, ochacaml,
etc.)

-}

{- $example-simple

Lorem ipsum...

>>> runDelim $ reset $ shift1 (\_ -> return "hello") >>= \r -> return (r + 1)
"hello"

-}

{- $example-printf

Lorem ipsum...

-}

{- $example-btrees

Here, we will...

> {-# LANGUAGE RankNTypes #-}
> -- Walking trees with coroutines.
> -- Based on:
> --    <http://okmij.org/ftp/continuations/ContExample.hs>
> module Tree where
>
> import Control.Delimited
>
> -- Binary trees
> data Tree a
>   = Leaf
>   | Node (Tree a) (Tree a) a
>   deriving (Show, Eq)
>
> make_tree :: Int -> Tree Int
> make_tree j = go j 1
>   where go 0 _ = Leaf
>         go i x = Node (go (i-1) $ 2*x) (go (i-1) $ 2*x+1) x
>
> tree1 = make_tree 3
> tree2 = make_tree 4
>
> -- Coroutines as lazy monadic lists.
> data Coro a
>   = Done
>   | Resume a (forall s. Delim s s (Coro a))
>
> walk_tree x = runDelim (walk_tree' x !>> ret Done)
>
> walk_tree' Leaf = ret ()
> walk_tree' (Node l r x) =
>   walk_tree' l !>>
>   yield x      !>>
>   walk_tree' r
>   where
>     yield n = shift2 (\k -> ret $ Resume n $ k ())
>
> walk1 t = go (walk_tree t)
>   where
>     go Done         = return ()
>     go (Resume x k) = print x >> go (runDelim k)

The full code is available in @examples/Tree.hs@.

-}

{- $othernotes

Here we discuss some of the design aspects of the library,
particularly for those wondering why we need indexed (or
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

Indexed monads solve this problem by \'expanding\' the kind of @m@ in
the 'Monad' typeclass. The result is 'IxMonad', which is defined as:

> class IxMonad (m :: * -> * -> * -> *) where
>   ret    :: t -> m a a t
>   (!>>=) :: m b g s -> (s -> m a b t) -> m a g t

Note the new type variables: these represent the input and output
answer types of a delimited computation. We can see that 'ret' is
fully polymorphic in its answer type: a statement of @ret foo@ in a
'Delim' simply does not change the answer type at all. Note the answer
types present in 'Control.Indexed.Monad.!>>=': we have an answer type
of @a b@ and @b g@, meaning we can get an overall answer type of @a
g@.

This polymorphism in the definition of 'Control.Indexed.Monad.!>>=' is
what gives us answer type polymorphism: it means delimited
computations may change the output answer type.

-}

{- $donotation

It's possible to use GHC's @RebindableSyntax@ extension to re-define
@do@ notation to use the 'IxMonad' type class.

Begin your module by hiding the regular 'Monad' methods, and then
redefine 'Prelude.>>=' and 'Prelude.return'. Feel free to redefine
other operators too. Here's an example (you'll need to fix the
@LANGUAGE@ pragma yourself on the first line, since Haddock eats it
otherwise):

> [-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-]
> module Foo where
>
> import Control.Indexed.Prelude
> import Control.Delimited
>
> -- Now use 'do' notation instead of the indexed bind/return
> -- functions.
>
> -- You can lift regular monads into indexed monads using
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
rank-2 type for the definition of the @shift@ operators 'shift2' and
'shift3'. The original implementation by Kiselyov is Haskell98 (he
defines @shift@ as the 'shift1' provided in this package, as opposed
to 'shift2' which matches the typing rules of the lambda/shift
calculus.)

Strictly speaking, the rank-2 type is probably not necessary, but it
is not very controversial either, and it makes the intent much
clearer.

For a lot of cases, you will need to use @RankNTypes@ if you want to
abstract over the answer type variables properly (for example, in a
recursive data structure.)

-}

{- $morereading

Lorem ipsum...

-}

{- $refs

  1. /Genuine shift\reset in Haskell98/, by Kiselyov, on /haskell-cafe/:
     <http://okmij.org/ftp/continuations/implementations.html#genuine-shift>

  2. /Polymorphic Delimited Continuations/, by Asai, Kameyama in /APLAS '07/:
     <http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf>

  3. /Introduction to programming with shift and reset/, by Kiselyov, Asai, in /CW2011/:
      <http://okmij.org/ftp/continuations/index.html#tutorial>

  4. /CC-delcont: Delimited continuations and dynamically scoped variables/: <http://hackage.haskell.org/package/CC-delcont>

  5. /CC-delcont introduction/: <http://www.haskell.org/haskellwiki/Library/CC-delcont#CC-delcont>

-}
