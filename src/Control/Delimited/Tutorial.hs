{-# OPTIONS_GHC -w #-}
{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
-- |
-- Module      : Control.Delimited.Tutorial
-- Copyright   : (c) Oleg Kiselyov 2007-2013, (c) Austin Seipp 2012-2013
-- License     : MIT
--
-- Maintainer  : mad.one@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides an introductory tutorial in the
-- \"Introduction\" section and beyond, followed by examples. Finally,
-- there's some discussion behind the theory and design of the
-- library, with references.
--
module Control.Delimited.Tutorial
       ( -- * Introduction
         -- $intro

         -- * An primer on continuations
         -- $callcc-primer

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

{- $primer-delimcc

Let us consider the expression:


-}

{- $examples

In the subsequent sections, we'll cover some examples of using this
library in a few simple forms, which should form a basis for reasoning
about how to use it. The same ideas should apply to other delimited
continuation libraries or interfaces (i.e. scheme, ocaml, ochacaml,
etc.)

-}

{- $example-simple

Basic answer type modification is very simple to demonstrate.
Consider that we @shift@ some computation and discard the continuation
@k@. Then the answer type of the enclosing @'reset'@ changes to the
return type of the @shift@ed block. For example:

>>> runDelim $ reset $ shift2 (\_ -> returnI "hello") !>>= \r -> returnI (r + 1)
"hello"

Here, the initial answer type of the enclosing @'reset'@ is initially
@Int@, because the return of the reset is @Int@. However, we use
@'shift2'@, which changes the answer type by /discarding/ the
delimited continuation, and simply returning a @'String'@.

On the other hand, it is also clear when the answer type is
polymorphic and cannot be changed. Instead consider this example:

>>> :t runDelim $ reset $ shift2 (\k -> returnI k) !>>= \r -> returnI (r + (1::Int))
runDelim $ reset $ shift2 (\k -> returnI k) !>>= \r -> returnI (r + (1::Int))
  :: Int -> Delim s s Int

Here, the answer type is /not/ changed by the call to @'shift2'@: the
continuation k is not discarded, but returned. This \'carries'\ the
answer type variables with the value, enforcing the input and output
answer types are the same. And so if we were to invoke @k@, then the
answer type of the @'reset'@ may still only be 'Int': that is the only
type of value we may place in the hole left by @'shift2'@.

-}

{- $example-printf

We will now write a simple, typesafe @printf@ function using
delimited continuations. The key observation is that when we write a
term like:

@
printf \"foo %s bar %d\" x y
@

we are actually filling in /holes/ in place of the @%s@ and @%d@
specifiers. Instead, we can write a type safe formatter that 'fills
in' the holes correctly by construction.

Let us first define formatting functions that will properly convert
arguments to a @'String'@. The trivial one for a @'String'@ itself is
obvious:

@
str :: String -> String
str = id
@

We may also write one for @'Int'@s:

@
int :: Int -> String
int = show
@

We observe that a call to @printf@ is similar to delimiting the
computation of the format string argument. So we define @printf@ as:

@
printf p = 'reset' p
@

Now we will define a type-safe formatter, that will \'plug\' the
value into our string properly:

@
fmt to = 'shift2' (\\k -> 'returnI' (k . to))
@

When we call @fmt@, we will /abort/ back to the enclosing @'reset'@,
returning a function. The function is @k . to@, which will convert our
value to a @'String'@ and plug it into the enclosing hole that @fmt@
left behind. Now we will define some operators for chaining these
function returns, and concatenating the string results:

@
f $$ x = f '!>>=' ($ x)
infixl 1 $$

f ^$ g = 'liftIxM2' (++) f g

run = 'runDelim'
@

Now, we can write:

@
test1 :: String
test1 = run $ printf ('returnI' \"hello world!\")
@

And more interestingly:

@
test2 :: String
test2 = run $ sprintf (fmt int) $$ 1
@

We may also format multiple arguments in a type safe manner, by
concatenating the formatters with @^$@ and passing arguments via @$$@:

@
test3, test4 :: String

test3 = run $ sprintf ('returnI' \"goodbye \" ^$ fmt str ^$ 'returnI' \"!\") $$ \"world\"

test4 = run $ sprintf (fmt str ^$ 'returnI' \" = \" ^$ fmt int) $$ \"x\" $$ 3
@

It is an error to pass a value of an incorrect type to the
corresponding formatter.

The full code is available in @examples/Printf.hs@.

-}

{- $example-btrees

Here, we will...

@
[-\# LANGUAGE RankNTypes \#-]
module Tree where

import Control.Delimited

\-- Binary trees
data Tree a
  = Leaf
  | Node (Tree a) (Tree a) a
  deriving (Show, Eq)

make_tree :: Int -> Tree Int
make_tree j = go j 1
  where go 0 _ = Leaf
        go i x = Node (go (i-1) $ 2*x) (go (i-1) $ 2*x+1) x

tree1 = make_tree 3
tree2 = make_tree 4

\-- Coroutines as monadic lists.
data Coro a
  = Done
  | Resume a (forall s. 'Delim' s s (Coro a))

walk_tree x = 'runDelim' (walk_tree' x '!>>' 'returnI' Done)

walk_tree' Leaf = 'returnI' ()
walk_tree' (Node l r x) =
  walk_tree' l '!>>'
  yield x      '!>>'
  walk_tree' r
  where
    yield n = 'shift2' (\\k -> 'returnI' $ Resume n $ k ())

walk1 :: Show a => Tree a -> IO ()
walk1 t = go (walk_tree t)
  where
    go Done         = return ()
    go (Resume x k) = print x >> go ('runDelim' k)
@

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

@
class 'Monad' (m :: * -> *) where ...
@

The @m@ type constructor abstracts over a single type variable. It is
possible to make types with multiple type variables an instance of
'Monad' of course, but their non-abstracted type variables must be
fixed. As an example, considering the instance for @'Either' e@:

@
instance 'Monad' ('Either' e) where ...
@

Note the type variable @e@ is fixed over the definition of a term of
type @'Either' e :: * -> *@. If you have something like:

@
thing :: a -> 'Either' String a
thing a = do
  ...
@

Then in the body we may say:

@
x <- Left \"oh no!\"
@

But we can never say:

@
x <- Left False
@

because @e@ is fixed to 'String'. Another example is that the
'Control.Monad.State.State' monad always has a fixed state type, and
it may never change over the course of the computation.

Indexed monads solve this problem by \'expanding\' the kind of @m@ in
the 'Monad' typeclass. The result is 'IxMonad', which is defined as:

@
 class 'IxMonad' (m :: * -> * -> * -> *) where
   'returnI' :: t -> m a a t
   ('!>>=')  :: m b g s -> (s -> m a b t) -> m a g t
@

Note the new type variables: these represent the input and output
answer types of a delimited computation. We can see that 'returnI' is
fully polymorphic in its answer type: a statement of @returnI foo@ in a
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

@
[-\# LANGUAGE RebindableSyntax \#-]
module Foo where

import Control.Indexed.Prelude
import Control.Delimited

\-- After importing the indexed prelude, we can use do notation

\-- Lifting ordinary monads
io1 :: IO ()
io1 = 'runI' $ do
  'lift' $ putStrLn \"hi!\"
  'lift' $ putStrLn \"hi!\"
  return ()

test1 :: String
test1 = 'runDelim' $ 'reset' $ do
  r <- 'shift1' (const $ return \"hello\")
  return (r + 1)
\-- This is equivalent to the OchaCaml term:
\--   reset (fun () -> 1 + shift (fun _ -> \"hello\")) ;;
@

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
