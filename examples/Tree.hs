{-# LANGUAGE RankNTypes #-}
-- Walking trees with coroutines.
-- Based on:
--    <http://okmij.org/ftp/continuations/ContExample.hs>
module Tree where

import Control.Delimited

-- Binary trees
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

-- Coroutines as lazy monadic lists.
data Coro a
  = Done
  | Resume a (forall s. Delim s s (Coro a))

walk_tree x = runDelim (walk_tree' x !>> returnI Done)

walk_tree' Leaf = returnI ()
walk_tree' (Node l r x) =
  walk_tree' l !>>
  yield x      !>>
  walk_tree' r
  where
    yield n = shift2 (\k -> returnI $ Resume n $ k ())

walk1 :: Show a => Tree a -> IO ()
walk1 t = go (walk_tree t)
  where
    go Done         = return ()
    go (Resume x k) = print x >> go (runDelim k)

{-
*Tree> walk1 tree1
4
2
5
1
6
3
7
*Tree>
-}
