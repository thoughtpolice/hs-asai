{-# LANGUAGE RankNTypes, RebindableSyntax #-}
-- Based on CC-delcont examples
module Iterator where

import Control.Indexed.Prelude
import Control.Delimited

-- Iterators. This is the same definition as
-- the monadic lazy list in 'Tree.hs'
data Iter a
  = Done
  | I a (forall s. Delim s s (Iter a))

current :: Iter a -> Maybe a
current (I a _) = Just a
current _       = Nothing

next :: Iter a -> Delim s s (Iter a)
next (I _ m) = m
next Done    = return Done

iterator loop =
  reset $ do
    loop $ \a ->
      shift2 $ \k ->
        return $ I a (k ())
    return Done

test = runDelim (iterator (forM_' [1..5]) !>>= go [])
  where
    go l Done = return l
    go l i    = do
      let (Just a) = current i
          l'       = replicate a a ++ l
      next i !>>= go l'

forM_' :: IxMonad m => [a] -> (a -> m s s b) -> m s s [b]
forM_' inp f = sequence' [] $ map f inp
  where sequence' r []     = return r
        sequence' r (x:xs) = x !>>= \v -> sequence' (v:r) xs
