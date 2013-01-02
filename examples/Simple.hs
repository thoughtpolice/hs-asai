{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
module Simple where

import Control.Indexed.Prelude
import Control.Delimited

-- Test of compatibility with Prelude

io1 :: IO ()
io1 = runI $ do
  lift $ putStrLn "hi!"
  lift $ putStrLn "bye!"
  return ()

-- Simple test of answer type modification

ans1 :: String
ans1 = runDelim $ reset $ do
  r <- shift1 (\_ -> return "hello")
  return (r + 1)
-- This is equivalent to the OchaCaml term:
--   reset (fun () -> 1 + shift (fun _ -> "hello")) ;;

ans1' :: String
ans1' = runDelim $ reset $ do
  r <- shift2 (\_ -> return "hello")
  return (r + 1)

ans2 :: Int
ans2 = runDelim $ do
  r <- reset $ do
    x <- shift1 (\k -> return k)
    return (x + 1)
  return (r 5)
-- In OchaCaml:
--   let f = reset (fun () -> 1 + shift (fun k -> k)) ;;
--   f 5 ;;

ans2' :: Int
ans2' = runDelim $ do
  r <- reset $ do
    x <- shift2 (\k -> return k)
    return (x + (1 :: Int))
  -- The type of 'r' is:
  --
  --   r :: forall t. Int -> Delim t t Int
  --
  -- i.e., it is polymorhic in the answer type
  r 5

-- Append example, with monadic syntax

appnd :: IxMonad m => [a] -> Delim b ([a] -> m a' a' b) [a]
appnd []       = shift1 (\k -> return (return . k))
appnd (a:rest) = do
  r <- appnd rest
  return (a:r)

testAppnd :: [Int]
testAppnd = runDelim $ do
  f <- reset (appnd [1,2,3])
  f [4,5,6] -- [1,2,3,4,5,6]

-- Visitors/cursors, with monadic syntax

visit :: [a] -> Delim b [b] [a]
visit []       = shift1 (\_ -> return [])
visit (a:rest) = do
  r <- shift1 (\k -> do
                    b <- return (k [])
                    c <- reset ((return . k) =<< visit rest)
                    return (b:c))
  return (a:r)


testVisit :: [[Int]]
testVisit = runDelim $ reset (visit [1,2,3])
-- [[1],[1,2],[1,2,3]]
