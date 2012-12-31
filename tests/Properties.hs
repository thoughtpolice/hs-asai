{-# OPTIONS_GHC -fno-cse #-}
module Main
       ( main -- :: IO ()
       ) where
import Test.Hspec

main :: IO ()
main = hspec $ do
  return ()
