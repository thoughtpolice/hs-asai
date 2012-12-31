{-# OPTIONS_GHC -fno-cse #-}
module Main
       ( main -- :: IO ()
       ) where
import Control.Delimited ()
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  return ()
