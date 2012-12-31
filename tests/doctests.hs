module Main
       ( main -- :: IO ()
       ) where
import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src/Control/Monad/Cont/Delimited.hs"]
