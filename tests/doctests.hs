module Main
       ( main -- :: IO ()
       ) where
import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "-isrc"
               , "src/Control/Delimited.hs"
               , "src/Control/Delimited/Tutorial.hs"
               ]
