module Main
       ( main -- :: IO ()
       ) where
import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "-isrc"
               , "src/Data/Indexed/Functor.hs"
               , "src/Control/Indexed/Applicative.hs"
               , "src/Control/Indexed/Monad.hs"
               , "src/Control/Indexed/Prelude.hs"
               , "src/Control/Delimited.hs"
               , "src/Control/Delimited/Tutorial.hs"
               ]
