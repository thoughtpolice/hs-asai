module Main
       ( main -- :: IO ()
       ) where
import Control.Delimited
import Criterion.Main (defaultMain, bench, nf)

main :: IO ()
main = defaultMain
       [ -- Simple answer-type modification
         bench "ans1" $ nf id $ runDelim $ reset
         (shift1 (\_ -> returnI "hello") !>>= \r ->
           returnI (r + 1 :: Int))

         -- Capturing the continuation
       , bench "ans2" $ nf id $ runDelim $
         (reset ( shift1 returnI !>>= \x ->
                   returnI (x + 1)
                 ) !>>= \r ->
         returnI (r (5 :: Int)))
       ]
