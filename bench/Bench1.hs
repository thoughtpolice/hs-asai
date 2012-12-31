module Main
       ( main -- :: IO ()
       ) where
import Control.Monad.Cont.Delimited
import Criterion.Main (defaultMain, bench, nf)

main :: IO ()
main = defaultMain
       [ -- Simple answer-type modification
         bench "ans1" $ nf id $ run $ reset
         (shift (\_ -> ret "hello") !>>= \r ->
           ret (r + 1 :: Int))

         -- Capturing the continuation
       , bench "ans2" $ nf id $ run $
         (reset ( shift ret !>>= \x ->
                  ret (x + 1)
                ) !>>= \r ->
         ret (r (5 :: Int)))
       ]
