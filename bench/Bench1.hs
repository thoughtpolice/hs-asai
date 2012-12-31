module Main
       ( main -- :: IO ()
       ) where
import Control.Monad.Cont.Delimited ()
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain []
