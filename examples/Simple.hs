{-# LANGUAGE RebindableSyntax #-}
module Simple where
import Prelude (Int, fromInteger, Show(..), (.), ($), (+))
import qualified Prelude
import Control.Monad.Cont.Delimited

-- Aspects of RebindableSyntax

m >>= f  = m !>>= f
return x = ret x
fail x   = Prelude.error x
f =<< m  = m !>>= f

-- Simple test of answer type modification

ans1 = run $ reset $ do
  r <- shift (\_ -> return "hello")
  return (r + 1)
-- This is equivalent to the OchaCaml term:
--   reset (fun () -> 1 + shift (fun _ -> "hello")) ;;

ans2 = run $ do
  r <- reset $ do
    x <- shift return
    return (x + 1)
  return (r 5)
--   reset (fun () -> 1 + shift (fun k -> k)) ;;

-- Append example, with monadic syntax

appnd []       = shift (\k -> return (return . k))
appnd (a:rest) = do
  r <- appnd rest
  return (a:r)

testAppnd = run $ do
  f <- reset (appnd [1,2,3])
  f [4,5,6] -- [1,2,3,4,5,6]

-- Visitors/cursors, with monadic syntax

visit [] = shift (\h -> return [])
visit (a:rest) = do
  r <- shift (\k -> do
                    b <- return (k [])
                    c <- reset ((return . k) =<< visit rest)
                    return (b:c))
  return (a:r)


testVisit = run $ reset (visit [1,2,3])
-- [[1],[1,2],[1,2,3]]
