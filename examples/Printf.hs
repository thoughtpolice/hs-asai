{-# LANGUAGE RebindableSyntax #-}
module Printf where

import Control.Indexed.Prelude
import Control.Delimited

int :: Int -> String
int x = show x
str :: String -> String
str x = x

f $$ x = f !>>= ($ x)
infixl 1 $$

e1 ^$ e2 = liftIxM2 (++) e1 e2

fmt to = shift2 (\k -> return (k . to))
ret = returnI
sprintf p = reset p
run = runDelim

stest1 = run $ sprintf (ret "Hello world")

stest2 = run $ sprintf (fmt int) $$ 1

stest3 = run $ sprintf (ret "goodbye " ^$ fmt str ^$ ret "!") $$ "world"

stest4 = run $ sprintf (fmt str ^$ ret " = " ^$ fmt int) $$ "x" $$ 3
