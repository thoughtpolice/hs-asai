{-# LANGUAGE RebindableSyntax #-}
module Printf where

import Control.Indexed.Prelude
import Control.Delimited

int x = show x
str x = x

f $$ x = f !>>= ($ x)
infixl 1 $$

e1 ^$ e2 = liftIxM2 (++) e1 e2

fmt to = shift2 (\k -> return (k . to))
sprintf p = reset p

stest1 = runDelim $
  sprintf (ret "Hello world")

stest2 = runDelim $
  sprintf (fmt int) $$ 3

stest3 = runDelim $
  sprintf (ret "goodbye " ^$ fmt str ^$ ret "!") $$ "world"

stest4 = runDelim $
  sprintf (fmt str ^$ ret " = " ^$ fmt int) $$ "x" $$ 3
