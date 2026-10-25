module Main (main) where

import Diff ( deriva )
import Expr ( Expressao(..) )

f :: Expressao
f = Soma [Var "y"]

main :: IO ()
main = do
  print $ deriva "x" f
