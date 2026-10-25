module Main (main) where

import Diff ( deriva )
import Expr ( Expressao(..) )

f :: Expressao
f = E (Produto [ Var "x" , Constante (-1)])

main :: IO ()
main = do
  print $ deriva f
