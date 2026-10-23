module Main (main) where

import Diff ( deriva )
import Expr ( Expressao(..) )

f :: Expressao
f = E X

main :: IO ()
main = do
  print $ deriva f
