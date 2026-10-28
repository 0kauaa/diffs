{- executável principal usado para testes -}

module Main (main) where

import Diff   ( deriva, derivada )
import Expr   ( Expressao(..), simplifica )
import Parser ( recebe, devolve )

f :: String
f = "(x  - 2 * (2 + x))"

main :: IO ()
main = do
  print $ (devolve . derivada "x" . recebe) f
  print $ deriva "x" f
