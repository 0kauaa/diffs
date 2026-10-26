{- executável principal usado para testes -}

module Main (main) where

import Diff ( deriva )
import Expr ( Expressao(..), simplifica )

f :: Expressao
f = simplifica (E (Produto [Var "x", Constante (-1)]))

main :: IO ()
main = do
  print $ simplifica $ deriva "x" f
