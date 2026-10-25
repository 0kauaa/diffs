module Diff
  ( deriva
  ) where

import Expr ( Expressao(..) )

deriva :: String -> Expressao -> Expressao
-- tratamento de multi-aridade
deriva x (Var v)
  | x == v = Constante 1
  | otherwise = Constante 0

-- casos base de expressões list-based
deriva _ (Soma [])        = Constante 0
deriva _ (Produto [])     = Constante 0

-- regras de derivação
deriva _ (Constante _)         = Constante 0
deriva x (Soma expressao)      = Soma (map (deriva x) expressao)
deriva x (Produto expressao)   = Soma [Produto (derivando x i expressao) | i <- [0 .. length expressao - 1]]
  where derivando var i fs = zipWith (\idx f -> if idx == i then deriva var f else f) [0..] fs
deriva x (Potencia f potencia) = Produto [potencia, Potencia f (Soma [potencia, Constante (-1)]), deriva x f]
deriva x (E f)                 = Produto [E f, deriva x f]
deriva x (Seno f)              = Produto [Cosseno f, deriva x f]
deriva x (Cosseno f)           = Produto [Seno f, Constante (-1), deriva x f]
