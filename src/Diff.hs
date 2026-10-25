module Diff
  ( deriva
  ) where

import Expr ( Expressao(..) )

deriva :: Expressao -> Expressao
-- casos base de expressões multi-árias
deriva (Soma [])        = Constante 0
deriva (Produto [])     = Constante 0

-- regras de derivação
deriva (Var _)               = Constante 1
deriva (Constante _)         = Constante 0
deriva (Soma expressao)      = Soma (map deriva expressao)
deriva (Produto expressao)   = Soma [Produto (deriva' i expressao) | i <- [0 .. length expressao - 1]]
  where
    deriva' i fs = zipWith (\idx f -> if idx == i then deriva f else f) [0..] fs
deriva (Potencia f potencia) = Produto [potencia, Potencia f (Soma [potencia, Constante (-1)]), deriva f]
deriva (E x)                 = Produto [E x, deriva x]
deriva (Seno f)              = Produto [Cosseno f, deriva f]
deriva (Cosseno f)           = Produto [Seno f, Constante (-1), deriva f]
