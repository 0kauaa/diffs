module Diff
  ( deriva
  ) where

import Expr ( Expressao(..) )

deriva :: Expressao -> Expressao
-- casos base de expressões multi-árias
deriva (Soma [])        = Constante 0
deriva (Produto [])     = Constante 1

-- regras de derivação
deriva (Var _)               = Constante 1
deriva (Constante _)         = Constante 0
deriva (Soma expressao)      = Soma (map deriva expressao)
deriva (Produto [f, g])      = Soma [Produto [deriva f, g], Produto [f, deriva g]]
deriva (Potencia f potencia) = Produto [potencia, Potencia f (Soma [potencia, Constante (-1)])]
deriva (E x)                 = Produto [E x, deriva x]
deriva (Seno f)              = Produto [Cosseno f, Constante (-1), deriva f]
deriva (Cosseno f)           = Produto [Seno f, Constante (-1), deriva f]
