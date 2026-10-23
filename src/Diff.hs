module Diff
  ( deriva
  ) where

import Expr ( Expressao(..) )

deriva :: Expressao -> Expressao
deriva X               = Constante 1
deriva (Constante _)   = Constante 0
deriva (Soma f g)      = Soma (deriva f) (deriva g)
deriva (Subtracao f g) = Subtracao (deriva f) (deriva g)
deriva (Produto f g)   = Soma (Produto (deriva f) g) (Produto f (deriva g))
deriva (Potencia f c)  = Produto (Produto (Constante $ fromIntegral c) (Potencia f (c-1))) (deriva f)
deriva (Quociente f g) = Quociente (Subtracao (Produto (deriva f) g) (Produto f (deriva g))) (Potencia g 2)
deriva (E x)           = Produto (E x) (deriva x)
deriva (Seno x)        = Produto (Cosseno x) (deriva x)
deriva (Cosseno x)     = Produto (Seno x) (deriva x)
