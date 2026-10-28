{- implementa as regras de derivação para a AST de Expressão -}

module Diff
  ( derivada
  , deriva
  ) where

import Expr   ( Expressao(..) )
import Parser ( recebe, devolve )

-- aplica as regras direto na entrada
deriva :: String -> String -> String
deriva var expressao = (devolve . derivada var . recebe) expressao

-- regras de derivação
derivada :: String -> Expressao -> Expressao
derivada _ Nulo = Nulo

-- tratamento de multi-aridade
derivada x (Var v)
  | x == v = Constante 1
  | otherwise = Constante 0

-- casos base de expressões list-based
derivada _ (Soma [])        = Constante 0
derivada _ (Produto [])     = Constante 0

-- regras de derivadação
derivada _ (Constante _)         = Constante 0
derivada x (Soma expressao)      = Soma (map (derivada x) expressao)
derivada x (Produto expressao)   = Soma [Produto (derivadando x i expressao) | i <- [0 .. length expressao - 1]]
  where derivadando var i fs = zipWith (\idx f -> if idx == i then derivada var f else f) [0..] fs
derivada x (Potencia f potencia) = Produto [potencia, Potencia f (Soma [potencia, Constante (-1)]), derivada x f]
derivada x (E f)                 = Produto [E f, derivada x f]
derivada x (Seno f)              = Produto [Cosseno f, derivada x f]
derivada x (Cosseno f)           = Produto [Seno f, Constante (-1), derivada x f]

