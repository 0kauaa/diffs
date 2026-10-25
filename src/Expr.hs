module Expr
  ( Expressao(..)
  ) where

data Expressao
  = Constante Double
  | Var String
  | Soma [Expressao]
  | Produto [Expressao]
  | Potencia Expressao Expressao
  | E Expressao
  | Seno Expressao
  | Cosseno Expressao
    deriving (Eq, Show)

