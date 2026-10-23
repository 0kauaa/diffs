{-# LANGUAGE InstanceSigs #-}

module Expr
  ( Expressao(..)
  ) where

data Expressao =
  X | Constante Double | Soma Expressao Expressao | Subtracao Expressao Expressao
  | Produto Expressao Expressao | Potencia Expressao Integer
  | Quociente Expressao Expressao | E Expressao
  | Seno Expressao | Cosseno Expressao
    deriving (Eq, Show)
