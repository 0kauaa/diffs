{- define a AST de expressões e implementa s simplificação de expressões -}

module Expr
  ( Expressao(..)
  , simplifica
  ) where

import Data.List ( sort, partition )

data Expressao
  = Constante Double
  | Var String
  | Soma [Expressao]
  | Produto [Expressao]
  | Potencia Expressao Expressao
  | E Expressao
  | Seno Expressao
  | Cosseno Expressao
    deriving (Eq, Show, Ord)

-- agrupamentos por Expressao
agrupaSomas :: [Expressao] -> [Expressao]
agrupaSomas = concatMap apenasSoma
  where
    apenasSoma (Soma t) = t
    apenasSoma        t = [t]

agrupaProdutos :: [Expressao] -> [Expressao]
agrupaProdutos = concatMap apenasProd
  where
    apenasProd (Produto t) = t
    apenasProd t = [t]

-- simplifica Expressao a uma forma canônica
simplifica :: Expressao -> Expressao
simplifica (Constante c) = Constante c
simplifica (Var x)       = Var x

simplifica (Soma f) =
  let somas    = agrupaSomas (map simplifica f)
      (cs, fs) = partition constantes somas
      somaCons = sum [c | Constante c <- cs]
      termos   = sort fs
  in case (somaCons, termos) of
      (0, [])  -> Constante 0
      (0, [x]) -> x
      (0, xs)  -> Soma xs
      (c, [])  -> Constante c
      (c, xs)  -> Soma (Constante c : xs)

simplifica (Produto f) =
  let produtos  = agrupaProdutos (map simplifica f)
      (cs, fs)  = partition constantes produtos
      prodCons  = product [c | Constante c <- cs]
      termos    = sort fs
  in if prodCons == 0 then Constante 0 else
    case (prodCons, termos) of
      (1, [])  -> Constante 1
      (1, [x]) -> x
      (1, xs)  -> Soma xs
      (c, [])  -> Constante c
      (c, xs)  -> Produto (Constante c : xs)

simplifica (Potencia b e) =
  case (simplifica b, simplifica e) of
    (_, Constante 0)             -> Constante 1
    (b', Constante 1)            -> b'
    (Constante b', Constante e') -> Constante (b' ** e')
    (b', e')                     -> Potencia b' e'

simplifica (E x) =
  case simplifica x of
    Constante 0 -> Constante 1
    Constante c -> Constante (exp c)
    x'          -> E x'

simplifica (Seno x) =
  case simplifica x of
    Constante 0 -> Constante 0
    Constante c -> Constante (sin c)
    x'          -> Seno x'

simplifica (Cosseno x) =
  case simplifica x of
    Constante 0 -> Constante 1
    Constante c -> Constante (cos c)
    x'          -> Cosseno x'

-- funções auxiliares
constantes :: Expressao -> Bool
constantes (Constante _) = True
constantes _             = False
