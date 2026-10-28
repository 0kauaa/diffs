{- implementa o parsing de String e garante o isomorfismo com Expressao -}

module Parser
  ( recebe
  , devolve
  ) where

import Expr ( Expressao(..), simplifica )

import Text.ParserCombinators.ReadP
    ( ReadP
    , between
    , eof
    , munch1
    , readP_to_S
    , satisfy
    , skipMany
    , string )
import Data.Char (isDigit, isAlpha, isSpace )
import Data.List ( intercalate )
import Control.Applicative ( (<|>) )

-- parsing de cada expressão
pAtomo :: ReadP Expressao
pAtomo = pParentese
     <|> pConstante
     <|> pVar
     <|> pE
     <|> pSeno
     <|> pCosseno
  where
    pParentese = between (simbolo "(") (simbolo ")") pSoma

pConstante :: ReadP Expressao
pConstante = Constante . read <$> pNumero

pVar :: ReadP Expressao
pVar = Var <$> parsing (munch1 isAlpha)

pSoma :: ReadP Expressao
pSoma = pProduto >>= chain
  where
    chain esq =
      (do simbolo "+"; dir <- pProduto; chain (Soma [esq, dir])) <|>
      (do simbolo "-"; dir <- pProduto; chain (Soma [esq, Produto [Constante (-1), dir]])) <|>
      return esq

pProduto :: ReadP Expressao
pProduto = pPotencia >>= chain
  where
    chain esq =
      (do simbolo "*"; dir <- pPotencia; chain (Produto [esq, dir])) <|>
      (do simbolo "/"; dir <- pPotencia; chain (Produto [esq, Potencia dir (Constante (-1))])) <|>
      return esq

pPotencia :: ReadP Expressao
pPotencia = do
  base <- pAtomo
  (simbolo "^" >> Potencia base <$> pPotencia) <|> return base

pE :: ReadP Expressao
pE = simbolo "e" *> simbolo "^" *> (E <$> pAtomo)

pSeno :: ReadP Expressao
pSeno = Seno <$> (simbolo "seno" *> between (simbolo "(") (simbolo ")") pSoma)

pCosseno :: ReadP Expressao
pCosseno = Cosseno <$> (simbolo "cosseno" *> between (simbolo "(") (simbolo ")") pSoma)

-- isomorfismo entre Expressao e String
recebe :: String -> Expressao
recebe expressao
  | null (filter (not . isSpace) expressao) = Nulo
  | otherwise = case readP_to_S (ignoraEspaços *> pSoma <* eof) expressao of
      [(ast, "")] -> simplifica ast
      _           -> Nulo

devolve :: Expressao -> String
devolve Nulo      = "erro ao receber expressao."
devolve expressao = converte $ simplifica expressao

converte :: Expressao -> String
converte Nulo = ""
converte (Constante c)            = if c == fromInteger (floor c) then show (floor c) else show c
converte (Var v)                  = v
converte (Soma ts)                = intercalate " + " (map converte ts)
converte (Produto fs)             = intercalate " * " (map formataFator fs)
  where formataFator (Soma ts) = "(" ++ converte (Soma ts) ++ ")"
        formataFator f         = converte f
converte (Potencia b e)           = formataBase b ++ "^" ++ formataExp e
  where formataBase (Soma ts)    = "(" ++ converte (Soma ts) ++ ")"
        formataBase (Produto fs) = "(" ++ converte (Produto fs) ++ ")"
        formataBase x            = converte x
        formataExp x             = "(" ++ converte x ++ ")"
converte (E f)                    = "e^" ++ formataExp f
  where formataExp x = "(" ++ converte x ++ ")"
converte (Seno f)                 = "(" ++ "seno(" ++ converte f ++ ")"
converte (Cosseno f)              = "(" ++ "cosseno(" ++ converte f ++ ")"

-- funções auxiliares de parsing
ignoraEspaços :: ReadP ()
ignoraEspaços = skipMany (satisfy isSpace)

parsing :: ReadP a -> ReadP a
parsing p = p <* ignoraEspaços

simbolo :: String -> ReadP String
simbolo s = parsing (string s)

pNumero:: ReadP String
pNumero = parsing $ do
  inteiro <- munch1 isDigit
  fracao  <- (do
               _       <- string "." <|> string ","
               digitos <- munch1 isDigit
               return ("." ++ digitos)
             ) <|> return ""
  return (inteiro ++ fracao)
