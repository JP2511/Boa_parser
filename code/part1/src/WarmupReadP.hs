module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E    ::= T Eopt | "-" T .
--   Eopt ::= "+" T Eopt | "-" T Eopt | e .
--   T    ::= num | "(" E ")" .

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec


type Parser a = ReadP a   -- may use synomym for easier portability to Parsec


type ParseError = String  -- not particularly informative with ReadP


data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)


token :: ReadP a -> Parser a
token p = skipSpaces >> p


isDigit :: Char -> Bool
isDigit x = elem x $ enumFromTo '0' '9'


lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipSpaces; return a


symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()


pNum :: Parser Int
pNum = lexeme $ do 
                  ds <- many1 (satisfy isDigit)
                  return $ read ds  


pExp :: Parser Exp
pExp = (do 
          t <- pTerm
          pEopt t)
    <|> (do 
          symbol "-"
          t <- pTerm
          return (Negate t))


pEopt :: Exp -> Parser Exp
pEopt t1 = (do 
              symbol "+" 
              t2 <- pTerm
              pEopt $ Add t1 t2)
        <|> (do 
                symbol "-" 
                t2 <- pTerm
                pEopt $ Add t1 (Negate t2))
        <|> return t1


pTerm :: Parser Exp
pTerm = (do 
          n <- pNum
          return $ Num n)
      <|> (do symbol "(" 
              e <- pExp 
              symbol ")"
              return e)


parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (pExp <* token eof) s of
  []       -> Left "Parsing Error"
  [(a, _)] -> Right a
  _        -> error "oops, my grammar is ambiguous!"