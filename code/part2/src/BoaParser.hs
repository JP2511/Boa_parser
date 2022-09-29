-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

-------------------------------------------------------------------------------

import BoaAST
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>), liftA2)

-------------------------------------------------------------------------------

type ParseError = String -- you may replace this
type Parser a = ReadP a

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipSpaces; return a


symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()


keyword :: String -> Parser ()
keyword s = lexeme $ do 
                        s' <- many1 (satisfy isAlphaNum)
                        if s' == s 
                        then return ()
                        else fail $ "expected " ++ s


pNum :: Parser Int
pNum = lexeme $ do 
                  possNeg <- satisfy (\x -> x == '-')
                  ds      <- many1 (satisfy isDigit)
                  num     <- return (possNeg : ds)
                  numRead <- return $ read num
                  if num == show numRead
                    then return numRead
                    else fail "Number with leading zeros"


-------------------------------------------------------------------------------


parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
