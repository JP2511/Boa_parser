-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

-------------------------------------------------------------------------------

import BoaAST
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

-------------------------------------------------------------------------------

type ParseError = String -- you may replace this
type Parser a = ReadP a

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipSpaces; return a


symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()


{- Determines if the given string is the keyword defined. -}
keyword :: String -> Parser ()
keyword s = lexeme $ do 
                        s' <- many1 (satisfy isAlphaNum)
                        if s' == s 
                        then return ()
                        else fail $ "expected " ++ s


{- Parses a number while checking if the number has leading zeros. If it has 
  then it outputs an error. -}
pNum :: Parser Int
pNum = lexeme $ do 
                  possNeg <- satisfy (\x -> x == '-')
                  ds      <- many1 (satisfy isDigit)
                  num     <- return (possNeg : ds)
                  numRead <- return $ read num
                  if num == show numRead
                    then return numRead
                    else fail "Number with leading zeros"


{- Checks if a character is a valid first character of a variable name. -}
isFstIdentLetter :: Char -> Bool
isFstIdentLetter x = isAlpha x || x == '_'


{- Checks if a character is a valid non-first character of a variable name. -}
isRestIdent :: Char -> Bool
isRestIdent x = isAlphaNum x || x == '_'


{- Parses variable names. -}
pIdent :: Parser String
pIdent = lexeme $ do 
                    fstIdent <- satisfy isFstIdentLetter
                    isRestIdent <- many (satisfy isRestIdent)
                    return $ fstIdent : isRestIdent


-------------------------------------------------------------------------------


parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
