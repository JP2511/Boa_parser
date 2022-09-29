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

baseKeywords = ["None", "True", "False", "for", "if", "in", "not"]

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipSpaces; return a


-------------------------------------------------------------------------------

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


-- ------------------------------------------------------ --
--  TODO - figure out what to do when it reads a keyword  --
-- ------------------------------------------------------ --
{- Parses variable names. -}
pIdent :: Parser String
pIdent = lexeme $ do 
                    fstIdent    <- satisfy isFstIdentLetter
                    isRestIdent <- many (satisfy isRestIdent)
                    varName     <- return (fstIdent : isRestIdent)
                    if elem varName baseKeywords
                      then fail "Read keywords instead of variable"
                      else return varName


isPrintAscii :: Char -> Bool
isPrintAscii x = isPrint x && isAscii x && not (isSingleQuote x)


isSingleQuote :: Char -> Bool
isSingleQuote = (==) '\''

-- ------------------------------------------ --
--  TODO - figure out how to parse \' and \\  --
-- ------------------------------------------ --
{- Parses strings. -}
pString :: Parser String
pString = lexeme $ do
                    fstSQuote <- satisfy isSingleQuote
                    content   <- many (satisfy isPrintAscii)
                    lstSQuote <- satisfy isSingleQuote
                    return $ fstSQuote : content ++ [lstSQuote]


-------------------------------------------------------------------------------


parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
