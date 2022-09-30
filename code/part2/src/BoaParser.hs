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
                  num     <- return $ possNeg : ds
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
                    satisfy isSingleQuote
                    content <- many (satisfy isPrintAscii)
                    satisfy isSingleQuote
                    return $ content


pOper :: String -> Parser String
pOper s = lexeme $ do 
                    operator <- string s
                    return operator


performOp :: String -> Stmt -> Stmt -> Either ParseError Stmt
performOp op (SExp x) (SExp y) = case op of
  "+"  -> Right $ SExp (Plus x y)
  "-"  -> Right $ SExp (Minus x y)
  "*"  -> Right $ SExp (Times x y)
  "//" -> Right $ SExp (Div x y)
  "%"  -> Right $ SExp (Mod x y)
  "==" -> Right $ SExp (Eq x y)
  "!=" -> Right $ SExp (Not (Eq x y))
  "<"  -> Right $ SExp (Less x y)
  "<=" -> Right $ SExp (Not (Greater x y))
  ">"  -> Right $ SExp (Greater x y)
  ">=" -> Right $ SExp (Not (Less x y))
  "in" -> Right $ SExp (In x y)
performOp op _ _ = Left "Statement defined inside of an expression"


-------------------------------------------------------------------------------

pProgram :: Parser Program
pProgram = pStmts


pStmts :: Parser [Stmt]
pStmts = (do
            fstStmt  <- pStmt
            restStmt <- pStmtCon
            return $ fstStmt : restStmt)


pStmtCon :: Parser [Stmt]
pStmtCon = (do
              symbol ';'
              currStmt <- pStmt
              restStmt <- pStmtCon
              return $ currStmt : restStmt)
            <|> (do 
                  return [])


pStmt :: Parser Stmt
pStmt = pExpr


pExpr :: Parser Stmt
pExpr = (do
          t <- pTerm
          pExprOpt t)


pExprOpt :: Stmt -> Parser Stmt
pExprOpt e1 = (do
                operator <- pOper
                e2       <- pExprOpt
                operRes  <- return $ performOp operator e1 e2
                case operRes of
                  Left e -> fail e
                  Right parsedStmt -> pExprOpt parsedStmt)
              <|> (do
                    return e1)


rtVal :: Value -> Parser Stmt
rtStmt x = return $ SExp (Const x)


pTerm :: Parser Stmt
pTerm = (do
          n -> pNum
          rtStmt $ IntVal n
        <|> (do
              str -> pString
              rtStmt $ StringVal str)
        <|> (do
              keyword "None"
              rtStmt NoneVal)
        <|> (do
              keyword "True"
              rtStmt TrueVal)
        <|> (do
              keyword "False"
              rtStmt FalseVal)
        <|> (do
              symbol "("
              e <- pExpr
              symbol ")"
              return e)
        <|> (do
              name <- pIdent
              ExprIdent name)
        <|> (do
              symbol '['
              e <- ExprList
              symbol ']'
              return e)


-------------------------------------------------------------------------------


parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
