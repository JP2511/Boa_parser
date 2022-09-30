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


opAndFunc :: [(String, Op)]
opAndFunc = [("+", Plus),
              ("-", Minus),
              ("*", Times),
              ("//", Div),
              ("%", Mod),
              ("==", Eq),
              ("!=", \x y -> Not (Eq x y)),
              ("<", Less),
              ("<=", \x y -> Not (Greater x y)),
              (">", Greater),
              (">=", \x y -> Not (Less x y)),
              ("in", In)]


performOp :: String -> Stmt -> Stmt -> Either ParseError Stmt
performOp op (SExp x) (SExp y) = case lookup op opAndFunc of
  Just operation -> Right $ SExp (Oper operation x y)
  Nothing        -> Left "Operation not defined"
performOp op _ _ = Left "Statement defined inside of an expression"


-------------------------------------------------------------------------------

rtVal :: Value -> Parser Stmt
rtVal x = return $ SExp (Const x)


rtExp :: Exp -> Parser Stmt
rtExp x = return $ SExp x


extractExp :: (Exp -> Exp) -> Stmt -> String -> Parser Stmt
extractExp f (SExp x) _ = rtExp (f x)
extractExp _ _        e = fail e


extractExp2 :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> String -> Parser Stmt
extractExp2 f (SExp x) (SExp y) _ = return $ SExp (f x y)
extractExp2 _ _        _        e = fail e


extractExp2e :: (Exp -> Exp -> Either String Exp) -> Stmt -> Stmt -> String 
                -> Parser Stmt
extractExp2e f (SExp x) (SExp y) _ = case f x y of
  Right val -> rtExp val
  Left  e   -> fail e
extractExp2e _ _        _        e = fail e


addToList :: Exp -> Exp -> Either String Exp
addToList x (List xs) = Right $ List x:xs
addToList _ _         = Left "List construction: recursive result is not list"


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
            <|> return []


pStmt :: Parser Stmt
pStmt = pExpr


pExpr :: Parser Stmt
pExpr = (do
          t <- pTerm
          pExprOpt t)


pExprOpt :: Stmt -> Parser Stmt
pExprOpt st1 = (do
                operator <- pOper
                st2       <- pExpr
                operRes  <- return $ performOp operator st1 st2
                case operRes of
                  Left e           -> fail e
                  Right parsedStmt -> pExprOpt parsedStmt)
              <|> return st1


pTerm :: Parser Stmt
pTerm = (do
          n <- pNum
          rtVal $ IntVal n)
        <|> (do
              str <- pString
              rtVal $ StringVal str)
        <|> (do
              keyword "None"
              rtVal NoneVal)
        <|> (do
              keyword "True"
              rtVal TrueVal)
        <|> (do
              keyword "False"
              rtVal FalseVal)
        <|> (do
              symbol "("
              st <- pExpr
              symbol ")"
              return st)
        <|> (do
              name <- pIdent
              pExprIdent name)
        <|> (do
              symbol '['
              st <- pExprList
              symbol ']'
              return st)


pExprIdent :: Parser Stmt
pExprIdent vname = (do
                    symbol '='
                    e <- pExpr
                    return $ SDef vname e)
                  <|> (do
                        symbol "("
                        sts <- pExprz
                        symbol ")"
                        extractExp (Call vname) sts "Bad expression in Call")
                  <|> rtExp $ Var vname


pExprList :: Parser Stmt
pExprList = (do
              st1 <- pExpr
              pExprNList st1)
            <|> rtExp $ List []


pExprNList :: Stmt -> Parser Stmt
pExprNList st1 = (do
                  for     <- pForClause
                  restCls <- pClausez
                  extractExp (\x -> Compr x (for : restCls)) st1 $
                    "Bad expression in List comprehension")
                <|> (do
                      symbol "," 
                      sts <- pExprs
                      extractExp2e addToList st1 sts)
                <|> extractExp (\x -> List [x]) st1 "Problem initializing list"


pForClause :: Parser CClause
pForClause = (do
                keyword "for"
                vname <- pIdent
                keyword "in"
                st <- pExpr
                extractExp (CCFor vname) st "Bad expression in for loop")


pIfClause :: Parser CClause
pIfClause = (do
              keyword "If"
              st <- pExpr
              extractExp (CCIf exp) st "Bad expression in If conditional")


pClausez :: Parser [CClause]
pClausez = (do
              for     <- pForClause
              restCls <- pClausez
              return $ for : restCls)
            <|> (do
                  ifCond  <- pIfClause
                  restCls <- pClausez
                  return $ ifCond : restCls)
            <|> return []


-------------------------------------------------------------------------------


parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
