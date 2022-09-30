module BoaParser where
-- Skeleton file for Boa Parser.

-- module BoaParser (ParseError, parseString, main) where

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


-- textParser :: String -> String
-- textParser wholeTxt = f wholeTxt "" where
--   f "" wholeTxt = head wholeTxt

-------------------------------------------------------------------------------

token :: Parser a -> Parser a
token p = skipSpaces >> p


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
                  possNeg <- munch ((==) '-')
                  fst     <- satisfy isDigit
                  ds      <- many (satisfy isDigit)
                  let num = possNeg ++ (fst : ds)
                  let result = read num :: Int
                  if fst == '0' && result /= 0 && length possNeg <= 1
                    then fail "Error" 
                    else return result


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
                    let varName = fstIdent : isRestIdent
                    if varName `elem` baseKeywords
                      then fail "Read keywords instead of variable"
                      else return varName


isSQuote :: Char -> Bool
isSQuote = (==) '\''


isBackSlx :: Char -> Bool
isBackSlx = (==) '\\'


isPrintAscii :: Char -> Bool
isPrintAscii x = isPrint x && 
  isAscii x && 
  not (isSQuote x) &&
  not (isBackSlx x)


-- ------------------------------------------ --
--  TODO - figure out how to parse \' and \\  --
-- ------------------------------------------ --
{- Parses strings. -}
pString :: Parser String
pString = lexeme $ 
  do
    satisfy isSQuote
    text <- pIEText
    satisfy isSQuote
    return text


pIEText :: Parser String
pIEText = pText
          <|> return ""


pText :: Parser String
pText = (do
          satisfy isBackSlx
          pScpd)
        <|> (do
              content <- satisfy isPrintAscii
              rest <- pIEText
              return $ content : rest)


pScpd :: Parser String
pScpd = (do
          satisfy ('n'==)
          rest <- pIEText
          return $ '\n' : rest)
          <|> (do
                satisfy isBackSlx
                rest <- pIEText
                return $ '\\' : rest)
          <|> (do
                satisfy isSQuote
                rest <- pIEText
                return $ '\'' : rest)
          <|> (do
                satisfy ((==) '\n')
                pIEText)
          <|> fail "Error"


isMathOpF :: Char -> Bool
isMathOpF x = x `elem` "+-*%=<>!"


isMathOpC :: Char -> Char -> Bool
isMathOpC p c 
  | elem p "!=<>" && c == '=' = True
  | p == '/'      && c == '/' = True
  | otherwise = False


-- pOper :: Parser String
-- pOper = lexeme $ do 
--                   fst <- satisfy isMathOpF
--                   rest <- satisfy (isMathOpC fst)
--                   return $ fst : [] --[rest]


-- listOp :: [(String, Exp -> Exp -> Exp)]
-- listOp = [("+",  Oper Plus),
--           ("-",  Oper Minus),
--           ("*",  Oper Times),
--           ("//", Oper Div),
--           ("%",  Oper Mod),
--           ("==", Oper Eq),
--           ("!=", \x y -> Not $ Oper Eq x y),
--           ("<",  Oper Less),
--           ("<=", \x y -> Not $ Oper Greater x y),
--           (">",  Oper Greater),
--           (">=", \x y -> Not $ Oper Less x y)]

-------------------------------------------------------------------------------

rtVal :: Value -> Parser Stmt
rtVal x = return $ SExp (Const x)


rtExp :: Exp -> Parser Stmt
rtExp x = return $ SExp x


-- performOp :: String -> Stmt -> Stmt -> Parser Stmt
-- performOp op (SExp x) (SExp y) = case lookup op listOp of
--   Just val -> rtExp $ val x y
--   Nothing  -> fail "Operator not defined"
-- performOp _  _        _        = fail "Bad expression used in operation"


extractExp :: (Exp -> Exp) -> Stmt -> String -> Parser Stmt
extractExp f (SExp x) _ = rtExp (f x)
extractExp _ _        e = fail e


extractExpC :: (Exp -> CClause) -> Stmt -> String -> Parser CClause
extractExpC f (SExp x) _ = return (f x)
extractExpC _ _        e = fail e


extractExp2 :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> String -> Parser Stmt
extractExp2 f (SExp x) (SExp y) _ = rtExp $ f x y 
extractExp2 _ _        _        e = fail e


-- extractExp2e :: (Exp -> Exp -> Either String Exp) -> Stmt -> Stmt -> String 
--                 -> Parser Stmt
-- extractExp2e f (SExp x) (SExp y) _ = case f x y of
--   Right val -> rtExp val
--   Left  e   -> fail e
-- extractExp2e _ _        _        e = fail e


-- addToList :: Exp -> Exp -> Either String Exp
-- addToList x (List xs) = Right $ List (x:xs)
-- addToList _ _         = Left "List construction: recursive result is not list"


-------------------------------------------------------------------------------

pProgram :: Parser Program
pProgram = pStmts


pStmts :: Parser [Stmt]
pStmts = do
            fstStmt  <- pStmt
            restStmt <- pStmtCon
            return $ fstStmt : restStmt


pStmtCon :: Parser [Stmt]
pStmtCon = (do
              symbol ";"
              currStmt <- pStmt
              restStmt <- pStmtCon
              return $ currStmt : restStmt)
            <|> return []


pStmt :: Parser Stmt
pStmt = pExpr


pExpr :: Parser Stmt
pExpr = do
          t <- pTerm
          pExprOpt t


pExprOpt :: Stmt -> Parser Stmt
pExprOpt st1 = (do 
                  symbol "+"
                  st2 <- pExpr
                  extractExp2 
                    (Oper Plus) st1 st2 "Bad expression in 'Plus' operation")
                <|> (do
                      symbol "-"
                      st2 <- pExpr
                      extractExp2 (Oper Minus) st1 st2 
                        "Bad expression in 'Minus' operation")
                <|> (do
                      symbol "*"
                      st2 <- pExpr
                      extractExp2 (Oper Times) st1 st2
                        "Bad expression in 'Times' operation")
                <|> (do
                      symbol "//"
                      st2 <- pExpr
                      extractExp2 (Oper Div) st1 st2
                        "Bad expression in 'Div' operation")
                <|> (do
                      symbol "=="
                      st2 <- pExpr
                      extractExp2 (Oper Eq) st1 st2
                        "Bad expression in 'Eq' operation")
                <|> (do
                      symbol "!="
                      st2 <- pExpr
                      extractExp2 (\x y -> Not $ Oper Eq x y) st1 st2
                        "Bad expression in 'Not Eq' operation")
                <|> (do
                      symbol "<"
                      st2 <- pExpr
                      extractExp2 
                        (Oper Less) st1 st2 "Bad expression in 'Eq' operation")
                <|> (do
                      symbol "<="
                      st2 <- pExpr
                      extractExp2 (\x y -> Not $ Oper Greater x y) st1 st2
                        "Bad expression in 'Eq' operation")
                <|> (do
                      symbol ">"
                      st2 <- pExpr
                      extractExp2 (Oper Greater) st1 st2 
                        "Bad expression in 'Eq' operation")
                <|> (do
                      symbol ">="
                      st2 <- pExpr
                      extractExp2 (\x y -> Not $ Oper Less x y) st1 st2
                        "Bad expression in 'Eq' operation")
                <|> (do
                      keyword "in"
                      st2 <- pExpr
                      extractExp2 (Oper In) st1 st2
                        "Bad expression in 'In' operation")
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
              keyword "not"
              st <- pExpr
              extractExp Not st "Error")
        <|> (do
              symbol "("
              st <- pExpr
              symbol ")"
              return st)
        <|> (do
              name <- pIdent
              pExprIdent name)
        <|> (do
              symbol "["
              st <- pExprList
              symbol "]"
              return st)


pExprIdent :: String -> Parser Stmt
pExprIdent vname = (do
                    symbol "="
                    st <- pExpr
                    case st of
                        SExp e -> return $ SDef vname e
                        _       -> fail "Bad expression while creating var")
                  <|> (do
                        symbol "("
                        sts <- pExprz
                        symbol ")"
                        rtExp (Call vname sts))
                  <|> rtExp (Var vname)


pExprList :: Parser Stmt
pExprList = (do
              st1 <- pExpr
              pExprNList st1)
            <|> rtExp (List [])


pExprNList :: Stmt -> Parser Stmt
pExprNList st1 = (do
                  for     <- pForClause
                  restCls <- pClausez
                  extractExp (\x -> Compr x (for : restCls)) st1
                    "Bad expression in List comprehension")
                <|> (do
                      symbol ","
                      es <- pExprs
                      case st1 of
                        SExp e1 -> rtExp $ List (e1 : es)
                        _       -> fail "Bad expression while building list")
                <|> extractExp (\x -> List [x]) st1 "Problem initializing list"


pForClause :: Parser CClause
pForClause = do
                keyword "for"
                vname <- pIdent
                keyword "in"
                st <- pExpr
                extractExpC (CCFor vname) st "Bad expression in for loop"


pIfClause :: Parser CClause
pIfClause = do
              keyword "If"
              st <- pExpr
              extractExpC CCIf st "Bad expression in If conditional"


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


pExprz :: Parser [Exp]
pExprz = pExprs
          <|> return []


pExprs :: Parser [Exp]
pExprs = do
            st   <- pExpr
            rest <- pExprCon
            case st of
              SExp x -> return $ x : rest
              _      -> fail "Bad expression building list"


pExprCon :: Parser [Exp]
pExprCon = (do
              symbol ","
              pExprs)
            <|> return []


-------------------------------------------------------------------------------


parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (pProgram <* token eof) s of
  []       -> Left "Parsing Error"
  [(a, _)] -> Right a
  _        -> error "oops, my grammar is ambiguous!"


main = parseString "-3"