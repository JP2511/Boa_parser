module BoaParser (ParseError, parseString) where

-------------------------------------------------------------------------------

import BoaAST
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Control.Monad (void)

-------------------------------------------------------------------------------

type ParseError = String -- you may replace this
type Parser a = ReadP a

baseKeywords = ["None", "True", "False", "for", "if", "in", "not"]

-------------------------------------------------------------------------------
-- parsing symbols

symbol :: Char -> Parser ()
symbol s = do
            satisfy (== s)
            pWhitespaceZ
            return ()


longSymbol :: String -> Parser ()
longSymbol ""      = do pWhitespaceZ ; return () 
longSymbol (l:wrd) = do
                        satisfy (== l)
                        longSymbol wrd


-------------------------------------------------------------------------------
-- parsing the whitespace

eol :: ReadP ()
eol = choice [void (char '\n'), eof]


pWhitespaces :: Parser ()
pWhitespaces = (do
            satisfy isSpace
            pWhitespaceZ)
            <|> (do
                  satisfy (=='#')
                  manyTill get eol
                  pWhitespaceZ)


pWhitespaceZ :: Parser ()
pWhitespaceZ = (do
                  satisfy isSpace
                  pWhitespaceZ)
                  <|> (do
                        satisfy (=='#')
                        manyTill get eol
                        pWhitespaceZ)
                  <|> return ()


isSpaceOrOther :: Char -> Bool
isSpaceOrOther l = isSpace l || l `elem` "()[],=<>!/%;*+-"


-------------------------------------------------------------------------------


{- Determines if the given string is the keyword defined. -}
keyword :: String -> Parser ()
keyword s = do 
                  s' <- many1 (satisfy isAlphaNum)
                  addSpaceOrNot
                  if s' == s 
                  then return ()
                  else fail $ "expected " ++ s



{- Parses a number while checking if the number has leading zeros. If it has 
  then it outputs an error. -}
pNum :: Parser Int
pNum = do 
            possNeg <- munch ('-' ==)
            fst     <- satisfy isDigit
            ds      <- many (satisfy isDigit)
            pWhitespaceZ
            let num = possNeg ++ (fst : ds)
            let result = read num :: Int
            if fst == '0' && result /= 0 && length possNeg <= 1
                  then fail "Error" 
                  else return result


-------------------------------------------------------------------------------
-- Parsing variable definitions

{- Checks if a character is a valid first character of a variable name. -}
isFstIdentLetter :: Char -> Bool
isFstIdentLetter x = isAlpha x || x == '_'


{- Checks if a character is a valid non-first character of a variable name. -}
isRestIdent :: Char -> Bool
isRestIdent x = isAlphaNum x || x == '_'


{- Parses variable names. -}
pIdent :: Parser String
pIdent = do 
            fstIdent    <- satisfy isFstIdentLetter
            isRestIdent <- many (satisfy isRestIdent)
            pWhitespaceZ
            let varName = fstIdent : isRestIdent
            if varName `elem` baseKeywords
                then fail "Read keywords instead of variable"
                else return varName


-------------------------------------------------------------------------------
-- Parses atoms. These can be names of variables or keywords.

pAtom :: Parser Stmt
pAtom = (do
            letter <- satisfy isAlpha
            pKey [letter])
            <|> (do
                  symbol '_'
                  pIden "_")


judger :: String -> Parser Stmt
judger wrd
  | wrd == "None"  = rtVal NoneVal
  | wrd == "True"  = rtVal TrueVal
  | wrd == "False" = rtVal FalseVal
  | wrd == "not"   = do
                      st <- pExpr
                      extractExp Not st "Error"
  | otherwise      = if wrd `elem` baseKeywords
                      then fail "error"
                      else pExprIdent wrd


addSpaceOrNot :: Parser ()
addSpaceOrNot = do
                  rest <- look
                  if rest == "" || isSpaceOrOther (head rest)
                    then pWhitespaceZ
                    else pWhitespaces


pKey :: String -> Parser Stmt
pKey wrd = (do
              letter <- satisfy isAlphaNum
              pKey $ wrd ++ [letter])
              <|> (do
                    symbol '_'
                    pIden $ wrd ++ "_")
              <|> (do
                    addSpaceOrNot
                    judger wrd)


pIden :: String -> Parser Stmt
pIden wrd = (do
              letter <- satisfy isAlphaNum
              pIden $ wrd ++ [letter])
              <|> (do
                    symbol '_'
                    pIden $ wrd ++ "_")
              <|> (do
                    addSpaceOrNot
                    pExprIdent wrd)


-------------------------------------------------------------------------------
-- Parsing Strings


isSQuote :: Char -> Bool
isSQuote = ('\'' ==)


isBackSlx :: Char -> Bool
isBackSlx = ('\\' ==)


isPrintAscii :: Char -> Bool
isPrintAscii x = isPrint x && 
  isAscii x && 
  not (isSQuote x) &&
  not (isBackSlx x)


{- Parses strings. -}
pString :: Parser String
pString = do
            satisfy isSQuote
            text <- pIEText
            satisfy isSQuote
            pWhitespaceZ
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
              rest    <- pIEText
              return $ content : rest)


pScpd :: Parser String
pScpd = (do
          satisfy ('n' ==)
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
                satisfy ('\n' ==)
                pIEText)
          <|> fail "Error"


-------------------------------------------------------------------------------

{- Converts a Value to a Stmt and lifts it for the parser. -}
rtVal :: Value -> Parser Stmt
rtVal x = return $ SExp (Const x)


{- Converts an Exp to a Stmt and lifts it for the parser. -}
rtExp :: Exp -> Parser Stmt
rtExp x = return $ SExp x


{- Works similar to LiftA or fmap, it applies to a function to the value (Exp) 
  inside a structure (Stmt) and returns result of the application of the 
  function on the value inside of the structure. Additionally, if there is an 
  error it passes the error to the parsing structure. -}
extractExp :: (Exp -> Exp) -> Stmt -> String -> Parser Stmt
extractExp f (SExp x) _ = rtExp (f x)
extractExp _ _        e = fail e


{- Very similar to the previous expression but the function also modifies the
  value inside of the structure. -}
extractExpC :: (Exp -> CClause) -> Stmt -> String -> Parser CClause
extractExpC f (SExp x) _ = return (f x)
extractExpC _ _        e = fail e


{- Very similar to the previous two functions but this time it is applied to 2
  value in the same that LiftA2 is. -}
extractExp2 :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> String -> Parser Stmt
extractExp2 f (SExp x) (SExp y) _ = rtExp $ f x y 
extractExp2 _ _        _        e = fail e


-------------------------------------------------------------------------------
-- top level parsing

pProgram :: Parser Program
pProgram = do
            pWhitespaceZ
            pStmts


pStmts :: Parser [Stmt]
pStmts = do
            fstStmt  <- pStmt
            restStmt <- pStmtCon
            return $ fstStmt : restStmt


pStmtCon :: Parser [Stmt]
pStmtCon = (do
              symbol ';'
              currStmt <- pStmt
              restStmt <- pStmtCon
              return $ currStmt : restStmt)
            <|> return []


pStmt :: Parser Stmt
pStmt = pExpr


-------------------------------------------------------------------------------
-- Initializes expression parsing

pExpr :: Parser Stmt
pExpr = do
          t <- pTerm
          pFstExprOpt t


pFstExprOpt :: Stmt -> Parser Stmt
pFstExprOpt st = pFstNR st
                  <|> pFstRel st
                  <|> return st


pFstNR :: Stmt -> Parser Stmt
pFstNR st0 = (do
                symbol '+'
                st1 <- pTerm
                pExprOpt (Oper Plus) st0 st1 False)
              <|> (do
                    symbol '-'
                    st1 <- pTerm
                    pExprOpt (Oper Minus) st0 st1 False)
              <|> (do
                    symbol '*'
                    st1 <- pTerm
                    pExprOpt (Oper Times) st0 st1 True)
              <|> (do
                    longSymbol "//"
                    st1 <- pTerm
                    pExprOpt (Oper Div) st0 st1 True)
              <|> (do
                    symbol '%'
                    st1 <- pTerm
                    pExprOpt (Oper Mod) st0 st1 True)


pFstRel :: Stmt -> Parser Stmt
pFstRel st1 = 
  (do
    longSymbol "=="
    st2 <- pTerm
    pNRop (Oper Eq) st1 st2 False)
  <|> (do
        longSymbol "!="
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper Eq x y) st1 st2 False)
  <|> (do
        symbol '<'
        st2 <- pTerm
        pNRop (Oper Less) st1 st2 False)
  <|> (do
        longSymbol "<="
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper Greater x y) st1 st2 False)
  <|> (do
        symbol '>'
        st2 <- pTerm
        pNRop (Oper Greater) st1 st2 False)
  <|> (do
        longSymbol ">="
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper Less x y) st1 st2 False)
  <|> (do
        keyword "in"
        st2 <- pTerm
        pNRop (Oper In) st1 st2 False)
  <|> (do
        keyword "not"
        keyword "in"
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper In x y) st1 st2 False)


-------------------------------------------------------------------------------
-- performs the parsing of chains of expressions separated by operations

pExprOpt :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> Bool -> Parser Stmt
pExprOpt op1 t0 t1 outP = pNRLP op1 t0 t1 outP  
                            <|> pNRHP op1 t0 t1 outP
                            <|> pRelOp op1 t0 t1 outP
                            <|> extractExp2 op1 t0 t1 "Error"


{- parses Non-Relational operations -}
pNRop :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> Bool -> Parser Stmt
pNRop op1 t0 t1 outP = pNRLP op1 t0 t1 outP  
                        <|> pNRHP op1 t0 t1 outP
                        <|> extractExp2 op1 t0 t1 "Error"


{- parses Non-Relational Low Priority operations -}
pNRLP :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> Bool -> Parser Stmt
pNRLP op1 t0 t1 _ = 
  (do
    symbol '+'
    st2 <- pTerm
    st1 <- extractExp2 op1 t0 t1 "Error"
    pExprOpt (Oper Plus) st1 st2 False) 
  <|> (do
        symbol '-'
        st2 <- pTerm
        st1 <- extractExp2 op1 t0 t1 "Error"
        pExprOpt (Oper Minus) st1 st2 False)


{- Defines the order of the operations in accordance with the priorities of 
  the passed operation. -}
priorityRels :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> 
                (Exp -> Exp -> Exp) -> Stmt -> Bool -> Parser Stmt
priorityRels op1 t0 t1 op2 t2 False = do 
                                        st2 <- extractExp2 op2 t1 t2 "Error"
                                        pExprOpt op1 t0 st2 False
priorityRels op1 t0 t1 op2 t2 True  = do
                                        st1 <- extractExp2 op1 t0 t1 "Error"
                                        pExprOpt op2 st1 t2 True


{- Parses Non-Relational High Priority operations -}
pNRHP :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> Bool -> Parser Stmt
pNRHP op1 t0 t1 outP =
  (do
    symbol '*'
    t2 <- pTerm
    priorityRels op1 t0 t1 (Oper Times) t2 outP)
  <|> (do
        longSymbol "//"
        t2 <- pTerm
        priorityRels op1 t0 t1 (Oper Div) t2 outP)
  <|> (do
        symbol '%'
        t2 <- pTerm
        priorityRels op1 t0 t1 (Oper Mod) t2 outP)


{- Parses relational operations. -}
pRelOp :: (Exp -> Exp -> Exp) -> Stmt -> Stmt -> Bool -> Parser Stmt
pRelOp op1 t0 t1 _ =
  (do
    st1 <- extractExp2 op1 t0 t1 "error"
    longSymbol "=="
    st2 <- pTerm
    pNRop (Oper Eq) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        longSymbol "!="
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper Eq x y) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        symbol '<'
        st2 <- pTerm
        pNRop (Oper Less) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        longSymbol "<="
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper Greater x y) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        symbol '>'
        st2 <- pTerm
        pNRop (Oper Greater) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        longSymbol ">="
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper Less x y) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        keyword "in"
        st2 <- pTerm
        pNRop (Oper In) st1 st2 False)
  <|> (do
        st1 <- extractExp2 op1 t0 t1 "error"
        keyword "not"
        keyword "in"
        st2 <- pTerm
        pNRop (\x y -> Not $ Oper In x y) st1 st2 False)


-------------------------------------------------------------------------------
-- Parsing of terms

pTerm :: Parser Stmt
pTerm = (do
          n <- pNum
          rtVal $ IntVal n)
        <|> (do
              str <- pString
              rtVal $ StringVal str)
        <|> pAtom
        <|> (do
              symbol '('
              st <- pExpr
              symbol ')'
              return st)
        <|> (do
              symbol '['
              st <- pExprList
              symbol ']'
              return st)


pExprIdent :: String -> Parser Stmt
pExprIdent vname = (do
                    satisfy (== '=')
                    pWhitespaceZ
                    st <- pExpr
                    case st of
                        SExp e -> return $ SDef vname e
                        _       -> fail "Bad expression while creating var")
                  <|> (do
                        symbol '('
                        sts <- pExprz
                        symbol ')'
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
                      symbol ','
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
              keyword "if"
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
              symbol ','
              pExprs)
            <|> return []


-------------------------------------------------------------------------------

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (pProgram <* eof) s of
  []       -> Left "Parsing Error"
  [(a, _)] -> Right a
  _        -> error "oops, my grammar is ambiguous!"