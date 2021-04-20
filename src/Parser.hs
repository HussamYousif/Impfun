module Parser (program, parse) where

import Syntax
import System.IO
import Control.Monad

import Text.Parsec

import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = words "true false var if while fun ref return try catch reset shift spawn detach join throw for type"
           , Token.reservedOpNames = words "+ - * / % == != < > <= >= && || ! = ++ || "
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
braces     = Token.braces     lexer -- parses surrounding braces
brackets   = Token.brackets   lexer -- Parses surrounding brackets.
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer -- parses a natural number
semi       = Token.semi       lexer -- parses a semicolon
symbol     = Token.symbol     lexer -- parses one of the ops
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses a comma separated list
stringlit = Token.stringLiteral lexer
top = parse program

program = do
  whiteSpace
  ss <- many (statement) -- <|> typeDec)
  eof
  return $ SBlock $ foldr SSeq SSkip ss

{-typeDec = do
  reserved "type"
  variables <- braces (many varDeclStmt) -}

statement =
  empty <|>
  ifStmt <|>
  whileStmt <|>
  block <|>
  returnStmt <|>
  tryStmt <|>
  throwStmt <|>
  varDeclStmt <|>
  assignStmt <|>
  for <|>
  exprStmt

empty = semi >> return SSkip

ifStmt = do
  reserved "if"
  e <- parens expr
  s1 <- statement
  reserved "else"
  s2 <- statement
  return $ SIf e s1 s2
whileStmt = do
  reserved "while"
  e <- parens expr
  s <- statement
  return $ SWhile e s
block = do
  ss <- braces (many statement)
  return $ SBlock $ foldr SSeq SSkip ss
assignStmt = do
  i <- try ( identifier >>= \j -> reservedOp "=" >> return j )
  e <- expr
  semi
  return $ SAssign i e
varDeclStmt = do
  reserved "var"
  i <- identifier
  reservedOp "="
  e <- expr
  semi
  return $ SVarDecl i e
returnStmt = do
  reserved "return"
  e <- expr
  semi
  return $ SReturn e
tryStmt = do
  reserved "try"
  s1 <- statement
  reserved "catch"
  i <- parens identifier
  s2 <- statement
  return $ STry s1 i s2
throwStmt = do
  reserved "throw"
  e <- expr
  semi
  return $ SThrow e
exprStmt = do
  e <- expr
  semi
  return $ SExpr e
for = do
  reserved "for"
  char '('
  initStmt <- varDeclStmt
  conditionExpr <- expr
  semi
  -- Code duplication to get rid of the semi.
  i <- try ( identifier >>= \j -> reservedOp "=" >> return j )
  e <- expr
  char ')'
  body <- statement
  return (SFor initStmt conditionExpr (SAssign i e) body)

expr = conjunction `chainl1` binOp "||"

conjunction = relation `chainl1` binOp "&&"
relation = do
  l <- summation
  (anyBinOp (words "== != < <= > >=") >>= \o -> summation >>= \r -> return $ o l r) <|> return l
summation = term `chainl1` anyBinOp (words "+ - ")
term = factor `chainl1` anyBinOp (words "* / % ++ !! car")
factor = literal <|> fun <|> atomicOrCall <|> ref
literal = intLiteral <|> boolLiteral "false" False <|> boolLiteral "true" True <|> stringLiteral <|> list

intLiteral = natural >>= \i -> return $ EVal (VInt (fromInteger i))
boolLiteral s v = reserved s >> (return $ EVal (VBool v))
stringLiteral = stringlit >>= \s -> return $ EVal (VString s)

ref = reserved "ref" >> factor >>= \e -> return $ ERef e
deref = reservedOp "*" >> atomic >>= \e -> return $ EDeref e

binOp s = reservedOp s >> (return $ (\a b -> ECall (EVar ("__b" ++ s)) [a, b] []))

-- Delete ?
unaryOp s = reservedOp s >> (return $ \a -> (ECall (EVar ("__u" ++ s)) [a] []))
anyBinOp ops = foldl1 (<|>) (map binOp ops)

list = do
  elements <- brackets (commaSep literal)
  return  (EVal (VList (map expr2val elements )))

fun = do
  reserved "fun"
  pars <- parens (commaSep identifier)
  body <- block
  return $ EFun pars body

variable = identifier >>= return . EVar
-- FIXME resetExpr = do ...
-- FIXME shiftExpr = do ...
--spawnExpr = do

--detachExpr = do ...
--joinExpr = do ...

-- FIXME
atomic = -- resetExpr <|> shiftExpr <|>
         -- spawnExpr <|> detachExpr <|> joinExpr <|>
         variable <|> parens expr <|> deref
atomicOrCall = do
  a <- atomic
  argss <- many (parens (commaSep expr))
  return $ foldl (\a arg -> ECall a arg []) a argss
