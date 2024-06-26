module Main where

import Text.Parsec
import qualified Text.Parsec.Token as L
import Text.Parsec.Language (emptyDef)
-- import Type
import Data.Char (isLower)
import Data.List ( (\\), nub )
import Control.Monad.Identity

-- Identificador de variável
type Id = String

-- Padrões (Pat)
data Pat = PVar Id
          | PLit Literal
          | PCon Id [Pat]
          deriving (Eq, Show)

-- Literais
data Literal = LitInt Integer
             | LitBool Bool
             deriving (Show, Eq)

-- Expressões (Expr)
data Expr = Var Id
          | Const Id
          | App Expr Expr
          | Lam Id Expr
          | Lit Literal
          | If Expr Expr Expr
          | Case Expr [(Pat, Expr)]
          | Let (Id, Expr) Expr
          deriving (Eq, Show)

-- Tipos Simples (SimpleType)
data SimpleType = TVar Id
                | TArr SimpleType SimpleType
                | TCon String
                | TApp SimpleType SimpleType
                | TGen Int
                deriving Eq

instance Show SimpleType where
    show (TVar varId) = varId
    show (TArr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (TCon s) = s
    show (TApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (TGen n) = "t" ++ show n

data Assump = Id :>: SimpleType deriving (Eq, Show)

iniCont :: [Assump]
iniCont = [
    "(,)" :>: TArr (TGen 0) (TArr (TGen 1) (TApp (TApp (TCon "(,)") (TGen 0)) (TGen 1))),
    "True" :>: TCon "Bool",
    "False" :>: TCon "Bool"
    ]
    
-- Definindo a linguagem
langDef :: L.LanguageDef ()
langDef = emptyDef {
    L.commentStart = "/*",
    L.commentEnd = "*/",
    L.commentLine = "//",
    L.identStart = letter,
    L.identLetter = alphaNum,
    L.reservedNames = ["let", "in", "case", "of", "if", "then", "else", "True", "False"],
    L.reservedOpNames = [".","=", "->"]
}

lexer :: L.TokenParser ()
lexer = L.makeTokenParser langDef

identifier = L.identifier lexer
reserved = L.reserved lexer
reservedOp = L.reservedOp lexer
parens = L.parens lexer
integer = L.integer lexer
symbol = L.symbol lexer
whiteSpace = L.whiteSpace lexer

-- Parsing expressions
expr :: ParsecT String () Identity Expr
expr = try lamAbs
   <|> try recLet
   <|> try recIf
   <|> try caseof
   <|> try tup
   <|> try app
   <|> parseNonApp

app = do
  es <- many1 parseNonApp
  return (foldl1 App es)

varOrCons = do
  i <- identifier
  return $ if isLower (head i) then Var i else Const i

litInteger = do {n <- integer; return $ LitInt n}

litBool = do {reserved "True"; return $ LitBool True}
       <|> do {reserved "False"; return $ LitBool False}

recLit = litBool <|> litInteger

lit = do {Lit <$> recLit}

lamAbs = do
    symbol "\\"
    i <- identifier
    reservedOp "."
    e <- expr
    return (Lam i e)

recIf = do
    reserved "if"
    e1 <- expr
    reserved "then"
    e2 <- expr
    reserved "else"
    If e1 e2 <$> expr

tup = parens (do {e1 <- expr; symbol ","; App (App (Const "(,)") e1) <$> expr})

recLet = do
    reserved "let"
    i <- identifier
    reservedOp "="
    e1 <- expr
    reserved "in"
    Let (i, e1) <$> expr

pvar i = do {return $ PVar i}

pcon i = do {PCon i <$> pats}

pconTup = parens (do {p1 <- pat; symbol ","; p2 <- pat; return $ PCon "(,)" [p1, p2]})

pVarOrPCon = do
    i <- identifier
    if isLower (head i) then pvar i
    else pcon i

pConsLit = do {PLit <$> recLit}

-- Suporte para padrões coringas e literais no case
pWild :: ParsecT String () Identity Pat
pWild = do {symbol "_"; return (PVar "_")}
pats = do {p <- pat; ps <- pats; return (p:ps)}
      <|> return []

pat = pconTup
    <|> try pVarOrPCon
    <|> try pConsLit
    <|> try pWild

patExpr = do
    p <- pat
    reservedOp "->"
    e <- expr
    return (p, e)

lpat = do {pe <- patExpr; pes <- maybePat; return (pe:pes)}

maybePat = do {symbol ";"; lpat}
          <|> return []

caseof = do
    reserved "case"
    e <- expr
    reserved "of"
    symbol "{"
    lp <- lpat
    symbol "}"
    return $ Case e lp

parseNonApp = try (parens expr)  -- (E)
             <|> lamAbs          -- \x.E
             <|> varOrCons       -- x or X
             <|> try caseof      -- case E of {<lpat>}
             <|> recIf           -- if E then E else E
             <|> tup             -- (E, E)
             <|> try recLet      -- let x = E in E
             <|> lit             -- bool or int

parseExpr = parse (whiteSpace >> expr) ""

-- Função para testar o parser
testParser :: [String] -> IO ()
testParser = mapM_ (\s -> case parseExpr s of
                             Left err -> putStrLn $ "Error parsing " ++ s ++ ": " ++ show err
                             Right expr -> putStrLn $ "Parsed " ++ s ++ ": " ++ show expr)


main = do


    let testCases =
                [
                 "let x = 5 in x"  -- Let simples
                , "let x = True in if x then 1 else 0"  -- Let com If
                , "if True then 1 else 0"  -- If simples
                , "if False then 5 else 10"  -- If com valor diferente
                , "case x of {True -> 1; False -> 0}"  -- Case simples
                , "f x"  -- Aplicação de função simples
                , "(f x) y"  -- Aplicação de função encadeada
                , "(1, 2)"  -- Tupla simples
                , "(True, False)"  -- Tupla de booleanos

                -- Funções Recursivas
                , "\\f. \\x. f (f x)"  -- Aplicação recursiva de função

                -- Casos Complexos de `case`
                , "case x of {0 -> 1; 1 -> 2; _ -> 0}"  -- Case com múltiplos padrões
                , "case (True, 42) of {(True, n) -> n; (False, _) -> 0}"  -- Case com tupla

                -- Literais Inteiros Grandes
                , "let x = 1000000000 in x"  -- Literal inteiro grande

                -- Expressões com Múltiplas Aplicações
                , "f (g (h x))"  -- Aplicação múltipla de funções

                -- Tuplas Aninhadas
                , "((1, 2), (3, 4))"  -- Tuplas de tuplas
                , "((True, False), (False, True))"  -- Tuplas de booleanos

                -- Lambda com Múltiplos Argumentos
                , "\\x. x"  -- Lambda simples
                , "\\x \\y x y"  -- Lambda com dois argumentos
                , "\\x. \\y. \\z.  x (y z)"  -- Lambda com três argumentos

                -- Expressões com Parênteses Aninhados
                , "(\\x. x) ((\\y. y) 5)"  -- Lambda com aplicação aninhada
                , "(f (g (h x)))"  -- Aplicações múltiplas com parênteses

                -- Expressões de `Let` Aninhadas
                , "let x = 5 in let y = x in y"  -- Let aninhado

                -- Expressões Aninhadas de `If`
                , "if x then if y then 1 else 2 else 3"  -- If aninhado

                -- `Let` com Definições Múltiplas
                , "let x = 1 in let y = 2 in x + y"  -- Let com múltiplas definições

                -- Literais Booleanos em Expressões de `Let`
                , "let b = True in if b then 1 else 0"  -- Let com booleano

                ]
    testParser testCases


