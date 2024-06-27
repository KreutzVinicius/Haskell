module Main where
import Text.Parsec
import qualified Text.Parsec.Token as L
import Text.Parsec.Language (emptyDef)
import Data.Char (isLower)
import Data.List ( (\\), nub )
import Control.Monad.Identity
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State qualified as CMState
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

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

litInteger :: ParsecT String () Identity Literal
litInteger = LitInt <$> integer

litBool :: ParsecT String () Identity Literal
litBool =
  (reserved "True" >> return (LitBool True))
    <|> (reserved "False" >> return (LitBool False))

recLit = litBool <|> litInteger

-- Parsing de expressões literais
lit :: ParsecT String () Identity Expr
lit = Lit <$> (litBool <|> litInteger)

lamAbs = do
    symbol "\\"
    i <- identifier
    reservedOp "."
    Lam i <$> expr

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

-- Inferidor de tipos

type Subst = Map Id SimpleType

nullSubst :: Subst
nullSubst = Map.empty

(+->) :: Id -> SimpleType -> Subst
(+->) = Map.singleton

class Types a where
  apply :: Subst -> a -> a
  tv :: a -> [Id]

instance Types SimpleType where
  apply s (TVar n) = Map.findWithDefault (TVar n) n s
  apply s (TArr l r) = TArr (apply s l) (apply s r)
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply _ t = t

  tv (TVar n) = [n]
  tv (TArr l r) = tv l ++ tv r
  tv (TApp l r) = tv l ++ tv r
  tv _ = []

instance (Types a) => Types [a] where
  apply s = map (apply s)
  tv = nub . concatMap tv

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

type TypeEnv = Map Id SimpleType

remove :: TypeEnv -> Id -> TypeEnv
remove env var = Map.delete var env

instance Types TypeEnv where
  apply s env = Map.map (apply s) env
  tv env = tv $ Map.elems env

type TI a = ExceptT String (CMState.State TIState) a

data TIState = TIState {tiSupply :: Int}

newTyVar :: String -> TI SimpleType
newTyVar prefix = do
  s <- CMState.get
  CMState.put s {tiSupply = tiSupply s + 1}
  return $ TVar (prefix ++ show (tiSupply s))

instantiate :: SimpleType -> TI SimpleType
instantiate (TGen n) = newTyVar "a"
instantiate (TArr t1 t2) = TArr <$> instantiate t1 <*> instantiate t2
instantiate (TApp t1 t2) = TApp <$> instantiate t1 <*> instantiate t2
instantiate t = return t

mgu :: SimpleType -> SimpleType -> TI Subst
mgu (TArr l r) (TArr l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TApp l r) (TApp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
mgu (TCon c1) (TCon c2)
  | c1 == c2 = return nullSubst
mgu t1 t2 =
  throwError $
    "types do not unify: "
      ++ show t1
      ++ " vs. "
      ++ show t2

varBind :: Id -> SimpleType -> TI Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `elem` tv t =
      throwError $
        "occurs check fails: "
          ++ u
          ++ " vs. "
          ++ show t
  | otherwise = return (u +-> t)

ti :: TypeEnv -> Expr -> TI (Subst, SimpleType)
ti env (Var n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " ++ n
    Just sigma -> do
      t <- instantiate sigma
      return (nullSubst, t)
ti env (App e1 e2) = do
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  tv' <- newTyVar "a"
  s3 <- mgu (apply s2 t1) (TArr t2 tv')
  return (s3 `compose` s2 `compose` s1, apply s3 tv')
ti env (Lam n e) = do
  tv' <- newTyVar "a"
  let env' = remove env n
  (s1, t1) <- ti (env' `Map.union` (n +-> tv')) e
  return (s1, TArr (apply s1 tv') t1)
ti env (Lit (LitInt _)) = return (nullSubst, TCon "Int")
ti env (Lit (LitBool _)) = return (nullSubst, TCon "Bool")
ti env (If e1 e2 e3) = do
  (s1, t1) <- ti env e1
  (s2, t2) <- ti (apply s1 env) e2
  (s3, t3) <- ti (apply s2 env) e3
  s4 <- mgu t1 (TCon "Bool")
  s5 <- mgu t2 t3
  return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t3)
ti env (Let (x, e1) e2) = do
  (s1, t1) <- ti env e1
  let env' = remove env x
  let t' = generalize (apply s1 env) t1
  (s2, t2) <- ti (env' `Map.union` (x +-> t')) e2
  return (s2 `compose` s1, t2)
ti env (Case e ps) = do
  (s1, t1) <- ti env e
  (s2, t2) <-
    foldM
      ( \(subst, t) (p, e') -> do
          (s3, env') <- tiPat (apply subst env) p
          (s4, t') <- ti (env' `Map.union` apply s3 env) e'
          s5 <- mgu t t'
          return (s5 `compose` s4 `compose` s3 `compose` subst, t')
      )
      (s1, TVar "")
      ps
  return (s2, t2)

tiPat :: TypeEnv -> Pat -> TI (Subst, TypeEnv)
tiPat env (PVar n) = do
  tv' <- newTyVar "a"
  return (nullSubst, Map.insert n tv' env)
tiPat env (PLit (LitInt _)) = return (nullSubst, env)
tiPat env (PLit (LitBool _)) = return (nullSubst, env)
tiPat env (PCon n ps) = do
  tv' <- newTyVar "a"
  (s1, env') <-
    foldM
      ( \(subst, env'') p -> do
          (s2, env''') <- tiPat (apply subst env'') p
          return (s2 `compose` subst, env''' `Map.union` env'')
      )
      (nullSubst, env)
      ps
  return (s1, env' `Map.union` env)

generalize :: TypeEnv -> SimpleType -> SimpleType
generalize env t = foldr TArr t (map TVar (tv t \\ tv env))

runTI :: TI a -> Either String a
runTI t = CMState.evalState (runExceptT t) (TIState 0)

runInfer :: TypeEnv -> Expr -> Either String SimpleType
runInfer env e = CMState.evalState (runExceptT (snd <$> ti env e)) (TIState 0)

-- Função para testar o parser
testParser :: [String] -> IO ()
testParser = mapM_ (\s -> case parseExpr s of
                             Left err -> putStrLn $ "Error parsing " ++ s ++ ": " ++ show err
                             Right expr -> putStrLn $ "Parsed " ++ s ++ ": " ++ show expr)


main :: IO ()
main = do

    let typeEnv = Map.fromList [(i, t) | (i :>: t) <- iniCont]
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
                , "\\x. \\y. x y"  -- Lambda com dois argumentos
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
    -- testParser testCases
    mapM_ (\exprStr ->
        case parseExpr exprStr of
        Left err -> putStrLn $ "Error parsing '" ++ exprStr ++ "': " ++ show err
        Right expr ->
            case runInfer typeEnv expr of
            Left err -> putStrLn $ "Error inferring type for '" ++ exprStr ++ "': " ++ err
            Right t -> putStrLn $ "Expression: '" ++ exprStr ++ "' has type: " ++ show t
        ) testCases

