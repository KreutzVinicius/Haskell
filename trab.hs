
data Term
  = Atom String
  | Var Name
  | Predicate Predicate
  deriving (Show, Eq)

type Name = (Int, String)

type Predicate = (String, [Term])

type Rule = (Predicate, [Predicate])

type Substitution = [(Name, Term)]

main :: IO ()
main = do
  let a = Predicate ("likes", [Atom "alice", Var (0, "X")])
  let b = Predicate ("likes", [Var (0, "Y"), Atom "apple"])
  putStrLn "Unificação"
  print a
  print b
  print $ unifyTerm a b

compose :: Substitution -> Substitution -> Substitution
compose xs ys =
  xs ++ auxSubst xs ys

auxSubst :: Substitution -> Substitution -> Substitution
auxSubst xs ys =
  -- A |-> b deve virar A |-> (xs)b
  let aux (name, term) =
        (name, subst xs term)
   in fmap aux ys

subst :: Substitution -> Term -> Term
subst xs (Atom a) =
  Atom a
subst xs (Var a) =
  case lookup a xs of
    Just b ->
      b
    Nothing ->
      Var a
subst xs (Predicate (a, ys)) =
  Predicate(a, fmap (subst xs) ys)

unifyTerm :: Term -> Term -> Maybe Substitution
-- Regra (ATOM)
unifyTerm (Atom x) (Atom y) | x == y =
    Just []
-- Regra (VAR)
unifyTerm (Var x) (Var y) | x == y =
    Just []
-- Regra (LEFT)
unifyTerm (Var a) b =
  if occursCheck a b
    then Nothing
    else Just [(a, b)]
-- Regra (RIGHT)
unifyTerm b (Var a) =
  if occursCheck a b
    then Nothing
    else Just [(a, b)]
-- Regra (PRED)
unifyTerm (Predicate (a, xs)) (Predicate (b, ys)) | a == b =
    unifyBody xs ys

unifyTerm _ _ =
  Nothing


unifyBody :: [Term] -> [Term] -> Maybe Substitution
-- Regra (NIL)
unifyBody [] [] =
  Just []
-- Regra (PRED)
unifyBody (x : xs) (y : ys) =
  case unifyTerm x y of
    Just t1 ->
      --Regra (CONS)
      case unifyBody (fmap (subst t1) xs) (fmap (subst t1) ys) of
        Just t2 ->
          Just (compose t2 t1)
        Nothing ->
          Nothing
    Nothing ->
      Nothing
-- não unifica
unifyBody _ _ =
  Nothing

occursCheck :: Name -> Term -> Bool
occursCheck x (Atom y) =
  False
occursCheck x (Var y) =
  x == y
occursCheck x (Predicate (y, xs)) =
  any (occursCheck x) xs


--freshen :: Rule -> Rule

--resolve:: Predicate -> [Rule] -> [Substitution]

--resolveBody :: Substitution -> [Rule] -> [Predicate] -> [Substitution]