-------------------------
-------- PART A --------- 
-------------------------


import Data.List

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
--  deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


------------------------- Assignment 1

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
  where
    numeral' i
      | i <= 0    = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i-1))


-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2

variables :: [Var]
variables = map (:[]) ['a'..'z'] ++ [ x : show i | i <- [1..] , x <- ['a'..'z'] ]

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs []     = xs 
filterVariables xs (y:ys) = filter (/=y) (filterVariables xs ys)

fresh :: [Var] -> Var
fresh = head . filterVariables variables

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = merge [x] (used n)
used (Apply  n m) = merge (used n) (used m)


------------------------- Assignment 3


rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | z == x    = Lambda z n
    | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m)


allSubsstitute :: Var -> Term -> Term -> Term
allSubsstitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
allSubsstitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (allSubsstitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
allSubsstitute x n (Apply m p) = Apply (allSubsstitute x n m) (allSubsstitute x n p)

------------------------- Assignment 4

beta :: Term -> [Term]
beta (Apply (Lambda x n) m) =
  [allSubsstitute x m n] ++
  [Apply (Lambda x n') m  | n' <- beta n] ++
  [Apply (Lambda x n)  m' | m' <- beta m]
beta (Apply n m) =
  [Apply n' m  | n' <- beta n] ++
  [Apply n  m' | m' <- beta m]
beta (Lambda x n) = [Lambda x n' | n' <- beta n]
beta (Variable _) = []


normalize :: Term -> Term
normalize n
    | null ns   = n
    | otherwise = normalize (head ns)
  where ns = beta n

run :: Term -> IO ()
run n = do
  print n
  let ns = beta n
  if null ns then
    return ()
  else
    run (head ns)

 
-------------------------

suc    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Apply (Variable "n") (Variable "f")) (Variable "x")))))
add    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Variable "f")) (Apply (Apply (Variable "n") (Variable "f")) (Variable "x"))))))
mul    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Apply (Variable "n") (Variable "f"))) (Variable "x")))))
dec    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Apply (Variable "n") (Lambda "g" (Lambda "h" (Apply (Variable "h") (Apply (Variable "g") (Variable "f")))))) (Lambda "u" (Variable "x"))) (Lambda "x" (Variable "x")))))
minus  = Lambda "n" (Lambda "m" (Apply (Apply (Variable "m") dec) (Variable "n")))

-------------------------
-------- PART B --------- 
-------------------------

------------------------- Assignment 5





-- unique function removes duplicates from list (used in free)
unique :: [Var] -> [Var]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = sort(unique(rid x (free n)))
  -- rid removes x from list
  where rid x list = filter (\e -> e/=x) list
free (Apply  n m) = sort(unique (free m ++ free n))

abstractions :: Term -> [Var] -> Term
abstractions n []                = n
abstractions n (x:xs) = Lambda x (abstractions n xs)


applications :: Term -> [Term] -> Term
applications n []     = n
applications n (x:xs) = applications (Apply n x) xs

-- use above functions and need to map free variables to variables
lift :: Term -> Term
lift n = applications (abstractions n (free n)) (map Variable (free n))


super :: Term -> Term
super (Apply n m)  = Apply (super n) (super m)
super (Variable x) = Variable x
super (Lambda x n) = lift(Lambda x (auxiliary n))
  where
    auxiliary (Lambda x n) = Lambda x (auxiliary n)
    auxiliary n            = super n

------------------------- Assignment 6

data Expr =
    V Var
  | A Expr Expr

toTerm :: Expr -> Term
toTerm(V x)   = Variable x
toTerm(A n m) = Apply (toTerm n) (toTerm m)

instance Show Expr where
  show = show . toTerm

type Inst = (Var, [Var], Expr)

data Prog = Prog [Inst]

instance Show Prog where
  show (Prog ls) = unlines (map showInst ks)
    where
      ks = map showParts ls
      n  = maximum (map (length . fst) ks)
      showParts (x,xs,e) = (x ++ " " ++ unwords xs , show e)
      showInst (s,t) = take n (s ++ repeat ' ') ++ " = " ++ t


names = ['$':show i | i <- [1..] ]

-------------------------

-- finds Application / Variable given a Term
findN :: Term -> Term
findN (Lambda x n) = findN n
findN (Variable x) = Variable x
findN (Apply n m)  = Apply n m

-- returns lambda terms in abstraction
abs' :: Term -> [Var]
abs' (Lambda x n) = [x] ++ abs' n
abs' n = []

stripAbs :: Term -> ([Var],Term)
stripAbs n = (abs' n, findN n)
-- Note these functions used are defined above

takeAbs :: Term -> [Term]
takeAbs(Lambda x n) = [Lambda x n]
takeAbs(Variable x) = []
takeAbs(Apply n m)  = takeAbs n ++ takeAbs m

toExpr :: [Var] -> Term -> Expr
toExpr _ (Variable y)      = V y
toExpr (x:xs) (Lambda y n) = V x
toExpr x (Apply n m)       = A (toExpr (take (length (takeAbs n)) x) n) (toExpr (drop (length (takeAbs n)) x) m)

toInst :: [Var] -> (Var,Term) -> (Inst,[(Var,Term)],[Var])
toInst x (v, n) = ((v, abs' n, toExpr x (findN n)), zip x (takeAbs (findN n)), drop (length (takeAbs (findN n))) x)

prog :: Term -> Prog
prog n = Prog (aux names [("$main", super n)])
  where
    aux :: [Var] -> [(Var,Term)] -> [Inst]
    aux x [] = []
    aux x (a:az) = [i] ++ aux iss az ++ aux iss is
      where (i,is,iss) = toInst x a


example2 = Apply (Variable "S") (Apply (Apply example (numeral 0)) (Variable "0"))
example3 = Apply (Apply add (numeral 1)) (Apply (Apply mul (numeral 2)) (numeral 3))
example4 = Apply (Apply example3 (Variable "S")) (Variable "0")

------------------------- Assignment 7

sub :: [(Var,Expr)] -> Expr -> Expr
sub [] e       = e
sub ((x,y):z) (V e)
  | x==e       = y
  | otherwise  = sub z (V e)
sub x (A e f)  = A (sub x e) (sub x f)



step :: [Inst] -> [Expr] -> IO [Expr]
step _ []             = return []
step inst ((A p q):m) = return ([p,q]++m)
step inst ((V n):m)   = do
  let proggy = [(x,i,e) | (x,i,e) <- inst]
  let c = filter (\(x,i,e)-> x == n) proggy
  let len = length c
  if len == 0 then do
    putStr (n ++ " ")
    return m
    else do
      let [(x,i,e)] = c
      if length i > length m then do
        error "step: insufficient arguments on stack"
        else do
          let paired = zip i (take (length i) m)
          return ([sub paired e] ++ drop (length i) m)


supernormalize :: Term -> IO ()
supernormalize n = do
 let Prog inst = prog n
 let outputs = step' inst [V "$main"]
 putStr (output (outputs))

-- function that recursively does step and puts 'outputs' in a list
step' :: [Inst] -> [Expr] -> [Expr]
step' _ [] = []
step' inst ((A p q):m) = step' inst ([p,q]++m)
step' inst ((V n):m)
    | length c > 0 && length i > length m = error "step: insufficient arguments on stack"
    | length c > 0 && length i <= length m = step' inst ([sub paired e] ++ drop (length i) m)
    | otherwise = [V n] ++ (step' inst m)
    where
      proggy = [(x,i,e) | (x,i,e) <- inst]
      c = filter (\(x,i,e)-> x == n) proggy
      [(x,i,e)] = c
      paired = zip i (take (length i) m )

-- functions to effectively print list of expressions
output :: [Expr]-> String
output ((V x):xs)   = x ++ " " ++ output xs
output ((A x y):xs) = output xs
output []           = "\n"