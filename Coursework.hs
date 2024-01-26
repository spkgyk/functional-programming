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


substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
substitute x n (Apply m p) = Apply (substitute x n m) (substitute x n p)

------------------------- Assignment 4

beta :: Term -> [Term]
beta (Apply (Lambda x n) m) =
  [substitute x m n] ++
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

free :: Term -> [Var]
free (Variable x)     = [x]
free (Lambda x n)     = filterVariables (free n) [x]
free (Apply  n m)     = merge (free n) (free m)

abstractions :: Term -> [Var] -> Term
abstractions n []     = n
abstractions n (x:xs) = Lambda x (abstractions n xs)

applications :: Term -> [Term] -> Term
applications n []     = n
applications n (m:ms) = applications (Apply n m) ms

lift :: Term -> Term
lift n = applications (abstractions n m) (map Variable m) 
    where m = free n


super :: Term -> Term
super (Variable x) = (Variable x)
super (Apply m n)  = Apply (super m) (super n)
super (Lambda x y) = lift (Lambda x (reducer y))
    where 
        reducer (Lambda a b) = Lambda a (reducer b)
        reducer n = super n

------------------------- Assignment 6

data Expr =
    V Var
    | A Expr Expr 

toTerm :: Expr -> Term
toTerm (V x) = Variable x
toTerm (A x y) = Apply (toTerm x) (toTerm y)

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

stripAbs :: Term -> ([Var],Term)
stripAbs (Lambda x n) = stripAbs' [x] n
stripAbs n            = ([], n)

stripAbs' :: [Var] -> Term -> ([Var],Term)
stripAbs' xs (Lambda x n) = stripAbs' (xs++[x]) n
stripAbs' xs m            = (xs, m)

takeAbs :: Term -> [Term]
takeAbs (Variable x) = []
takeAbs (Lambda x n) = [Lambda x n]
takeAbs (Apply m n)  = takeAbs m ++ takeAbs n

toExpr :: [Var] -> Term -> Expr
toExpr _ (Variable x)     = V x
toExpr (v:vs) (Lambda x n) = V v
toExpr vs (Apply m n)      =  A (toExpr a m) (toExpr b n)
    where (a,b)            = splitAt (length (takeAbs m)) vs


toInst :: [Var] -> (Var,Term) -> (Inst,[(Var,Term)],[Var])
toInst vs (i, n) = ((i, xs, e), (zip vs a), drop (length a) vs)
    where 
        (xs,m)   = stripAbs n
        a        = takeAbs m
        e        = toExpr vs m


prog :: Term -> Prog
prog n = Prog (aux names [("$main" ,super n)])
    where 
        aux :: [Var] -> [(Var,Term)] -> [Inst]
        aux n [] = []
        aux n pairs = do
            let (instruction, vas, labels) = toInst n (head pairs)
            [instruction] ++ (aux labels ((tail pairs)++vas))



example2 = Apply (Variable "S") (Apply (Apply example (numeral 0)) (Variable "0"))
example3 = Apply (Apply add (numeral 1)) (Apply (Apply mul (numeral 2)) (numeral 3))
example4 = Apply (Apply example3 (Variable "S")) (Variable "0")

------------------------- Assignment 7

sub :: [(Var,Expr)] -> Expr -> Expr
sub [] x          = x
sub ((x,e):s) (V z)
  | x==z          = e
  | otherwise     = sub s (V z)
sub stack (A m n) = A (sub stack m) (sub stack n)




--this is another version of sub

{-
subb :: [(Var,Expr)] -> Expr -> Expr
subb [] x        = x
subb stack x = sub' [] stack x

sub' :: [(Var,Expr)] -> [(Var,Expr)] -> Expr -> Expr
sub' todo [] x = allSubs todo x
sub' todo (pair:stack) x 
    | w         = sub' (pair:todo) stack x
    | otherwise = sub' todo stack x
    where w     = subSearch pair x
    
subSearch :: (Var, Expr) -> Expr -> Bool
subSearch (x, e) (V z) = (x==z)
subSearch pair (A m n) = (subSearch pair m) || (subSearch pair n)

allSubs :: [(Var,Expr)] -> Expr -> Expr
allSubs [] x        = x
allSubs (pair:s) ex = allSubs s (subExpr pair ex) 

subExpr :: (Var,Expr) -> Expr -> Expr
subExpr (x, e) (V z) 
    | x==z           = e
    | otherwise      = V z
subExpr pair (A m n) = A (subExpr pair m) (subExpr pair n)
-}



step :: [Inst] -> [Expr] -> IO [Expr]
step instructions []          = return []
step instructions ((A n m):s) = return ([n,m]++s)
step instructions ((V i):s)   = do
    let (a,(_,vs,o))          = srch i instructions
    if a 
        then do
            let lvs           = length vs
            let ls            = length s
            if ls<lvs 
                then do
                    error "step: insufficient arguments on stack" 
                else do
                    let (f,l) = splitAt lvs s
                    let snew  = (sub (zip vs f) o):l
                    return snew
        else do
            putStr (i++" ")
            return s    


srch :: Var -> [Inst] -> (Bool, Inst)
srch _ []       = (False, ("1", ["1"], V "1"))
srch x ((i, xs , e):is) 
    | i==x      = (True, (i, xs , e))
    | otherwise = srch x is


{-
--this version outputs all the variables at once.

supernormalize :: Term -> IO ()
supernormalize n = do
    let Prog p   = prog n
    let result   = concat (allSteps p [V "$main"])
    putStrLn result


allSteps :: [Inst] -> [Expr] -> [Var]
allSteps instructions []          = []
allSteps instructions ((A n m):s) = allSteps instructions ([n,m]++s)
allSteps instructions ((V i):s)   = do
    let (a,(_,vs,o))              = srch i instructions
    if a
        then do
            let lvs               = length vs
            let ls                = length s
            if ls<lvs 
                then
                    error "step: insufficient arguments on stack" 
                else do
                    let (f,l)     = splitAt lvs s
                    let snew      = (sub (zip vs f) o):l
                    allSteps instructions snew
        else do
            [i++" "]++(allSteps instructions s)
-}



--this version outputs the dropped variables seperately
supernormalize :: Term -> IO ()
supernormalize n = do
    let Prog p   = prog n
    putThroughSteps p [V "$main"]
  

putThroughSteps :: [Inst] -> [Expr] -> IO()
putThroughSteps instructions stack = do
    w <- step instructions stack
    if length w > 0
        then do
            putThroughSteps instructions w
        else 
            putStrLn ""
