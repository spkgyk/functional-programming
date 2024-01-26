
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



