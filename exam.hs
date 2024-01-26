data Tree = Node Int [Tree]

t :: Tree

t = Node 4 [Node 5 [], Node 1 [Node 6 [], Node 2 []], Node 7 [Node 3 []]]

t2= Node 4 [Node 5 [], Node 1 [Node 6 [], Node 2 [], Node 6 [], Node 21 []], Node 7 [Node 3 [Node 6 [], Node 2 [], Node 6 [], Node 21 []]]]



treefold f (Node x ts) = f x (map (treefold f) ts)

fun1 (Node _ xs) = maximum (length xs : map fun1 xs)
-- this finds the maximum number of children in a tree

fun1fold = treefold f
    where 
        f _ xs = maximum (length xs: xs)


fun2 = treefold f
    where
        f i [] = i
        f i is = i + maximum is


fun2rec (Node x []) = x
fun2rec (Node x xs) = x + maximum (map fun2rec xs)