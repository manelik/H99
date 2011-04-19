module B3
    where

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty

{-Problem 54A

(*) Check whether a given term represents a binary tree

In Prolog or Lisp, one writes a predicate to do this.

Non-solution: Haskell's type system ensures that all terms of type
Tree a are binary trees: it is just not possible to construct an
invalid tree with this type. Hence, it is redundant to introduce a
predicate to check this property: it would always returnTrue.-}



{-Problem 55

(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for
every node: The number of nodes in its left subtree and the number of
nodes in its right subtree are almost equal, which means their
difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary
trees for a given number of nodes. The predicate should generate all
solutions via backtracking. Put the letter 'x' as information into all
nodes of the tree. -}

--xmap takes a list of functions, a list and returns a list of lists,
--each inner list is a map itself applied to the second argument

xmap :: [(a->b)] -> [a] -> [[b]]
xmap f [] = []
xmap [] x = []
xmap (f:fs) x = (map f x):(xmap fs x) 


cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = (Branch 'x' Empty Empty):[]
cbalTree n = 
    let n1= div (n-1) 2
        n2= n1 + (mod (n-1) 2)
        b1 = cbalTree n1
        b2 = cbalTree n2 
        accum = (\b c-> (Branch 'x' b c ):[]
                        ++ if n2 /= n1 then (Branch 'x' c b ):[]
                           else []) 
    in  foldr (++) [] (foldr (++) [] (xmap (map accum b1) b2))
   
{-Problem 56

(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line
through the root node and then the right subtree is the mirror image
of the left subtree. Write a predicate symmetric/1 to check whether a
given binary tree is symmetric. Hint: Write a predicate mirror/2 first
to check whether one tree is the mirror image of another. We are only
interested in the structure, not in the contents of the nodes. -}

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True                   
mirror Empty (Branch rv rl rr) = False      
mirror (Branch lv ll lr) Empty = False      
mirror (Branch lv ll lr) (Branch rv rl rr) =
    and [(mirror ll rr),(mirror lr rl)]

symmTree :: Tree a -> Bool
symmTree Empty = True                   
symmTree (Branch lv lefty righty) =
    mirror lefty righty




