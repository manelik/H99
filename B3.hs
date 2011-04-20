module B3
    where

import Lists (myLenght,mySplit)

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
cbalTree 1 = (leaf 'x'):[]
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


{-Problem 57

(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to
write a predicate to construct a binary search tree from a list of
integer numbers. -}


consTree :: [a] -> Tree a
consTree [] = Empty
consTree (x:[])= Branch x Empty Empty
--consTree (x:x1:[])= Branch x (consTree (x1:[])) Empty
--consTree (x:x1:x2:[])= Branch x (consTree (x1:[])) (consTree (x2:[])) 
consTree (x:xs) = 
    let 
        stail= mySplit (div (myLenght xs) 2) xs 
    in Branch x (consTree (fst stail)) (consTree (snd stail))


{-Problem 58

(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric,
completely balanced binary trees with a given number of nodes.  -}

-- We can generate balanced Trees, and see if a given tree is symmetric

-- maybe improved by a on the go check?


scbalTrees :: Int -> [Tree Char]
scbalTrees n = filter symmTree (cbalTree n) 


{-Problem 59

(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for
every node: The height of its left subtree and the height of its right
subtree are almost equal, which means their difference is not greater
than one. -}





{-Problem 60

(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the
maximum number of nodes it can contain?  Clearly, MaxN = 2**H -
1. However, what is the minimum number MinN? This question is more
difficult. Try to find a recursive statement and turn it into a
function minNodes that returns the minimum number of nodes in a
height-balanced binary tree of height H. On the other hand, we might
ask: what is the maximum height H a height-balanced binary tree with N
nodes can have? Write a function maxHeight that computes this.

Now, we can attack the main problem: construct all the height-balanced
binary trees with a given nuber of nodes. Find out how many
height-balanced trees exist for N = 15. -}



{-Problem 61 

Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2
to count them. -}


leafcount :: Tree a -> Int
leafcount Empty = 0
leafcount (Branch x Empty Empty) = 1
leafcount (Branch x lefty righty)=
    (leafcount lefty) + (leafcount righty)

{-Problem 61A

Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to
collect them in a list. -}

leafcollect :: Tree a -> [a]
leafcollect Empty = []
leafcollect (Branch x Empty Empty) = x:[]
leafcollect (Branch x lefty righty)=
    (leafcollect lefty) ++ (leafcollect righty)


{-Problem 62

Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty
successors. Write a predicate internals/2 to collect them in a list.
-}

intcollect :: Tree a -> [a]
intcollect Empty = []
intcollect (Branch x Empty Empty) = []
intcollect (Branch x lefty righty)=
    x:[]++(intcollect lefty) ++ (intcollect righty)


{-Problem 62B

Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the
node has length N-1. The root node is at level 1. Write a predicate
atlevel/3 to collect all nodes at a given level in a list.-}

levcollect :: Int -> Tree a -> [a]
levcollect n Empty = []
levcollect 1 (Branch x lefty righty) = x:[]
levcollect n (Branch x lefty righty) =
    x:[]++(levcollect (n-1) lefty)++(levcollect (n-1) righty)


{-Problem 63

Construct a complete binary tree

A complete binary tree with height H is defined as follows:

    The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e
    2**(i-1) at the level i) In level H, which may contain less than
    the maximum possible number of nodes, all the nodes are
    "left-adjusted". This means that in a levelorder tree traversal
    all internal nodes come first, the leaves come second, and empty
    successors (the nil's which are not really nodes!) come last.

Particularly, complete binary trees are used as data structures (or
addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree
by enumerating the nodes in level-order, starting at the root with
number 1. For every node X with address A the following property
holds: The address of X's left and right successors are 2*A and 2*A+1,
respectively, if they exist. This fact can be used to elegantly
construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.-}

compTree :: Int -> Tree Char
compTree 0 = Empty
compTree n =
    let 
        hleft i = if (n+1)>=(2^(i-1)) then hleft (i+1)
                  else i-1
        hloc = hleft 1
        res = n + 1 - 2^(hloc-1)
        nb = 2^(hloc-2)-1
        nsplit = if res<=(nb+1) then (res,0)
                 else ((nb +1),res - (nb+1))
        rn = nb  + (snd nsplit)
        ln = nb  + (fst nsplit)
    in Branch 'x' (compTree ln) (compTree rn)




