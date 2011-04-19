module LogicCodes
    where


--import IO


{-Problem 46

(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and
equ/2 (for logical equivalence) which succeed or fail according to the
result of their respective operations; e.g. and(A,B) will succeed, if
and only if both A and B succeed. -}

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd False x = False
myAnd x False = False

myNand :: Bool -> Bool -> Bool
myNand True True = False
myNand False x = True
myNand x False = True

myOr :: Bool -> Bool -> Bool
myOr True x = True
myOr x True = True
myOr False False = False

myXor :: Bool -> Bool -> Bool
myXor True True = False
myXor True False = True
myXor False True = True
myXor False False = False

myImp :: Bool -> Bool -> Bool
myImp True True = True
myImp True False = False
myImp False True = True
myImp False False = True

myDimp :: Bool -> Bool -> Bool
myDimp True True = True
myDimp True False = False
myDimp False True = False
myDimp False False = True


{-A logical expression in two variables can then be written as in the
following example: and(or(A,B),nand(A,B)).

-- (\x y -> (myAnd (myOr x y ) (myNand x y)))

Now, write a predicate table/3 which prints the truth table of a given
logical expression in two variables. -}

tTable :: (Bool -> Bool -> Bool) -> IO ()
tTable x = do
  putStrLn ( (show True)++" "++(show True)++" "++(show (x True True)))
  putStrLn ( (show True)++" "++(show False)++" "++(show (x True False)))
  putStrLn ( (show False)++" "++(show True)++" "++(show (x False True)))
  putStrLn ( (show False)++" "++(show False)++" "++(show (x False False)))


{-Problem 47

(*) Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being
operators. This allows to write the logical expression in the more
natural way, as in the example: A and (A or not B). Define operator
precedence as usual; i.e. as in Java. -}

-- you mean infix `` notation?


{-Problem 48

(**) Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may
contain any number of logical variables. Define table/2 in a way that
table(List,Expr) prints the truth table for the expression Expr, which
contains the logical variables enumerated in List. -}


-- tTableM :: ([Bool] -> Bool) -> IO ()
-- tTableM x = do
  
-- how to get the lenght of the argument of x??

{-Problem 49

(**) Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.-}


gray :: Int -> [[Char]]
gray 0 = []
gray 1 = ["0","1"]
gray n =
    let aux = gray (n-1)
        myF :: [[Char]]->[[Char]]
        myF [] = []
        myF (x:xs) = ("0"++x):(myF xs)++(("1"++x):[])
    in myF aux




{-Problem 50

(***) Huffman codes.

We suppose a set of symbols with their frequencies, given as a list of
fr(S,F) terms. Example:
[fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective
is to construct a list hc(S,C) terms, where C is the Huffman code word
for the symbol S. In our example, the result could be Hs = [hc(a,'0'),
hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')]
[hc(a,'01'),...etc.].-}

{- Read enough. The huffman symbols are found by making an extended
   binary tree with the starting symbols on its leafs. The two
   lightest nodes come from a branch of weight sumed up and this node
   is going to be treated as a leaf to determine relative positions of
   the others.

Remember, ligther -> leftier -}

--data BinaryTree a
--    = Leaf a Int
--    | Branch (BinaryTree a) a (BinaryTree a)


--huffman :: [(a,Int)]->[(a,[Char])]
--huffman [] = []
--huffman hl =
    let