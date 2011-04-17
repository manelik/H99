module Prob1_10
    where

--import Random


-- Standard random function

getRndInt :: () -> Int
getRndInt () = 4 -- chosen by fair dice roll.
                 -- guaranteed to be random.

getRndinRange :: Int -> Int -> Int
getRndinRange a b =
    if a <= b then
        if a > 4 then a 
        else if b < 4 then b
             else 4
    else 4


-- Actually ListProblems would do a better name

{- Problem 1

(*) Find the last element of a list.  -}
myLast :: [a]-> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs


{- Problem 2

(*) Find the last but one element of a list. -} 
myButLast :: [a]-> Maybe a
myButLast [] = Nothing
myButLast (x:[]) = Nothing
myButLast (x:(y:[])) = Just x
myButLast (x:xs) = myButLast xs


{- Problem 3

(*) Find the K'th element of a list. The first element in the list is number 1. -}
elementAt :: [a]-> Int -> Maybe a
elementAt [] n = Nothing
elementAt (x:xs) 1 = Just x 
elementAt (x:xs) n = elementAt xs (n-1)


{- Problem 4

(*) Find the number of elements of a list. -}
myLenght :: [a]-> Int
myLenght [] = 0
myLenght (x:xs) = 1 + (myLenght xs)


{- Problem 5

(*) Reverse a list. -}
myReverse :: [a]->[a]
myReverse [] = []
myReverse (x:xs) = 
    let x1 = (myReverse xs)
    in x1 ++ (x:[])


{- Problem 6

(*) Find out whether a list is a palindrome. A palindrome can be read
forward or backward; e.g. (x a m a x). -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome x =
    let x1 = (myReverse x)
    in if x==x1 then True
       else False

{- Problem 7

(**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat'
list by replacing each list with its elements (recursively). -}

--Need to learn hoe to handle this kind of structures

{- Problem 8

(**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a
single copy of the element. The order of the elements should not be
changed. -}
transAltuzar :: Eq a => [a]-> [a]
transAltuzar [] = []
transAltuzar (x:[]) = x:[]
transAltuzar (x:xs) =
    let x1 =  if x /= (head xs) then x:[]
              else []
    in x1++(transAltuzar xs)

{- Problem 9

(**) Pack consecutive duplicates of list elements into sublists.  If a
 list contains repeated elements they should be placed in separate
 sublists.-}
pack :: Eq a => [a]->[[a]]
pack [] = []
pack (x:[]) = (x:[]):[]
pack (x:xs) =
    if x==(head xs) then (x:(head . pack)xs):((tail . pack) xs)
    else (x:[]):(pack xs)

{- Problem 10

(*) Run-length encoding of a list. Use the result of problem P09 to
implement the so-called run-length encoding data compression
method. Consecutive duplicates of elements are encoded as lists (N E)
where N is the number of duplicates of the element E. -}

encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
--lazy solution
encode x =
    let pcount [] = []
        pcount a = (((myLenght.head) a , (head.head) a):[])++((pcount.tail) a)
    in (pcount.pack) x
{- Another not so lazy way is to use the same rcursive algrithm as
 pack.  If there are consecutive duplicates rebulid head, if there is
 a change insert new head-}


{- Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has
no duplicates it is simply copied into the result list. Only elements
with duplicates are transferred as (N E) lists. -}

--Need a list tipe that has pairs and single values as elements


{- Problem 12

(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem
11. Construct its uncompressed version. -}

-- unmodified
decode :: [( Int , a )] -> [a]
decode [] = []
decode ((0,a):xs) = decode xs
decode ((n,a):xs) = a:[] ++ decode ( (n-1,a):xs)


{- Problem 13

(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method
directly. I.e. don't explicitly create the sublists containing the
duplicates, as in problem 9, but only count them. As in problem P11,
simplify the result list by replacing the singleton lists (1 X) by
X. -}

-- this is the not so lazy method I was talking about

encodeD :: Eq a => [a]->[(Int,a)]
encodeD [] = []
encodeD (x:[]) = (1,x):[]
encodeD (x:xs) =
    let xh= (head . encodeD) xs
    in if x==(snd xh) then ( (fst xh)+1, x) :((tail . encodeD) xs)
       else (1,x):(encodeD xs)

{- Problem 14

(*) Duplicate the elements of a list.  -}

duppeList :: [a] -> [a]
duppeList [] = []
duppeList (x:xs) = x:x:(duppeList xs)

-- iteration of this function is almost equivalent to translate 
-- a list to altuzar-spiiiiiiiiikkkkkkkkkkk 


{- Problem 15

(**) Replicate the elements of a list a given number of times.  -}

repList :: Int -> [a] -> [a]
repList n [] = []
repList 0 x = []
repList n (x:xs) = x:[]++(repList (n-1) (x:[]))++(repList n xs)


{- Problem 16

(**) Drop every N'th element from a list. -}

mydrop :: Int -> [a] -> [a]
mydrop n [] = []
--mydrop 1 x = []
mydrop n (x:xs) = 
    let rm1occur nl [] = ([],[])
        rm1occur 1 xl = ([],tail xl)
        rm1occur nl (xl:xsl) = (xl:fst((rm1occur (nl-1) xsl)),snd ((rm1occur (nl-1) xsl)) ) 
    in fst(rm1occur n (x:xs))++(mydrop n (snd(rm1occur n (x:xs))) )

{- Problem 17

(*) Split a list into two parts; the length of the first part is
given. -}

mySplit :: Int -> [a] -> ([a],[a])
mySplit n [] = ([],[])
mySplit 1 (x:xs) = (x:[],xs)
mySplit n (x:xs) = (x:fst((mySplit (n-1) xs)),snd ((mySplit (n-1) xs)) )

{- Problem 18

(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the
elements between the i'th and k'th element of the original list (both
limits included). Start counting the elements with 1.  -}

slice :: Int -> Int -> [a] ->[a]
slice n m [] = []
slice 1 1 (x:xs) = x:[]
slice 1 m (x:xs) = x:(slice 1 (m-1) xs)
slice n m (x:xs) = slice (n-1)(m-1) xs


{- Problem 19

(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).  -}

rotate :: Int -> [a] ->[a]
rotate n [] = []
rotate n x =
    let aux = mySplit (mod n (length x)) x
    in (snd aux)++(fst aux)

{- Problem 20

(*) Remove the K'th element from a list.  -}

popAt :: Int -> [a] -> (a,[a])
--popAt n [] = ([],[]) --this case will be met if n > length x
popAt 1 (x:xs) = (x,xs)
popAt n (x:xs) = (fst(popAt (n-1) xs), x:(snd (popAt (n-1) xs)) )


{- Problem 21

Insert an element at a given position into a list. -}

insertAt :: Int -> a -> [a] -> [a]
insertAt n a [] = a:[] --for the moment
insertAt 1 a x = a:x
insertAt n a (x:xs) = x:(insertAt (n-1) a xs)

{- Problem 22

Create a list containing all integers within a given range. -}


myRange :: (Ord a, Num a) => a -> a -> [a]
--myRange n n = n:[]
myRange n m = if n>m then []
              else n:(myRange (n+1) m)

{- Problem 23

Extract a given number of randomly selected elements from a list. -}

rndSelect :: Int -> [a] -> [a]
rndSelect n [] = []
rndSelect 0 x = []
rndSelect n (x:xs) =     
    let aux = popAt (getRndinRange 1 (myLenght (x:xs))) (x:xs)
    in (fst aux):(rndSelect (n-1) (snd aux))


{- Problem 24

Lotto: Draw N different random numbers from the set 1..M. -}

diffSelect :: Int -> Int -> [Int]
diffSelect a b =
    if a > b then myRange 1 b
    else rndSelect a (myRange 1 b)


{- Problem 25

Generate a random permutation of the elements of a list. -}

-- leave it for later


{- Problem 26

(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12
people? We all know that there are C(12,3) = 220 possibilities (C(N,K)
denotes the well-known binomial coefficients). For pure
mathematicians, this result may be great. But we want to really
generate all the possibilities in a list.-}

-- have to double-check exit conditions
combos :: Int -> [a] -> [[a]]
combos n [] = []
combos 0 x = []
combos 1 x =
    let spl :: [a]->[[a]]
        spl [] = []
        spl (x:xs)= (x:[]):(spl xs)
    in spl x
combos n (x:xs) =
    let sub = combos (n-1) xs 
        zipp :: a -> [[a]] ->[[a]]
        zipp a [] = []
        zipp a [[]] = (a:[]):[]
        zipp a x1 =(a:(head x1)):(zipp a (tail x1))
    in (zipp x sub)  ++ (
                        if n <= (myLenght xs) then (combos n xs)
                        else []
                        ) 

{- Problem 27 

Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint
subgroups of 2, 3 and 4 persons? Write a function that generates all
the possibilities and returns them in a list. 

b) Generalize the above predicate in a way that we can specify a list
of group sizes and the predicate will return a list of groups. -}

-- Later
-- myGroup :: [Int] -> [[a]] -> [[[[a]]]]


{- Problem 28

Sorting a list of lists according to length of sublists

a) We suppose that a list contains elements that are lists
themselves. The objective is to sort the elements of this list
according to their length. E.g. short lists first, longer lists later,
or vice versa. 

-}

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:[]) = x:[]
lsort (x:xs) = 
    let stail = lsort xs
    in if (myLenght x) <= ( (myLenght.head)stail) then  x:stail
       else (head stail):(lsort (x:(tail stail)) )
    
{-

b) Again, we suppose that a list contains elements that are lists
themselves. But this time the objective is to sort the elements of
this list according to their length frequency; i.e., in the default,
where sorting is done ascendingly, lists with rare lengths are placed
first, others with a more frequent length come later. -}

-- later


