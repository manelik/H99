module Prob1_10
    where

-- Actually ListProblems would do a better name

{-1 Problem 1

(*) Find the last element of a list.  -}
myLast :: [a]-> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs


{- 2 Problem 2

(*) Find the last but one element of a list. -} 
myButLast :: [a]-> Maybe a
myButLast [] = Nothing
myButLast (x:[]) = Nothing
myButLast (x:(y:[])) = Just x
myButLast (x:xs) = myButLast xs


{-3 Problem 3

(*) Find the K'th element of a list. The first element in the list is number 1. -}
elementAt :: [a]-> Int -> Maybe a
elementAt [] n = Nothing
elementAt (x:xs) 1 = Just x 
elementAt (x:xs) n = elementAt xs (n-1)


{-4 Problem 4

(*) Find the number of elements of a list. -}
myLenght :: [a]-> Int
myLenght [] = 0
myLenght (x:xs) = 1 + (myLenght xs)

{-5 Problem 5

(*) Reverse a list. -}
myReverse :: [a]->[a]
myReverse [] = []
myReverse (x:xs) = 
    let x1 = (myReverse xs)
    in x1 ++ (x:[])

{-6 Problem 6

(*) Find out whether a list is a palindrome. A palindrome can be read
forward or backward; e.g. (x a m a x). -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome x =
    let x1 = (myReverse x)
    in if x==x1 then True
       else False

{-7 Problem 7

(**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat'
list by replacing each list with its elements (recursively). -}

--Need to learn hoe to handle this kind of structures

{-8 Problem 8

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

{-9 Problem 9

(**) Pack consecutive duplicates of list elements into sublists.  If a
 list contains repeated elements they should be placed in separate
 sublists.-}
pack :: Eq a => [a]->[[a]]
pack [] = []
pack (x:[]) = (x:[]):[]
pack (x:xs) =
    if x==(head xs) then (x:(head . pack)xs):((tail . pack) xs)
    else (x:[]):(pack xs)

{-10 Problem 10

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


{-1 Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has
no duplicates it is simply copied into the result list. Only elements
with duplicates are transferred as (N E) lists. -}

--Need a list tipe that has pairs and single values as elements


{-2 Problem 12

(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem
11. Construct its uncompressed version. -}

-- unmodified
decode :: [( Int , a )] -> [a]
decode [] = []
decode ((0,a):xs) = decode xs
decode ((n,a):xs) = a:[] ++ decode ( (n-1,a):xs)


{- 3 Problem 13

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

{-4 Problem 14

(*) Duplicate the elements of a list.  -}

duppeList :: [a] -> [a]
duppeList [] = []
duppeList (x:xs) = x:x:(duppeList xs)

-- iteration of this function is almost equivalent to translate 
-- a list to altuzar-spiiiiiiiiikkkkkkkkkkk 


{-5 Problem 15

(**) Replicate the elements of a list a given number of times.  -}

repList :: Int -> [a] -> [a]
repList n [] = []
repList 0 x = []
repList n (x:xs) = x:[]++(repList (n-1) (x:[]))++(repList n xs)


{- 6 Problem 16

(**) Drop every N'th element from a list. -}

mydrop :: Int -> [a] -> [a]
mydrop n [] = []
--mydrop 1 x = []
mydrop n (x:xs) = 
    let rm1occur nl [] = ([],[])
        rm1occur 1 xl = ([],tail xl)
        rm1occur nl (xl:xsl) = (xl:fst((rm1occur (nl-1) xsl)),snd ((rm1occur (nl-1) xsl)) ) 
    in fst(rm1occur n (x:xs))++(mydrop n (snd(rm1occur n (x:xs))) )

{- 7 Problem 17

(*) Split a list into two parts; the length of the first part is
given. -}

mySplit :: Int -> [a] -> ([a],[a])
mySplit n [] = ([],[])
mySplit 1 (x:xs) = (x:[],xs)
mySplit n (x:xs) = (x:fst((mySplit (n-1) xs)),snd ((mySplit (n-1) xs)) )

{-8 Problem 18

(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the
elements between the i'th and k'th element of the original list (both
limits included). Start counting the elements with 1.  -}

slice :: Int -> Int -> [a] ->[a]
slice n m [] = []
slice 1 1 (x:xs) = x:[]
slice 1 m (x:xs) = x:(slice 1 (m-1) xs)
slice n m (x:xs) = slice (n-1)(m-1) xs


{-9 Problem 19

(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).  -}

rotate :: Int -> [a] ->[a]
rotate n [] = []
rotate n x =
    let aux = mySplit (mod n (length x)) x
    in (snd aux)++(fst aux)

{-10 Problem 20

(*) Remove the K'th element from a list.  -}

