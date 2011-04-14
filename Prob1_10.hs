module Prob1_10
    where

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
