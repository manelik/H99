module Arithmetic
    where 




{-Problem 31

(**) Determine whether a given integer number is prime. -}

isPrime :: Int -> Bool
isPrime x =
    let recDiv :: Int -> Int -> Bool
        recDiv n m = 
            if n==m then True
            else
                if (mod m n) == 0 then False
                else recDiv (n+1) m
    in recDiv 2 x

-- related nextPrime
-- calculates the next prime from a given Int

nextPrime :: Int -> Int
nextPrime 1 = 2
nextPrime n = 
    if isPrime (n+1) then n+1
    else nextPrime (n+1)

{-Problem 32

(**) Determine the greatest common divisor of two positive integer
numbers. Use Euclid's algorithm. -}

myGCD :: Int -> Int -> Int
myGCD x y =
    let aux = mod x y
    in if aux==0 then y
       else myGCD y aux


{- Problem 33

(*) Determine whether two positive integer numbers are coprime. Two
numbers are coprime if their greatest common divisor equals 1. -}

coprime :: Int -> Int -> Bool
coprime x y =
    if x >= y then
       if (myGCD x y) == 1 then True
       else False
    else coprime y x

 
{- Problem 34

(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of
positive integers r (1 <= r < m) that are coprime to m. -}

totient :: Int -> Int
totient 1 = 1
totient n =
    let aux 1 = 1
        aux x = if (coprime n x) then 1 + (aux (x-1)) 
                else aux (x-1)
    in aux n


{-Problem 35

(**) Determine the prime factors of a given positive
integer. Construct a flat list containing the prime factors in
ascending order. -}

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n =
    let aux x = if (mod n x) == 0 then x --return factor
                else (aux . nextPrime) x --test next
                -- if n is prime this will reach x=m and obviously exits
        fact = aux 2
    in fact:(primeFactors (div n fact))


{-Problem 36

(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity. -}

encodeP :: [Int] -> [(Int,Int)]
encodeP [] = []
encodeP (x:[]) = (x,1):[]
encodeP (x:xs) = 
    let aux = encodeP xs
    in if x == ((fst . head) aux) then (x,((snd.head)aux)+1):(tail aux)
       else (x,1):aux


primeFactorsM :: Int -> [(Int,Int)]
primeFactorsM 1 = []
primeFactorsM n = (encodeP . primeFactors) n

-- to do it on the go have to mix encodeP along with primeFactors
-- I dont see any advantage



{-Problem 37

(**) Calculate Euler's totient function phi(m) (improved). 

phi(m) = (p1 - 1) * p1 ** (m1 - 1) 
       + (p2 - 1) * p2 ** (m2 - 1) 
       + (p3 - 1) * p3 ** (m3 - 1) + ...

-}


imptotient :: Int -> Int
imptotient n =
    let cumProd :: [(Int,Int)]-> Int
        cumProd [] = 1
        cumProd (x:xs) = ( ((fst x)-1)*(fst x)^((snd x)-1) )*(cumProd xs)
    in cumProd (primeFactorsM n)-- facts


{-Problem 38

(*) Compare the two methods of calculating Euler's totient function.

Use the solutions of problems 34 and 37 to compare the
algorithms. Take the number of reductions as a measure for
efficiency. Try to calculate phi(10090) as an example. -}


-- Difference is BIG on BIG numbers


{-Problem 39

(*) A list of prime numbers.

Given a range of integers by its lower and upper limit, construct a
list of all prime numbers in that range.-}

-- so usefull now the nextPrime function
primesR :: Int -> Int -> [Int]
primesR m n =
    if m <= n then
        if isPrime m then m:(primesR (nextPrime m) n)
        else primesR (nextPrime m) n
    else []
        

{-Problem 40

(**) Goldbach's conjecture.

Goldbach's conjecture says that every positive even number greater
than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is
one of the most famous facts in number theory that has not been proved
to be correct in the general case. It has been numerically confirmed
up to very large numbers (much larger than we can go with our Prolog
system). Write a predicate to find the two prime numbers that sum up
to a given even integer. -}


goldbach :: Int -> (Int,Int)
--goldbach 1 = (1,0)
goldbach 2 = (1,1)
goldbach n = 
    if (mod n 2)==0 then  
        let aux x = if isPrime (n-x) then (x, n-x)
                    else (aux . nextPrime ) x
        in aux 2 -- yep, 2 is prime (we checked it with isPrime)
    else (0,0)

{-Problem 41

(**) Given a range of integers by its lower and upper limit, print a
list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime
numbers, one of them is very small. Very rarely, the primes are both
bigger than say 50. Try to find out how many such cases there are in
the range 2..3000. -}

goldbachList :: Int -> Int -> Int-> [(Int,(Int,Int))]
goldbachList m n l = 
    if (mod m 2)/= 0 then goldbachList (m+1) n l
    else
        if m <= n then 
            let aux = goldbach m
            in if (fst aux)>l then (m, aux):(goldbachList (m+2) n l)
               else (goldbachList (m+2) n l)
        else []



