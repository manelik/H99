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
                   let aux = mod m n
                   in if aux == 0 then False
                      else recDiv (n+1) m
    in recDiv 2 x


{-Problem 32

(**) Determine the greatest common divisor of two positive integer
numbers. Use Euclid's algorithm. -}

myGCD :: Int -> Int -> Int
myGCD x y =
    if (mod x y)==0 then y
    else myGCD y ((div x y)*y)


 