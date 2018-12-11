{- Tristan Tapson
   tapsonte --
   Nov 01, 2018 -}

{- A3 - Polynomials
   deals with various computations regarding
   polynomials and integers using data types -}

module Polynomial where

data Poly = X Exp Coeff | Sum Poly Poly deriving Show

type Exp = Integer
type Coeff = Integer

-- powerOf returns x^y --
powerOf :: Integer -> Integer -> Integer
powerOf x y = x^y


-- powerOfPoly returns a polynomial evaluated at n; uses the powerOf auxilary function -- 
powerOfPoly :: Poly -> Integer -> Integer
powerOfPoly (X exp coeff) n = coeff * (powerOf n exp)


-- polyEval evaluates a polynomial p at value n and returns the result --
polyEval :: Poly -> Integer -> Integer
-- base case -- 
-- simply computes (coeff * (n)^x); uses the powerOfPoly auxilary function -- 
polyEval (X exp coeff) n = powerOfPoly (X exp coeff) n -- or = coeff * (getPow n exp) --
-- recursive step (on a polynomial sum of any size > than the base cases) --  
polyEval (Sum p1 p2) n = polyEval p1 n + polyEval p2 n


-- maxDegree returns the largest exponent found in the polynomial --
maxDegree :: Poly -> Integer
-- base case -- 
maxDegree (X exp coeff) = exp
-- recursive step --
-- check to see which part of Sum contains the higher exponent and do recursion on it -- 
maxDegree (Sum p1 p2) | maxDegree p1 >= maxDegree p2 = maxDegree p1 
                      | otherwise                    = maxDegree p2


{- polyCoeff returns the coefficient of the corresponding exponent, regardless if it is
   in standard form or not in standard form beforehand -}
polyCoeff :: Poly -> Integer -> Integer
-- base cases -- 
{- check if the exponent is equal to user input; if so then sum all coefficients with the same exponent,
   otherwise we don't sum the coefficeints -}
polyCoeff (X exp coeff) n | (exp == n) = coeff
                          | otherwise  = 0
-- recursive step (on a polynomial sum of any size > than the base cases) -- 
polyCoeff (Sum p1 p2) n = polyCoeff p1 n + polyCoeff p2 n


-- polyProduct returns the product (in Poly form) of two polynomials
polyProduct :: Poly -> Poly -> Poly
-- base cases (either have two polynomials, or a polynomial and a polynomial sum) --
polyProduct (X exp1 coeff1) (X exp2 coeff2) = (X (exp1 + exp2) (coeff1 * coeff2))
polyProduct (X exp1 coeff1) (Sum p1 p2)     =  Sum (polyProduct p1 (X exp1 coeff1)) (polyProduct p2 (X exp1 coeff1))
polyProduct (Sum p1 p2) (X exp1 coeff1)     =  Sum (polyProduct p1 (X exp1 coeff1)) (polyProduct p2 (X exp1 coeff1))

-- recursive step (on a polynomial sum of any size > than the base cases) --
polyProduct (Sum p1 p2) (Sum p3 p4)         = Sum (Sum (polyProduct p1 p3) (polyProduct p1 p4)) (Sum (polyProduct p2 p3)(polyProduct p2 p4))


-- polyDerive returns the derivative (in Poly form) of a polynomial --
polyDerive :: Poly -> Poly
-- base cases (either have a constant, or a single polynomial with an exponent > zero) --
polyDerive (X (0) coeff) = (X (0) (coeff * (0)))
polyDerive (X exp coeff) = (X (exp-1) (coeff * exp))
-- recursive step (on a polynomial sum of any size > than the base cases) -- 
polyDerive (Sum p1 p2)   = Sum (polyDerive p1) (polyDerive p2)


{- 

Function: polyEval
Test Case Number: 1
Input: (X 0 3) 2
Expected Output: 3 
Actual Output: 3

Function: polyEval
Test Case Number: 2
Input: (Sum (X 1 1) (X 0 3)) (-2)
Expected Output: 1
Actual Output: 1

Function: polyEval
Test Case Number: 3
Input: (Sum (X 2 1) (Sum (X 1 (-4)) (X 0 3))) 2
Expected Output: -1
Actual Output: -1

Function: maxDegree
Test Case Number: 1
Input: (X 0 3) 
Expected Output: 0
Actual Output: 0

Function: maxDegree
Test Case Number: 2
Input: (Sum (X 1 1) (X 0 3)) 
Expected Output: 1
Actual Output: 1

Function: maxDegree
Test Case Number: 3
Input: (Sum (X 2 1) (Sum (X 1 (-4)) (X 0 3))) 
Expected Output: 2
Actual Output: 2

Function: maxDegree
Test Case Number: 4
Input: (Sum (X 1 1) (Sum (X 2 3) (X 2 (-3))))
Expected Output: 2
Actual Output: 2

Function: polyCoeff
Test Case Number: 1
Input: (X 0 3) 2
Expected Output: 0
Actual Output: 0

Function: polyCoeff
Test Case Number: 2
Input: (Sum (X 1 1) (X 0 3)) 0
Expected Output: 3
Actual Output: 3

Function: polyCoeff
Test Case Number: 3
Input: (Sum (X 2 1) (Sum (X 1 (-4)) (X 0 3))) 1
Expected Output: -4
Actual Output: -4

Function: polyCoeff
Test Case Number: 4
Input: (Sum (X 1 1) (Sum (X 2 3) (X 2 (-3)))) 2
Expected Output: 0
Actual Output: 0

Function: polyProduct
Test Case Number: 1
Input: (X 1 3) (X 2 4)
Expected Output: X 3 12
Actual Output: X 3 12

Function: polyProduct
Test Case Number: 2
Input: (Sum (X 1 1) (X 0 3)) (X 2 4)
Expected Output: Sum (X 3 4) (X 2 12)
Actual Output: Sum (X 3 4) (X 2 12)

Function: polyProduct
Test Case Number: 3
Input: (X 5 (-6)) (Sum (X 2 3) (X 3 4))
Expected Output: Sum (X 7 (-18)) (X 8 (-24))
Actual Output: Sum (X 7 (-18)) (X 8 (-24))

Function: polyProduct
Test Case Number: 4
Input: (Sum (X 2 1) (Sum (X 1 (-4)) (X 0 3))) (Sum (X 3 4) (X 2 12))
Expected Output: Sum (Sum (X 5 4) (X 4 12)) (Sum (Sum (X 4 (-16)) (X 3 12)) (Sum (X 3 (-48)) (X 2 36)))
Actual Output: Sum (Sum (X 5 4) (X 4 12)) (Sum (Sum (X 4 (-16)) (X 3 12)) (Sum (X 3 (-48)) (X 2 36)))

Function: polyDerive
Test Case Number: 1
Input: (X 0 3) 
Expected Output: X 0 0
Actual Output: X 0 0

Function: polyDerive
Test Case Number: 2
Input: (Sum (X 1 (-4)) (X 2 3))
Expected Output: Sum (X 0 (-4)) (X 1 6)
Actual Output: Sum (X 0 (-4)) (X 1 6)

Function: polyDerive
Test Case Number: 3
Input: (Sum (X 6 5) (Sum (X 1 (-4)) (X 0 3)))
Expected Output: Sum (X 5 30) (Sum (X 0 (-4)) (X 0 0))
Actual Output:  Sum (X 5 30) (Sum (X 0 (-4)) (X 0 0))

Function: polyDerive
Test Case Number: 4
Input: (Sum (X 1 1) (Sum (X 2 3) (X 2 (-3))))
Expected Output: Sum (X 0 1) (Sum (X 1 6) (X 1 (-6)))
Actual Output:  Sum (X 0 1) (Sum (X 1 6) (X 1 (-6)))

-}