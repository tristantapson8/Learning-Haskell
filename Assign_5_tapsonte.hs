{- Tristan Tapson
   tapsonte
   Dec 02, 2018 -}

{- A5 - Integrals
   deals with various computations regarding
   integrals and roots of a function -}

module Fucntion where

{- check signs returns true if two input numbers are opposite signs, false otherwise
   auxilary function to getRoot -}
checkSigns :: Double -> Double -> Bool
checkSigns a b = if a * b < 0 then True else False 

{- getRootCalculation returns the root of a function f, on interval [a,b]
   within a tolerance value epsilon
   auxilary function to getRoot -}
getRootCalculation :: (Double -> Double) -> Double -> Double -> Double -> Double
getRootCalculation f a b epsilon
                       {- return the point as a solution to the root if any of the
                          evaluated values return 0 -}
                       | f(a) == 0 = a
                       | f(b) == 0 = b
                       | f(m) == 0 = m
                       {- return a if b-a is less than or equal to epsilon;
                          else use the midpoint as a new start or end interval value -}
                       | otherwise = if (b-a) <= epsilon then a
                       else
                          if (f(a) * f(m)) <= 0 then getRootCalculation f a m epsilon
                          else getRootCalculation f m b epsilon

                       -- midpoint of the interval [a,b] --
                       where m = ((a+b) / 2)

{- getRoot returns the root of a function f, on interval [a,b]
   within a tolerance value epsilon using the getRootCalculation function; if inputs a and b 
   are not of opposite signs, then return an error message instead -}
getRoot :: (Double -> Double) -> Double -> Double -> Double -> Double
getRoot f a b epsilon | checkSigns a b == False = error "signs not opposite"
                      | otherwise = getRootCalculation f a b epsilon

{- definiteIntegralCalculation returns the root of a function g, on interval [a,b]
   using the trapezoidal rule with n partitions
   auxilary function to definiteIntegral -}
definiteIntegralCalculation :: Double -> Double ->  Double -> (Double -> Double) -> Integer -> Integer -> Double
definiteIntegralCalculation a constA b g n constN =
    {- constN tracks how many times the trapezoidal rule needs to be recursed on; 
       if its value is zero (0), then the summation is complete -}
    if constN == 0
        then 0
        {- right side and left side of the trapezoidal rule;
           rhs: b-a / n, lhs: f(xi-1) + f(xi) / 2 -}
        else let rhs = (b-constA) / fromIntegral(n)
                 lhs = (g(a) + g(a+rhs)) / 2
             {- definite integral calcualted using the summation of the trapezoidal rule;
                update remaining summation count by decrementing constN -}
             in (lhs * rhs) + definiteIntegralCalculation (a+rhs) constA b g n (constN-1)

{- definiteIntegral returns the root of a function g on interval [a,b]
   using the trapezoidal rule with n partitions, using definiteIntegralCalculation -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = definiteIntegralCalculation a a b g n n    

{- 

   Function: getRoot
   Test Case Number: 1
   Input: f(x) = x^2
          getRoot f (-2) 2 0.01
   Expected Output: 0.0
   Actual Output: 0.0

   Function: getRoot
   Test Case Number: 2
   Input: f(x) = x^2
          getRoot f 1 2 0.01
   Expected Output: *** Exception: signs not opposite
   Actual Output: *** Exception: signs not opposite

   Function: getRoot
   Test Case Number: 3
   Input: f(x) = x^3 - 6
          getRoot f (-4) 5 0.001
   Expected Output: 1.81671142578125
   Actual Output: 1.81671142578125

   Function: getRoot
   Test Case Number: 4
   Input: f(x) = 4*x^5 - 2*x^3 + 3
          getRoot f (-1.7) 1.7 0.001
   Expected Output: -1.061669921875
   Actual Output: -1.061669921875

   Function: definiteIntegeral
   Test Case Number: 1
   Input: g(x) = x^2
          definiteIntegral 1 5 g 4
   Expected Output: 42.0
   Actual Output: 42.0

   Function: definiteIntegeral
   Test Case Number: 2
   Input: g(x) = x^3 + 5*x - 9
          definiteIntegral (-2) 7 g 8
   Expected Output: 641.98828125
   Actual Output: 641.98828125

   Function: definiteIntegeral
   Test Case Number: 3
   Input: g(x) = x^3 + 5*x - 9
          definiteIntegral (-2) 7 g 350
   Expected Output: 627.7574387755166
   Actual Output: 627.7574387755166

-}