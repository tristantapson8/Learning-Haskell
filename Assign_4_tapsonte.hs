{- Tristan Tapson
   tapsonte --
   Nov 20, 2018 -}

{- A4 - Polynomials & IO
   deals with various computations regarding
   polynomials and integers using data types and IO -}

module PolynomialList where

-- install quick check -- 
import Test.QuickCheck

data Poly = X Exp Coeff | Sum Poly Poly deriving Show

type Exp = Integer
type Coeff = Integer

-- strToList takes a string and converts it into a list of integers using map --
-- auxilary function to getPolyList --
strToList :: String -> [Integer]
strToList str = map read $ words str :: [Integer]

-- getPolyList reads a file (specified as argument) and returns the content as an IO Integer list --
getPolyList :: FilePath -> IO [Integer]
getPolyList path = do{
   content <- readFile path;
   return  (strToList content);
   }

-- powerOf returns x^y --
powerOf :: Integer -> Integer -> Integer
powerOf x y = x^y

{- chopTail returns a list with the last element removed --
-- auxilary function to polyListValue -}
chopTail :: [Integer] -> [Integer]
chopTail (x:xs) = reverse(tail (reverse (x:xs)))

-- polyListValue evaluates a list of polynomials at n --
polyListValue :: [Integer] -> Integer -> Integer
-- base case; empty list --
polyListValue [] n = 0
{- recursive step; multiply n with (list index)-1 ^ list element, and sum the the result 
   recusively done on last element to first element ordering -}
polyListValue (x:xs) n = last (x:xs) * (n ^ (toInteger(length(x:xs))-1)) + polyListValue (chopTail (x:xs)) n

{- chopTailZeroes returns a list with all zeroes removed from the start of the list --
   necessary as a polynomial list such as [0,4,0] is valid, but will return the wrong 
   result for the degree if list length - 1 is the degree of the list. Reversing the 
   original list will remove the zeroes off of the tail, which is done in polyListDegree
   auxilary function to polyListDegree -}
chopZeroes :: [Integer] -> [Integer]
-- base case; empty list --
chopZeroes [] = []
--recursive step; remove the head of the list until a non-zero element is found --
chopZeroes (x:xs) = if x == 0 then chopZeroes xs else (x:xs)

-- polyListDegree returns the degree of the polynomial list -- 
polyListDegree :: [Integer] -> Integer
-- base cases; either an empty list, or a polynomial list of type integer --
polyListDegree [] = undefined
polyListDegree (x:xs) = toInteger(length(chopZeroes(reverse(x:xs))))-1

{- computeDerivative returns a list of the derivatives of the polynomial sum coefficients -- 
   auxilary function to polyListDerive -}
computeDerivative :: [Integer] -> [Integer]
-- base case; empty list --
computeDerivative [] = []
{- recursive step; multiply (list index)-1 by list element, and concatenate the result to a new list 
   recusively done on last element to first element ordering -}
computeDerivative (x:xs) = computeDerivative (chopTail (x:xs)) ++ [last(x:xs) * (toInteger(length(x:xs))-1)]

-- polyListDerive returns a list of the derivatives of the polynomial sum coefficients -- 
polyListDeriv :: [Integer] -> [Integer]
-- base cases; empty list, or a list with one element --
polyListDeriv [] = []
polyListDeriv [xs] = []
{- recursive step; use computeDerivative to return a list of derived coefficients 
   the head of the list is removed to 'shift' coefficients, to signify the decrease of a
   degree due to deriving -}
polyListDeriv (x:xs) = tail (computeDerivative ((x:xs)))

--polyListSum returns a list of the summation of the polynomial coefficients in list1 & list2 --
polyListSum :: [Integer] -> [Integer] -> [Integer]
-- base cases; either two empty lists, or a list with an empty list --
polyListSum [] [] = []
polyListSum (x:xs) [] = [x] ++ polyListSum xs []
polyListSum [] (y:ys) = [y] ++ polyListSum [] ys
-- recursive step; add polynomials with the same coefficient together and concatenate the result into a new list --
polyListSum (x:xs) (y:ys) = [(x+y)] ++ polyListSum xs ys

-- getIndex returns the index at which an element is found in a list
-- auxilary function to cartProd2 --
getIndex :: Integer -> [Integer] -> Integer
-- base case; empty list --
getIndex n [] = 0
-- recursive step; add 1 to index count until element is found --
getIndex n (x:xs) = if (n == x) then 0 else 1 + getIndex n xs

{- cartProd returns a list of the cartesian product of two polynomial lists, in the form:
   [(answer, index)], where answer is the result of the cartesian product, and index is the
   sum of the indexes of the values used for the answer (used to track the exponent the coefficient belongs to)
   auxilary function to polyListProd -}
cartProd :: [Integer] -> [Integer] -> [(Integer,Integer)]
cartProd xs [] = []
cartProd [] ys = []
cartProd (x:xs) ys =  map (\y -> ((x*y), (getIndex x xs - getIndex y ys) + toInteger(length(ys)-1))) ys ++ cartProd xs ys

{- sumCoeffs returns the value of all matching index coefficients from a list in the form:
   [(answer, index)], where answer is the result of the cartesian product, and index is the
   sum of the indexes of the values used for the answer; n is the specified index 
   auxilary function to polyListProd -}
sumCoeffs:: Integer -> [(Integer, Integer)] -> Integer
sumCoeffs n [] = 0
sumCoeffs n ((a,b):xs) = if (b == n) then a + sumCoeffs n xs else sumCoeffs n xs

{- listProdReversed returns a list of the product of the polynomial coefficients in list1 & list2 in reversed order;
   where n is the start index (0 in all cases) and m is the end index, recursing through a list of size m-n
   auxilary function to polyListProd -}
listProdReversed :: Integer -> Integer -> [(Integer, Integer)]  -> [Integer]
listProdReversed n m ((a,b):xs) = if n < m then [sumCoeffs n ((a,b):xs)] ++ listProdReversed (n+1) m ((a,b):xs) else []

-- polyListProdF returns a list of the product of the polynomial coefficients in list1 & list2 --
polyListProd :: [Integer] -> [Integer] -> [Integer]
-- case (1); multiplying two non-polynomial sum representations --
polyListProd [] [] = []
-- cases (2 & 3); multiplying a polynomial list by anything thats not a polynomial sum representation --
polyListProd (x:xs) [] = []
polyListProd [] (y:ys) = []
{- case (4); use the auxilary functions to return the product of two polynomial lists
   recursion done in auxilary functions -} 
polyListProd (x:xs) (y:ys) = reverse(listProdReversed 0 max (cartProd (x:xs) (y:ys))) 
                             where max = toInteger(length(x:xs) + length(y:ys))-1

{- polyList2Poly returns a polynomial of type poly from a list of integers in reversed order --
   auxilary function to polyListToPoly -}
polyList2Poly :: [Integer] -> Poly
{- base case; list with one element is a poly in the form X exp coeff
		    ; note: empty list is handled in polyListToPoly as this is an auxilary function -}
polyList2Poly [] = undefined
polyList2Poly [xs] = X 0 xs
-- recursive step; break down list until in the form X exp coeff
polyList2Poly (x:xs) = Sum (X exp x) (polyList2Poly xs) where exp = toInteger(length(x:xs)) - 1

-- polyListToPoly returns a polynomial of type poly from a list of integers --
polyListToPoly :: [Integer] -> Poly
-- base cases; either empty list, or a list with one poly element in the form X exp coeff
polyListToPoly [] = undefined
polyListToPoly [xs] = X 0 xs
-- recursive step; reverse the results from polyList2Poly
polyListToPoly (x:xs) = polyList2Poly(reverse (x:xs))

{- replaceAtN returns a new list where the old value at a specified index n is replaced with newValue;
   auxilary function to modifyByExp -}
replaceAtN :: Integer -> Integer -> [Integer] -> [Integer]
replaceAtN _ _ [] = []
replaceAtN n newValue (x:xs)
  | n == 0 = newValue:xs
  | otherwise = x:replaceAtN (n-1) newValue xs

{- maxDegree returns the largest exponent found in a polynomial 
   auxilary function for createList -}
maxDegree :: Poly -> Integer
-- base case; singular polynomial representation -- 
maxDegree (X exp coeff) = exp
{- recursive step; --
   check to see which part of Sum contains the higher exponent and do recursion on it -}
maxDegree (Sum p1 p2) | maxDegree p1 >= maxDegree p2 = maxDegree p1 
                      | otherwise                    = maxDegree p2

{- createList returns a list containing all zeroes (0) using replicate;
   its length is the max degree of the polynomial argument -}
createList :: Poly -> [Integer]
createList p1 = replicate (fromIntegral(maxDegree p1)+1) 0


{- findCE returns a list in the form [(coeff,exp)], used to track at what index value 
   the coefficient should belong to in the integer list representation of the polynomial;
   auxilary function to polyToPolyList -}
findCE :: Poly -> [(Integer, Integer)]
-- base case; singular polynomial representation -- 
findCE (X exp coeff) = [(coeff, exp)]
-- recursive step; break down polynomial until in the form X exp coeff --
findCE (Sum p1 p2) = findCE p2 ++ findCE p1

{- modifyByCE uses the list from findCE (correlation of coeff exp), and the zero list,
   and returns the sum of coefficients at their corresponding exp index in the form of 
   a polynomial integer list
   auxilary function to polyToPolyList -}
modifyByCE :: [(Integer,Integer)] -> [Integer] -> [Integer]
-- base case; empty list, no more modifications neeeded --
modifyByCE [] ys = ys
-- recursive step; update the coefficient value at correspoding exp index in the zero list --
modifyByCE ((coeff,exp):xs) ys =  modifyByCE xs (replaceAtN exp (coeff + ys !! fromIntegral (exp)) ys) 

--polyToPolyList returns a list of integers from a polynomial of type poly -- 
polyToPolyList :: Poly -> [Integer]
polyToPolyList p1 = modifyByCE (findCE p1) (createList p1) 

-- quickCheck functions -- 
hornersMethod :: (Num a) => a -> [a] -> a
hornersMethod n = foldr (\x y -> x + y*n) 0

{- prop_polyListValue checks if polyListValue is equal to the result computed via horners method on 
   a list evaluated at n -}
prop_polyListValue :: [Integer] -> Integer -> Bool
prop_polyListValue xs n =  polyListValue xs n == hornersMethod n xs

{- 

Function: getPolyList
Test Case Number: 1
Input: "Desktop/input.txt"
Expected Output: [1,2,-3,4,5]
Actual Output: [1,2,-3,4,5]

Function: getPolyList
Test Case Number: 2
Input: "Desktop/input.txt"
Expected Output: []
Actual Output: []

Function: getPolyList
Test Case Number: 3
Input: "Desktop/input.txt"
Expected Output: [1]
Actual Output: [1]

Function: polyListValue
Test Case Number: 1
Input: [1 2 3] 2
Expected Output: 17
Actual Output: 17

Function: polyListValue
Test Case Number: 2
Input: [-2, 0 , 3] 9
Expected Output: 241
Actual Output: 241

Function: polyListValue
Test Case Number: 3
Input: [] 5
Expected Output: 0
Actual Output: 0

Function: polyListDegree
Test Case Number: 1
Input: [1,0,-3,0]
Expected Output: 2
Actual Output: 2

Function: polyListDegree
Test Case Number: 2
Input: [0,0,-3,4,5]
Expected Output: 4
Actual Output: 4

Function: polyListDegree
Test Case Number: 3
Input: [0,0,1,0,0]
Expected Output: 2
Actual Output: 2

Function: polyListDegree
Test Case Number: 4
Input: [2]
Expected Output: 0
Actual Output: 0

Function: polyListDegree
Test Case Number: 5
Input: []
Expected Output: ***Exception: Prelude.undefined
Actual Output: ***Exception: Prelude.undefined

Function: polyListDeriv
Test Case Number: 1
Input: [1,-2,3,0]
Expected Output: [-2,6,0]
Actual Output: [-2,6,0]

Function: polyListDeriv
Test Case Number: 2
Input: [5]
Expected Output: []
Actual Output: []

Function: polyListDeriv
Test Case Number: 3
Input: []
Expected Output: []
Actual Output: []

Function: polyListSum
Test Case Number: 1
Input: [1,-2,3] [4,5]
Expected Output: [5,3,3]
Actual Output: [5,3,3]

Function: polyListSum
Test Case Number: 2
Input: [1,-2,3] []
Expected Output: [1,-2,3]
Actual Output: [1,-2,3]

Function: polyListSum
Test Case Number: 3
Input: [] [4,5]
Expected Output: [4,5]
Actual Output: [4,5]

Function: polyListSum
Test Case Number: 4
Input: [] []
Expected Output: []
Actual Output: []

Function: polyListProd
Test Case Number: 5
Input:  [1,2] [0,5,0,-6,0]
Expected Output: [1,7,0,-6,0]
Actual Output: [1,7,0,-6,0]

Function: polyListProd
Test Case Number: 1
Input: [1,-2,3] [4,5]
Expected Output: [4,-3,2,15]
Actual Output: [4,-3,2,15]

Function: polyListProd
Test Case Number: 2
Input: [1,-2,3] []
Expected Output: []
Actual Output: []

Function: polyListProd
Test Case Number: 3
Input: [] [4,5]
Expected Output: []
Actual Output: []

Function: polyListProd
Test Case Number: 4
Input: [] []
Expected Output: []
Actual Output: []

Function: polyListProd
Test Case Number: 5
Input: [1,2] [0,5,0,-6,0]
Expected Output: [0,5,10,-6,-12,0]
Actual Output: [0,5,10,-6,-12,0]

Function: polyListToPoly
Test Case Number: 1
Input: [1,-2,3]
Expected Output: Sum (X 2 3) (Sum (X 1 (-2)) (X 0 1))
Actual Output: Sum (X 2 3) (Sum (X 1 (-2)) (X 0 1))

Function: polyListToPoly
Test Case Number: 2
Input: [0,4,0]
Expected Output: Sum (X 2 0) (Sum (X 1 4) (X 0 0))
Actual Output: Sum (X 2 3) (Sum (X 1 (-2)) (X 0 1))

Function: polyListToPoly
Test Case Number: 3
Input: [5,6]
Expected Output: Sum (X 1 6) (X 0 5)
Actual Output: Sum (X 1 6) (X 0 5)

Function: polyListToPoly
Test Case Number: 4
Input: [-7]
Expected Output: X 0 (-7)
Actual Output: X 0 (-7)

Function: polyListToPoly
Test Case Number: 5
Input: []
Expected Output: ***Exception: Prelude.undefined
Actual Output: ***Exception: Prelude.undefined

Function: polyToPolyList
Test Case Number: 1
Input: (X 0 (-8))
Expected Output: [-8]
Actual Output: [-8]

Function: polyToPolyList
Test Case Number: 2
Input: (X 2 1)
Expected Output: [0,0,1]
Actual Output: [0,0,1]

Function: polyToPolyList
Test Case Number: 3
Input: (Sum (X 1 1) (X 0 3))
Expected Output: [3,1]
Actual Output: [3,1]

Function: polyToPolyList
Test Case Number: 4
Input: Sum (X 2 1) (Sum (X 5 (-4)) (X 0 3))
Expected Output: [3,0,1,0,0,-4]
Actual Output: [3,0,1,0,0,-4]

Function: polyToPolyList
Test Case Number: 5
Input: (Sum (X 5 (-3) (X 3 2))
Expected Output: [0,0,0,2,0,3]
Actual Output: [0,0,0,2,0,3]

Function: polyToPolyList
Test Case Number: 6
Input: (Sum (X 5 3) (X 5 7))
Expected Output: [0,0,0,2,0,3]
Actual Output: [0,0,0,0,0,10]

Function: polyListValue
Property: prop_polyListValue
Actual Test Result: Pass

-}