{- Tristan Tapson
   tapsonte --
   Oct 14, 2018 -}

{- A2 - Vector Space
   deals with various computations regarding
   3-dimensional (x,y,z) vectors -}

module VectorSpace where 

type Vector = (Double, Double, Double)

vecZero = (0,0,0) :: Vector

-- vecScalarProd returns the scalar product of a real number -- 
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd r (a, b, c) = (r*a, r*b, r*c)

-- vecSum returns the sum of vector addition between two vectors --
vecSum :: Vector -> Vector -> Vector
vecSum (a, b, c) (a', b', c') = (a + a', b + b', c + c')

{- vecSum returns the difference of vector subtraction between two vectors;
   an auxilary function used to help compute vecF -}
vecDiff :: Vector -> Vector -> Vector
vecDiff (a, b, c) (a', b', c') = (a - a', b - b', c - c')

-- vecMagnitude returns the magnitude of a vector --
vecMagnitude :: Vector -> Double
vecMagnitude (a ,b, c) = sqrt((a**2) + (b**2) + (c**2))

{- vecF returns a list of the distance between a vector  
   compared to all vectors in a list of vectors -} 
vecF :: Vector -> [Vector] -> [Double]
vecF (a, b, c) [] = []
vecF (a, b, c) (v:vs) =  [vecMagnitude(vecDiff(a, b, c)(i)) | i <- (v:vs)]

-- vecDot returns the dot product of two vectors --
vecDot :: Vector -> Vector -> Double
vecDot (a, b, c) (a', b', c') = (a * a') +  (b * b') + (c * c')

{- computeOrthogonal computes the dot product between all list elements recursively,
   and returns a list of doubles of those computed values;
   an auxilary function used to help compute vecOrthogonal -}
computeOrthogonal :: [Vector] -> [Double]
computeOrthogonal [] = []
computeOrthogonal (v:vs) = [vecDot v j | j <- vs] ++ computeOrthogonal vs  

{- checkOrthogonal checks through a list of doubles recursively and returns a boolean
   (true) if all elements in the list are zero or the list is empty, and false otherwise;
   an auxilary function used to help compute vecOrthogonal -}
checkOrthogonal :: [Double] -> Bool
-- if the list has been exhausted and no zero values are found, then the list is orthogonal --
checkOrthogonal []                 = True
-- if a non-zero value is found at the head of the list, it is not orthogonal --
-- otherwise continue to recurse through the remaining values of the list --
checkOrthogonal (v:vs) | 0.0 /= v  = False
                       | otherwise = checkOrthogonal vs

-- vecOrthogonal returns a boolean to check if a list of vectors are pairwise orthogonal --
vecOrthogonal :: [Vector] -> Bool
-- empty list returns true --
vecOrthogonal [] = True
-- otherwise check for orthogonality and return the result of the check --
vecOrthogonal (v:vs) = checkOrthogonal(computeOrthogonal(v:vs))

{- Function: vecScalarProd
   Test Case Number: 1
   Input: 2 (2,3,5)
   Expected Output: (4.0,6.0,10.0)
   Actual Output: (4.0,6.0,10.0)

   Function: vecScalarProd
   Test Case Number: 2
   Input: (-1) ((-2),4.0,6.0)
   Expected Output: (2.0,-4.0,-6.0)
   Actual Output: (2.0,-4.0,-6.0) 

   Function: vecScalarProd
   Test Case Number: 3
   Input: (-1.5) (7.3,8.6,(-2))
   Expected Output: (-10.95, -12.8999999999999,3.0)
   Actual Output: (-10.95, -12.8999999999999,3.0) 

   Function: vecSum
   Test Case Number: 1
   Input: (1,2,3) (4,5,6)
   Expected Output: (5.0,7.0,9.0)
   Actual Output: (5.0,7.0,9.0) 

   Function: vecSum
   Test Case Number: 2
   Input: (1,(-2.0),(-3)) (7.4,1.2,(-6))
   Expected Output: (8.4,-0.8,-9.0)
   Actual Output: (8.4,-0.8,-9.0) 

   Function: vecSum
   Test Case Number: 3
   Input: (0,1,-1) ((-0.5),0,0)
   Expected Output: (-0.5,1.0,-1.0)
   Actual Output: (-0.5,1.0,-1.0) 

   Function: vecMagnitude
   Test Case Number: 1
   Input: (0,1,2)
   Expected Output: 2.23606797749979
   Actual Output:  2.23606797749979 

   Function: vecMagnitude
   Test Case Number: 2
   Input: (-2,3.5,(-4.0))
   Expected Output: 5.678908345800274
   Actual Output: 5.678908345800274 

   Function: vecMagnitude
   Test Case Number: 3
   Input: (0,0,0.4)
   Expected Output: 0.4
   Actual Output: 0.4 

   Function: vecF
   Test Case Number: 1
   Input: (1,2,3) [(2,3,4),(4,5,6)]
   Expected Output: [1.73205088075688772, 5.196152422706632]
   Actual Output: [1.73205088075688772, 5.196152422706632] 

   Function: vecF
   Test Case Number: 2
   Input: (1,2,3) []
   Expected Output: []
   Actual Output: [] 

   Function: vecF
   Test Case Number: 3
   Input: (-4,-5,6) [(1,-2,-3)]
   Expected Output: [10.723805294763608]
   Actual Output: [10.723805294763608] 

   Function: vecOrthogonal
   Test Case Number: 1
   Input: [(1,2,3)]
   Expected Output: True
   Actual Output: True

   Function: vecOrthogonal
   Test Case Number: 2
   Input: []
   Expected Output: True
   Actual Output: True

   Function: vecOrthogonal
   Test Case Number: 3
   Input: [(1,-1,0),(1,1,0),(0,0,0)]
   Expected Output: True
   Actual Output: True

   Function: vecOrthogonal
   Test Case Number: 4
   Input: [(1,-1,0),(-1,1,0),(1,1,0)]
   Expected Output: False
   Actual Output: False -}



