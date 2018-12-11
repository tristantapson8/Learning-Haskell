{- Bonus Part for A1 - Complex Numbers
   cubicRealSolution returns the x1 root of a cubic equation 
   ax^3 + bx^2 + cx +d for both real and complex numbers -}

import Data.Complex

-- cubicQ --
cubicQ :: Float -> Float -> Float -> Float
cubicQ a b c = ((3 * a * c) - (b**2)) / (9 * (a**2))

-- cubicR -- 
cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = ((9*a*b*c) - (27*(a**2)*d) - (2*(b**3))) / (54 * (a**3))

-- cubicS -- 
cubicS :: Float -> Float -> Float
cubicS q r = getCubicRoot((r + (sqrt((q**3) + (r**2)))))

-- cubicT --
cubicT :: Float -> Float -> Float
cubicT q r = getCubicRoot((r - (sqrt((q**3) + (r**2)))))

{- getRealPart gets the real part of a complex number just used for testing,
   not called within any other functions -}
getRealPart :: Complex Float -> Float
getRealPart num = let n = num in realPart(sqrt(-n))

{- getImagPart gets the imaginary part of a complex number just used for testing,
   not called within any other functions -}
getImagPart :: Complex Float -> Float
getImagPart num = let n = num in imagPart(sqrt(-n))

-- computes the imaginary part of sqrt(q^3 + r^2) if < 0
computeQR_ImagPart :: Float -> Float -> Float
computeQR_ImagPart q r = let num = (q**3) + (r**2) 
                         in if num < 0
                            then (imagPart(sqrtComplex(toComplexFloat(num))))
                            else num

{- computes the real part of sqrt(q^3 + r^2) if < 0
  note: the real part of a sqrt(-number) is always 0 -}
computeQR_RealPart :: Float -> Float -> Float
computeQR_RealPart q r = let num = (q**3) + (r**2) 
                         in if num < 0
                            then 0
                            else num

-- cubicComlpexS is cubicS inclusive of complex numbers; conversion to complex done here --
cubicComplexS :: Float -> Float -> Complex Float
cubicComplexS q r = cbrtComplex((computeQR_RealPart q r :+ (computeQR_ImagPart q r)) + (r :+ 0))

-- cubicComplexT is cubicT inclusive of complex numbers; conversion to complex done here --
cubicComplexT :: Float -> Float -> Complex Float
cubicComplexT q r = cbrtComplex((computeQR_RealPart q r :+ (computeQR_ImagPart q r)) - (r :+ 0))

{- toComplexFloat converts a float to a complex float; used in computeQR_ImagPart
   the type conversions may look a bit off, however testing proves that it works
   within computeQR_ImagPart (for example, computeQR_ImagPart (-4) 0 means that 
   we are trying to compute the square root of -64, and it correctly returns 8.0,
   which is the imaginary part of -64) -}
toComplexFloat :: Float -> Complex Float
toComplexFloat num = fromInteger(toInteger(toInt(num)))

{- fromFloatToComplexForm converts a float to its complex form
   (ex: passing in x will return a complex float in the form (x :+ 0.0)) -}
fromFloatToComplexForm :: Float -> Complex Float
fromFloatToComplexForm num = (num :+ 0.0) 

{- getCubicRoot gets the cubic root of a number of type float
   done this way to avoid the NaN error for cubic root of negative numbers -}
getCubicRoot :: Float -> Float 
getCubicRoot num = if num < 0 
                   then -((-num) ** (1/3)) 
                   else num ** (1/3) 

{- cbrtComplex computes the cubed root of a complex number 
   note: we only need the real part of this calculation as we are computing 
   the real solution of a cubic equation with complex numbers, hence why
   the only check is done on the real part. If we were returning the 
   solution with an imaginary part, then the conditional(s) would have
   to be changed to accurately reflect this -}
cbrtComplex :: Complex Float -> Complex Float
cbrtComplex num = if realPart(num) < 0 || imagPart(num) < 0
                  then -((-num) ** (1/3)) 
                  else num ** (1/3) 

-- sqrtComplex computes the square root of a complex number -- 
sqrtComplex :: Complex Float -> Complex Float
sqrtComplex num = num ** (1/2)

-- toInt converts a Float to an Int -- 
toInt :: Float -> Int
toInt num = round num

-- addComplex adds two complex numbers together -- 
addComplex :: Complex Float -> Complex Float -> Complex Float
addComplex a b = (a + b)

{- cubicRealSolution computes a value for root x1 given a cubic equation
   ax^3 + bx^2 + cx +d, considering complex numbers -}
cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d = if ((cubicQ a b c)**3 + (cubicR a b c d)**2) < 0
                            -- compute x1 accounting for complex numbers --
                            then let q = cubicQ a b c
                                     r = cubicR a b c d
                                     s = cubicComplexS q r
                                     t = cubicComplexT q r
                                     u = (b / (3*a))
                                     in  realPart((addComplex s t) - fromFloatToComplexForm(u))
                            -- compute x1 disregarding complex numbers -- 
                            else let q = cubicQ a b c
                                     r = cubicR a b c d
                                     s = cubicS q r
                                     t = cubicT q r
                                     in  s + t - (b / (3*a))
