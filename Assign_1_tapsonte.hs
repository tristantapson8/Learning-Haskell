{- A1 - Complex Numbers
   cubicRealSolution returns the x1 root of a cubic equation 
   for real numbers -}

-- cubicQ computes Q from parameters a,b,c --
cubicQ :: Float -> Float -> Float -> Float
cubicQ a b c = ((3 * a * c) - (b**2)) / (9 * (a**2))

-- cubicR computes R from parameters a,b,c,d --
cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = ((9*a*b*c) - (27*(a**2)*d) - (2*(b**3))) / (54 * (a**3))

-- cubicS computes S from parameters q,r -- 
cubicS :: Float -> Float -> Float
cubicS q r = getCubicRoot((r + (sqrt((q**3) + (r**2)))))

-- cubicT computes T from parameters q,r --
cubicT :: Float -> Float -> Float
cubicT q r = getCubicRoot((r - (sqrt((q**3) + (r**2)))))

{- getCubicRoot gets the cubic root of a number of type float
   done this way to avoid the NaN error for cubic root of negative numbers -}
getCubicRoot :: Float -> Float 
getCubicRoot num = if num < 0 then -((-num) ** (1/3)) else num ** (1/3)

{- cubicRealSolution computes a value for root x1 given a cubic equation
   ax^3 + bx^2 + cx +d -}
cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d = let q = cubicQ a b c
                                r = cubicR a b c d
                                s = cubicS q r
                                t = cubicT q r
                                in  s + t - (b / (3*a))