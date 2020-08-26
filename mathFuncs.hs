c_G = 6.67430e-11
c_ME = 5.97237e24
c_MS = 1.9887e30

sumOfThreeCubes :: (Integral a) => a -> a -> a -> a
sumOfThreeCubes a b c = a * a * a + b * b * b + c * c * c

arithmeticSequenceSum :: (Fractional a) => a -> a -> a -> a
arithmeticSequenceSum a d n = ( (n / 2) * ( (2 * a) + ( (n - 1) * d ) ) ) 

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

permutation :: (Integral a) => a -> a -> a
permutation n r = (factorial n) `div` (factorial (n - r))

combination :: (Integral a) => a -> a -> a
combination n r = (permutation n r) `div` (factorial r)

dotProductCartesian :: (Num a) => (a,a,a) -> (a,a,a) -> a
dotProductCartesian (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

crossProductCartesian :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
crossProductCartesian (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

fibonacciNumRecursive :: (Integral a) => a -> a -> a -> a
fibonacciNumRecursive x y 1 = x
fibonacciNumRecursive x y 2 = y
fibonacciNumRecursive x y n = (fibonacciNumRecursive x y (n - 1) ) + (fibonacciNumRecursive x y (n - 2))

distancePointLine :: (Floating a) => (a, a) -> (a, a, a)-> a
distancePointLine (x,y) (a,b,c) = (abs (a * x + b * y + c)) / (hypot a b)
    where 
        hypot a b = sqrt (a^2 + b^2)
        


fibonacciNumIterative :: (Integral a) => a -> a -> a -> a
fibonacciNumIterative x y n
    | n <= 0 = 0
    | n == 1 = x
    | n == 2 = y
    | otherwise = fibonacciNumIterative y (x + y) (n - 1)

fibonacciNumList :: (Integral a) => [a] -> a -> [a]
fibonacciNumList list@(a:b:xs) n
    | n < 3 = list
    | otherwise = fibonacciNumList ( [a + b] ++ list ) (n - 1)

mGcd :: (Integral n) => n -> n -> n
mGcd a b = 
    if b == 0
        then abs a
        else mGcd b (a `mod` b)

pythagoreanTriplets :: (Integral a) => a -> [(a,a,a)]
pythagoreanTriplets n = [ (a,b,c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a^2 + b^2 == c^2, (mGcd a (mGcd b c)) == 1]

polarToCartesian :: (Floating a) => (a,a) -> (a,a)
polarToCartesian (r,theta) = (r * (cos theta), r * (sin theta))

cartesianToPolar :: (Floating a) => (a,a) -> (a,a)
cartesianToPolar (x,y) =
    let r = sqrt (x^2 + y^2)
        theta = atan ( y / x)
    in (r, theta)

cartesianDistance :: (Floating a) => (a,a) -> (a,a) -> a
cartesianDistance (x1,y1) (x2,y2) = sqrt ( (x2 - x1)^2 + (y2 - y1)^2 )

polarDistance :: (Floating a) => (a,a) -> (a,a) -> a
polarDistance (r1,t1) (r2,t2) = sqrt ( r1^2 + r2^2 - (2 * r1 * r2 * (cos (t2 - t1) ) ) )

multiplyComplex :: (Floating a) => (a,a) -> (a,a) -> (a,a)
multiplyComplex (r1,i1) (r2, i2) = (r1 * r2 - i1 * i2, r1 * i2 + r2 * i1)

{-
inMandelbrotSet :: (Floating a) => (a,a) -> a -> Int -> Bool
inMandelbrotSet (r,i) maxLength maxDepth = checkMandelbrot maxLength maxDepth 0 (0,0) (r,i)

checkMandelbrot :: (Floating a) => a -> Int -> Int -> (a,a) -> (a,a) -> Bool
checkMandelbrot maxLength maxDepth iter z c
    | iter > maxDepth = True
    | (checkDistance > maxLength) = False
    | otherwise = checkMandelbrot maxLength maxDepth (iter + 1) (fst newZ + fst c, snd newZ + snd c) c
    where
        checkDistance = sqrt ( (fst z)^2 + (snd z)^2)
        newZ = multiplyComplex z z
-}