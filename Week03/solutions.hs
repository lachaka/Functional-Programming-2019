main :: IO()
main = do
 print (primesInRange 1 10)
 print (squares 1 10 2)
 print (reverseNumber 1223)
 print (countOccurences 1023124241431 2)
 print (getVolume [(1, 2), (3, 4)])
 
-- Task 01 
isPrime :: Int -> Bool
isPrime n = length[x | x <- [1..n], n `mod` x == 0] == 2
 
primesInRange :: Int -> Int -> [Int]
primesInRange a b = [x | x <- [a..b], isPrime x]
 
-- Task 02
squares :: Int -> Int -> Int -> [(Int, Int)]
squares a b step = [(x, x^2) | x <- [a, a + step..b]]

-- Task 03
reverseNumber :: Integer -> Integer
reverseNumber n = myReverse n 0
 where
  myReverse n num
   | n == 0 = num
   | otherwise = myReverse (n `div` 10) (num * 10 + (n `mod` 10))

-- Task 04
countOccurences :: Integer -> Integer -> Int
countOccurences num digit = help num digit 0
 where
  help num digit count
   | num == 0 = count
   | (num `mod` 10 == digit) = help (num `div` 10) digit (count + 1)
   | otherwise = help (num `div` 10) digit count

-- Task 05
type Cylinder = (Double, Double)
getVolume :: [Cylinder] -> [Double]
getVolume cylinders = [(3.14 * r * r * h) | (r, h) <- cylinders]