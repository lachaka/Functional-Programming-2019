main :: IO()
main = do
    print (myMin 5 10)
    print (countDigits 1001)
    print (countDigits' 1001)
    print (sumDigits' 123)
    print (isAscending 11223)
    print (isPrime 97)
    print (isPerfect 28)

-- Task 01
myMin :: Int -> Int -> Int
myMin a b
    | a > b = b
    | otherwise = a

-- Task 02 v.01
countDigits :: Int -> Int
countDigits n
 | n < 10 = 1
 | otherwise =  1 + countDigits (div n 10)

-- Task 02 v.02
countDigits' :: Int -> Int
countDigits' n = myCount n 0
    where 
        myCount 0 count = count
        myCount n count = myCount (div n 10) (count + 1)

-- Task 03 v.01
sumDigits :: Int -> Int
sumDigits n
 | n < 10 = n
 | otherwise = (mod n 10) + sumDigits (div n 10)

-- Task 03 v.02
sumDigits' :: Int -> Int
sumDigits' n = mySum n 0
    where 
        mySum 0 sum = sum
        mySum n sum = mySum (div n 10) (sum + mod n 10)

-- Task 04
isAscending :: Int -> Bool
isAscending n = myPredicate n (n `mod` 10)
 where 
  myPredicate n prev
   | n < 10 = True
   | (n `mod` 10) > prev = False
   | otherwise = myPredicate (n `div` 10) (n `mod` 10)

-- Task 05
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
 | length[x | x <- [2..n-1], (n `mod` x) == 0] > 0
 | otherwise = True     

-- Task 06
isPerfect :: Int -> Bool
isPerfect n
 | n < 6 = False
 | otherwise = sum[x | x <- [1..n-1], n `mod` x == 0] == n