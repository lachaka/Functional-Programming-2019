main :: IO()
main = do
    print (inside 10 20 10)
    print (sumSquares 5 10)
    print (myFact 5)
    print (myFact' 5)
    print (myFib 10)
    print (myFib' 10)
    print (myGcd 10 3)

-- Task 01
inside :: Int -> Int -> Int -> Bool
inside a b x = (a <= x) && (x <= b)

-- Task 02
sumSquares :: Int -> Int -> Int
sumSquares x y = x ^ 2 + y ^ 2

-- Task 03 v.01
myFact :: Integer -> Integer
myFact 0 = 1                   
myFact n = n * myFact (n - 1)

-- Task 03 v.02
myFact' :: Integer -> Integer
myFact' n = myFactIter n 1
    where
        myFactIter 0 res = res
        myFactIter n res = myFactIter (n - 1) (res * n)

-- Task 04 v.01
myFib :: Int -> Int
myFib 0 = 1
myFib 1 = 1
myFib n = myFib (n - 1) + myFib (n - 2)

-- Task 04 v.02
myFib' :: Int -> Int
myFib' 0 = 1
myFib' n = myFibIter n 0 1
    where
        myFibIter 0 prev current = current
        myFibIter n prev current = myFibIter (n - 1) current (current + prev)

-- Task 05
myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)