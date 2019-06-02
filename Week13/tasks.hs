main :: IO()
main = do
 print (func (\x -> x + 1) [1,2,3,4] $ 3)
 print (func' (\x -> x + 1) [1,2,3,4] $ 3)
 print (boundUp (\x -> x + 1) 10 $ 1)
 print (boundUp (\x -> x + 1) 10 $ 10)
 print (getOddCompositionValue [(\x -> x + 1), (\x -> x * 2),(\x -> x - 1), (\x ->x `div` 2)] $ 2)
 print (minDepthGreenNode tree)
 print (coldestCapital europe)
 print ((specialSum (5 -) [1..10]) (> 0))
 print ((specialSum (\x -> x + 1) [(-5)..5]) odd)

-- Task 01
func :: (Int -> Int) -> [Int] -> (Int -> Int)
func f lst x = sum (zipWith (\an n -> n * f(an * x)) lst [1..])

-- Task 02
func' :: (Int -> Int) -> [Int] -> (Int -> Int)
func' f lst = \x -> (sum (zipWith (\an n -> an * f(x ^ an)) lst [1..]))

-- Task 03
boundUp :: (Int -> Int) -> Int -> (Int -> Int)
boundUp f y = \x -> if (f x) > y then f x else y

-- Task 04
getOddCompositionValue :: [(Int -> Int)] -> (Int -> Int)
getOddCompositionValue lst = foldl1 (.) [y | (x, y) <- zip [1..] lst, odd x]

-- Task 05
data Color = Red | Green | Blue deriving (Read, Show, Eq)
data Tree = Empty | Node Color Tree Tree 

tree :: Tree
tree = Node Red (Node Blue Empty (Node Green Empty Empty))
                Empty


minDepthGreenNode :: Tree -> Int
minDepthGreenNode tree = minimum (findGreenNode tree 0)
 where 
  findGreenNode :: Tree -> Int -> [Int]
  findGreenNode Empty _                    = []
  findGreenNode (Node Green Empty Empty) n = [n] 
  findGreenNode (Node Green l r) n         =  n : findGreenNode l (n + 1) ++ findGreenNode r (n + 1) 
  findGreenNode (Node _ l r) n             = findGreenNode l (n + 1) ++ findGreenNode r (n + 1) 
  
  
-- Task 06  
type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature deriving (Read, Show)
data Country = Country Name Capital [City] deriving (Read, Show)

europe = [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16),
                                       (City "Plovdiv" 120 14),
                                       (City "Sofia" 420 13)]),
          (Country "Germany" "Berlin" [(City "Munchen" 200 15),
                                       (City "Berlin" 150 12),
                                       (City "Ulm" 210 15)]),
          (Country "France" "Paris" [(City "Paris" 180 15),
                                     (City "Nice" 0 14),
                                     (City "Lyon" 500 13)])]

coldestCapital :: [Country] -> Name
coldestCapital lst = fst $ foldl1 closer [(name, (temp cname cities)) | (Country name cname cities) <- lst]
 where 
  closer c1@(_, t1) c2@(_, t2) = if t1 <= t2 then c1 else c2
  temp cname cities = head [t | (City name _ t) <- cities, cname == name]  
  
-- Task 07
specialSum :: (Int ->Int) ->[Int] ->((Int ->Bool) -> Int)
specialSum f lst = (\x -> sum [y * y | y <- lst, x(f y) == True])
 