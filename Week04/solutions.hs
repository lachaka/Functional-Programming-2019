main :: IO()
main = do
 print (getPrice p1)
 print (getTotal shop)
 print (buy shop "Fish Fingers" 1)
 print (getNeeded shop 2)

-- Task 01
type Name = String
type Quantity = Int
type Price = Double

type Product = (Name, Quantity, Price)
type Shop = [Product]

p1 :: Product 
p1 = ("Fish Fingers", 2, 1.5)

p2 :: Product
p2 = ("Orange Jelly", 1, 3.0)

p3 :: Product 
p3 = ("Dry Sherry, 1lt", 3, 2.5)

shop :: Shop
shop = [p1, p2, p3]

-- Task 02
getPrice :: Product -> Double
getPrice ( _, quantity, price) = fromIntegral(quantity) * price

-- Task 03
getTotal :: Shop -> Double
getTotal []     = 0
getTotal (x:xs) = getPrice x + getTotal xs

-- Task 04
buy :: Shop -> Name -> Quantity -> Shop
buy shop name quantity = 
    [p | p@(n, _, _) <- shop, name /= n]
    ++ [(n, q - quantity, p) | (n, q, p) <- shop, 
                                           n == name, 
                                           q > quantity]
                                           
-- Task 05
getNeeded :: Shop -> Quantity -> [Product]
getNeeded shop quantity = [p | p@(_, q, _) <- shop, q <= quantity]