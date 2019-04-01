import Data.Char
import Data.List
import Data.Maybe

main :: IO()
main = do
 print (normalizeMessage "Attack London tomorrow at ten a.m.")
 print (encode ['A'..'Z'] 'A' 2)
 print (encrypt ['A'..'Z'] 5 (normalizeMessage "Attack London tomorrow at ten a.m."))
 print (decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR")
 print (crackall ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
 print (substring  "Curry" "Haskell Curry")
 print (crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR")

-- Task 01
normalizeMessage :: String -> String
normalizeMessage [] = []
normalizeMessage (x:xs)
 | x >= 'a' && x <= 'z' = chr(ord x - 32) : normalizeMessage xs
 | x >= 'A' && x <= 'Z' = x : normalizeMessage xs
 | x >= '0' && x <= '9' = error "digits not allowed"
 | otherwise = normalizeMessage xs
 
-- Task 02_1
encode :: String -> Char -> Int -> Char
encode alphabet ch offset
 | length [x | x <- alphabet, x == ch] == 0 = error "unsupported symbol" 
 | otherwise = alphabet!!(((fromJust (elemIndex ch alphabet)) + offset) `mod` length alphabet)
 
-- Task 02_2
encrypt  :: String -> Int -> String -> String
encrypt  _ _ [] = [] 
encrypt  alphabet offset (x:xs) = (encode alphabet x offset) : encrypt  alphabet offset xs

-- Task 02_3
decrypt :: String -> Int -> String -> String
decrypt  _ _ [] = [] 
decrypt alphabet offset (x:xs) = (encode alphabet x (-offset)) : decrypt  alphabet offset xs

-- Task 03_1
crackall :: String -> String -> [String]
crackall _ [] = [[]]
crackall alphabet encrypted = myCracker alphabet encrypted (length(alphabet) - 1)
 where
  myCracker _ _ 0 = []
  myCracker alphabet encrypted offset = decrypt alphabet offset encrypted : myCracker alphabet encrypted (offset - 1)
  
-- Task 03_2
substring  :: String -> String -> Bool
substring sub str = mySub sub str
 where 
  mySub _ [] = False
  mySub [] _ = True
  mySub (x:xs) (y:ys)
   | x == y = mySub xs ys
   | otherwise = mySub sub ys
   
-- Task 03_3
crackcandidates :: String -> [String] -> String -> [String]
crackcandidates alphabet commonwords encrypted = 
 [x | x <- crackall alphabet encrypted,
      y <- commonwords, substring y x == True]