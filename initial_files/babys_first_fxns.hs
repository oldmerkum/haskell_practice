import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else x*2
    
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase ::  String -> String
removeNonUppercase str = [ c | c <- str, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number SEVEN!!!"
lucky x = "Sorry, no so lucky..."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

max' :: (Ord a) => [a] -> a
max' [] = error "maximum of empty list"
max' [x] = x
max' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = max' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]