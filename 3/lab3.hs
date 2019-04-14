import Data.List
import Data.Char

f1a y = \y -> y - 2

f1 :: Num a => a -> a
f1 = \x -> x - 2

tmpa :: Fractional a => a -> a
tmpa = (3.4 /)

f2 :: Floating a => a -> a -> a
f2 = \x y -> sqrt x^2 + y^2

f3 = \x y z -> sqrt x^2 + y^2 + z^2

a1 = \x -> (2*x)

a1a = (2*)

a2 = \x -> (x*2)

a3 = \x -> 2^x

a4 = \x -> x^2

a5 = \x -> 2/x

a6 = \x -> x/3

a7 = \x -> 4-x

f7 = \x -> if x `mod` 2 == 0
                then True
                else False

f8 = \x -> let y = sqrt x 
            in 2 * y^3 * (y + 1)

f9 1 = \() -> 3
f9 _ = \() -> 0

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs  

w f x y = f x y

sum1 = sumWith (\ x -> x)
sumSqr1 = sumWith (\x -> x^2)
sumCube1 = sumWith (\x -> x^3)
sumAbs1 = sumWith (\x -> if x < 0 then (-1)*x else x)

f10 = sumWith (\x -> x^5) [1..15]

listLength = sumWith (\x -> 1)

sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

silnia x = loop x
  where {
    loop 1 = 1;
    loop x = x * loop (x - 1);
  }

--expApproxUpTo :: Int -> Double -> Double
expApproxUpTo2 n = loop 0 [1..n]
  where {
    loop acc [] = acc;
    loop acc (x:xs) = loop (acc + (1 / (silnia x))) xs;
  }


expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 ->  const 1
    1 ->  (\x -> 1 + (x / (fact 1)))
    2 ->  (\x -> (expApproxUpTo 1 x) + ((x^2) / (fact 2)))
    3 ->  (\x -> (expApproxUpTo 2 x) + ((x^3) / (fact 3)))
    4 ->  (\x -> (expApproxUpTo 3 x) + ((x^4) / (fact 4)))
    5 ->  (\x -> (expApproxUpTo 4 x) + ((x^5) / (fact 5)))
    6 ->  (\x -> (expApproxUpTo 5 x) + ((x^6) / (fact 6)))
    y ->  (\x -> (expApproxUpTo (y-1) x) + ((x^y) / (fact y)))
    where
        fact n = fromIntegral $ product [1..n]

expApproxUpTo' :: Int -> Double -> Double
expApproxUpTo' n = (\x ->  1 + (sum [ (x^k) / (fromIntegral $ product [1..k]) | k <-[1..n]]) )

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

--f x = (2 + x) * 5
--w notacji point free
--a x = (*5)((+2)x)

--[xd] - stringów, konkatenacja przy pomocy map z przecinkiem
-- pp :: [String] -> String
-- pp xs = foldr1 (\x y -> x ++ "," ++ y) xs

--string - collection pipeline, długość najdłuższego słowa w liście
-- faa :: String -> Int
-- faa s = 
--   foldr max 0 .
--   map length $ 
--   words s

-- faa2 :: String -> Int
-- faa2 s = 
--   maximum .
--   map length $
--   words s

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc2 :: Ord a => [a] -> [a]
sortDesc2 xs = reverse (sort xs)


onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) 
  | (p x) == True = x : filter' p xs
  | otherwise = filter' p xs


filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs) = if (p x) == True 
  then x : filter1 p xs
  else filter1 p xs

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElem xs = [x*2 | x <- xs]

sumWith1 g []     = 0
sumWith1 g (x:xs) = g x + sumWith1 g xs -- (+) (g x) (sumWith g xs)

prodWith1 g []     = 1
prodWith1 g (x:xs) = g x * prodWith1 g xs -- (*) (g x) (prodWith g xs)

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldl123' :: (a -> b -> b) -> b -> [a] -> b
foldl123' f z [] = z
foldl123' f z (x:xs) = foldl123' f (f x z) xs

-- *Main> foldl (\acc x -> x : acc) [] list1To5
-- [5,4,3,2,1]
-- *Main> foldr (\x xs -> xs ++ [x]) [] list1To5
-- [5,4,3,2,1]

fTmp x = (*5)((+2)x)

maxWord :: String -> Int
maxWord s = foldr max 0 (map length (words s))

--string - collection pipeline, długość najdłuższego słowa w liście
-- faa :: String -> Int
-- faa s = 
--   foldr max 0 .
--   map length $ 
--   words s

maxWord2 :: String -> Int
maxWord2 s = foldr max 0 . map length $ words s

convertToP :: [String] -> String
convertToP xs = foldl (\x y -> x ++ "," ++ y) "" xs


concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

--isSortedAsc :: Ord a => [a] -> Bool
--isSortedAsc xs = if length (zip [(minimum xs)..(maximum xs)] xs > 0 then False else True