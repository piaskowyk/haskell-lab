myFn x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => a -> a -> a
add2C x y = x + y

add2Cn :: Num a => (a -> (a -> a))
add2Cn x y = x + y
-- jest prawo stronnie łączy, czyli z prawejs strony można dołaczyć do niego funkcję

add3T :: Num a => (a, a, a) -> a
add3T (x, y, z) = z + y + z

add3C :: Num a => (a -> a -> (a -> a))
add3C x y z = x + y + z

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = \(x, y) -> f x y

fiveToPower :: Integer -> Integer
fiveToPower = (5 ^)

toPower5 :: Num a => a -> a
toPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = ((-) 5)

flip2 :: (a -> b -> c) -> b -> a -> c 
flip2 f x y = f y x

isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx n l = head ( drop n l )

how3Pit :: Int
how3Pit = length [(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) | x == True = True
           | otherwise = or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = if x == False
                then False
              else and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | x == a = True
               | otherwise = elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll list = map (*2) list

squareAll :: Num t => [t] -> [t]
squareAll list = map (^2) list

selectEven :: Integral t => [t] -> [t]
selectEven xs = filter even xs

sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where { 
        loop acc [] = acc;
        loop acc (x:xs) = loop (x + acc) xs;
    }

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where { 
        loop acc [] = acc;
        loop acc (x:xs) = loop (x + acc) xs;
    }

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
    where {
        loop acc [] = acc;
        loop acc (x:xs) = loop (x * acc) xs;
    }

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
    where {
        loop acc [] = acc;
        loop acc (x:xs) = loop (acc + 1) xs;
    }

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where {
        leftPart  xs = [ y | y <- xs, y <= x ];
        rightPart xs = [ y | y <- xs, y > x  ];
    }

qSort' :: Ord a => [a] -> [a]
qSort' []     = []
qSort' (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
    where {
        leftPart  xs = filter (<= x) xs;
        rightPart xs = filter ( > x) xs;
    }

fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2Div :: (Eq a, Integral a) => [a] -> Bool
fst2Div (x : y : _) | (y `mod` x) == 0 = True
fst2Div _                    = False



len1 :: Num b => [a] -> b
len1 xs = loop 0 xs
    where {
        loop acc [] = acc;
        loop acc (x:xs) = loop (acc + 1) xs;
    }

sum1    :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum xs

sum2    :: Num a => [a] -> a
sum2 xs = loop 0 xs
    where {
        loop acc [] = acc;
        loop acc (x:xs) = loop (acc + x) xs;
    }

