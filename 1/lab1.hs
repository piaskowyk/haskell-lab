sqr :: Double -> Double
sqr x = x * x

vec2Dln :: (Double, Double) -> Double
vec2Dln (x, y) = sqrt (x^2 + y^2)

vec3Dln :: (Double, Double, Double) -> Double
vec3Dln (x, y, z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a, b) = (b, a)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == y) && (x == z) && (y == z)

sng :: Int -> Int
sng x = if x > 0
            then 1
        else if x == 0
            then 0
        else 
            4

costam :: (Int, Int) -> Int
costam (a, b) = let k = 5
            in k+a+b

costam2 :: (Int, Int) -> Int
costam2 (a, b) = a+b+2*c
            where c = 5

c1 :: (Int) -> Bool
c1 n | n > 5 = True
    | otherwise = False

abs2 :: Int -> Int
abs2 x = if x >= 0 then 
            x
        else
            x*(-1)

tmp1 :: Int -> Int
tmp1 k = abs2 k

absIntt :: Int -> Int
absIntt n | n >= 0 = n
          | otherwise = -n

sgn2 :: Int -> Int
sgn2 n | n > 0 = 1
       | n == 0 = 0
       | otherwise = -1

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) | a < b && a < c = a
                  | b < a && b < c = b
                  | c < b && c < a = c
                  | a == b = a
                  | a == c = a
                  | b == c = b
                  | otherwise = a

ttt :: String -> Bool
ttt "mleko" = True
ttt _      = False

or' :: (Bool, Bool) -> Bool
or' (x, y) | x == False && y == False = False
           | otherwise = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (False, True) = False
and' (True, False) = False
and' _ = False

not' :: Bool -> Bool
not' a = if a == True 
        then False
        else True

notc :: Bool -> Bool
notc x = case x of
        True -> False
        False -> True

cas :: String -> String
cas x = case x of
        "a" -> "a xDDDD"
        "b" -> "xddddd"
        _ -> "hahah"

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ((-b - d)/e, (-b + d)/e)
    where { 
        d = sqrt(b^2 - 4*a*c); 
        e = 2*a
    }

roots2 :: (Double, Double, Double) -> (Double, Double)
roots2 (a, b, c) = 
    let {
        d = sqrt(b^2 - 4*a*c);
        e = 2*a
    }
    in ((-b - d)/e, (-b + d)/e)














    