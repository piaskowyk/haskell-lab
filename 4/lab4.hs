polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)


type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)


newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)


-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y


data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y


data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y


-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"



{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = MkCart3DVec a a a deriving Show 

xC :: Cart3DVec a -> a
xC (MkCart3DVec x _ _) = x

yC :: Cart3DVec a -> a
yC (MkCart3DVec _ y _) = y

zC :: Cart3DVec a -> a
zC (MkCart3DVec _ _ z) = z

data Cart3DVec' a = MkCart3DVec' {x1::a, y1::a, z1::a} deriving Show 

xC' :: Cart3DVec' a -> a
xC' (MkCart3DVec' {x1 = xVal, y1 = _, z1 = _}) = xVal

yC' :: Cart3DVec' a -> a
yC' (MkCart3DVec' {x1 = _, y1 = yVal, z1 = _}) = yVal

zC' :: Cart3DVec' a -> a
zC' (MkCart3DVec' {x1 = _, y1 = _, z1 = zVal}) = zVal


data Shape = Circle Float |
             Rectangle Float Float

-- area :: Shape -> Float