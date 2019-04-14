-- {-# LANGUAGE DeriveFunctor #-}

doActSeq = do
    putChar 'A'
    putChar 'G'
    putChar 'H'
    putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
    line <- getLine
    putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"


echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
        >> getLine
        >>= \n -> let num = read n :: Int in
                    if num == 7
                    then putStrLn "Ah, lucky 7!"
                    else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doEcho3 = do
    l1 <- getLine
    l2 <- getLine
    putStrLn $ l1 ++ l2

doDialog = do
    putStr "a? "
    n <-getLine
    let num = read n :: Int in
        if num == 7
        then putStrLn "Ah, lucky 7!"
        else if odd num
            then putStrLn "Odd number! That's most people's choice..."
            else putStrLn "Hm, even number? Unusual!"

-----------------------------------------------
nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
  action
  nTimes (n-1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
  1 -> \name -> putStrLn ("Good morning, " ++ name)
  2 -> \name -> putStrLn ("Good afternoon, " ++ name)
  3 -> \name -> putStrLn ("Good night, " ++ name)
  _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList = [ioActionFactory 1 "Ben",
              ioActionFactory 2 "Joe",
              ioActionFactory 3 "Ally"]

sequence'        :: [IO ()] -> IO ()
sequence' []     =  return ()
sequence' (a:as) =  do a
                       sequence' as


-- import System.Environment
-- import System.IO

-- main = do
--   (inFileName:outFileName:_) <- getArgs
--   inHdlr <- openFile inFileName ReadMode
--   outHdlr <- openFile outFileName WriteMode
--   inpStr <- hGetContents inHdlr
--   hPutStr outHdlr inpStr
--   hClose inHdlr
--   hClose outHdlr
----------------------------------------------

-- newtype Box a = MkBox a deriving (Show, Functor)

-- instance Functor Box where
--   fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
    fmap _ EmptyBT    = EmptyBT
    fmap f (NodeBT x lBt rBt) = NodeBT (f x) (fmap f lBt) (fmap f rBt)


newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Applicative MyTriple where
  pure f = MyTriple f
  (MyTriple f) <*> w = fmap f w

instance Functor MyTriple where
  fmap f (MyTriple (x,y,z)) = MyTriple ((f x),(f y),(f z))