module Test where


import Data.List
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Ratio

sumNumbers :: [Int] -> Int
sumNumbers [] = 0
sumNumbers (x:xs) = x 

yourHead :: [Int] -> Int
yourHead xs = case xs of 
      (x:[1,2,3]) -> 3
      (x:_) -> x
      []    -> -1 

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

readInt :: String -> Either String Int
readInt "0" = Right 1
readInt "1" = Right 0
readInt _   = Left "This is otherwise"

addEither :: Either String Int -> Bool
addEither a = if (a==(Right 3)) then True else False 

myFilter :: [Int] -> [Int]
myFilter xs = filter (>3) xs 

--function as a parameter
addFunc :: (Int -> Int) -> Int 
addFunc addFunc' = addFunc' 7 

addFunc' :: Int -> Int
addFunc' a = a+3 

myMap :: [Int] -> [Int]
myMap xs = map (+2) xs

questionnaire = do
  putStrLn "Write something!"
  s <- getLine
  putStrLn $ "Your wrote: "++s

yesNoQuestion :: String -> IO Bool
yesNoQuestion question = do
  putStrLn question
  s <- getLine
  return $ s == "Y"

build_List :: Int -> [Int]
build_List 0 = []
build_List n = n:build_List (n-1)

--list consuming

sumNumbers1 :: [Int] -> Int
sumNumbers1 [] = 0
sumNumbers1 (x:xs) = x + sumNumbers xs 

myMaximum :: [Int] -> Int
myMaximum [] = 0
myMaximum (x:xs) = go x xs
  where go biggest [] = biggest
        go biggest (x:xs) = go (max biggest x) xs

fibonacci :: Int -> Int
fibonacci n = fibonacci' 0 1 n
  where fibonacci' a b 1 = b
        fibonacci' a b n = fibonacci' b (a+b) (n-1)

isums :: Int -> IO Int
isums n = go 0 n
  where go sum 0 = return sum
        go sum n = do i <- readLn
                      let sum' = sum + i
                      print sum'
                      go sum' (n-1) 

class Foo a where
  foo :: a -> Int

class Foo a => Bar a where
  bar :: a -> a -> Int
  bar x y = foo x + foo y

apply :: (Ord k) => k -> v -> (v -> v) -> [(k,v)] -> [(k,v)]
apply k v f ds =
  let (p1,px) = span ( (k >) . fst) ds
      (p2,p3) = case px of
        []     -> ((k,v),[])
        (x:xs) -> if fst x == k
           then ((k, f $ snd x), xs)
           else ((k, v),       x:xs)
  in  p1 ++ (p2 : p3)

class Foo_class a where
  empty :: a
  size  :: a -> Int
  sameSize :: a -> a -> Bool

instance Foo_class (Maybe a) where
  empty = Nothing
  
  size Nothing = 0   
  size (Just a) = 1
 
  sameSize x y = size x == size y

evenList :: [Int] -> Bool
evenList = foldr ((&&) . even) True

(?>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing ?> _ = Nothing
Just x ?> f = f x

increase :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase key val assocs = lookup key assocs ?> check ?> mk
  where check x
          | x < val   = Nothing
          | otherwise = Just x
        mk x = Just ((key, val) : delete (key, x) assocs)

increase' :: Eq a => a -> Int -> [(a,Int)] -> Maybe [(a,Int)]
increase' key val assocs = 
     lookup key assocs >>= 
     check >>= 
     mk
  where check x
          | val < x   = fail "" 
          | otherwise = return x
        mk x = return ((key, val) : delete (key, x) assocs) 

data Logger a = Logger [String] a deriving Show
getVal (Logger _ a) = a
getLog (Logger s _) = s

add :: Int -> State Int ()
add i = do old <- get
           put (old+i)
    
nomsg x = Logger [] x
annotate s x = Logger [s] x
msg s = Logger [s] ()

cylinder ::(RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in sideArea + 2 * topArea

head' :: [a] -> a
head' xs = case xs of [] -> error "No head"
                      (x:xs) -> x 

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "Empty"
                                                [x] -> "a singleton list"
                                                xs  -> "a longer list"

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
  where what [] = "empty"
        what [x] = "Sigleton List"
        what xs = "big list"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Max of empty list"
maximum' [x] = x
maximum' (x:xs) 
  | x > maxTail = x
  | otherwise = maxTail
      where maxTail = maximum' xs 

oddUpdate :: Int -> State [Int] ()
oddUpdate x = modify (inc x)
  where inc x [] = [x]
        inc x (y:yx)
           | (even x) && (x==y) = drop x yx
           | otherwise = x:inc x yx     


count :: Eq a => a -> State [(a,Int)] ()
count x = modify (inc x)
   where inc x [] = [(x,1)]
         inc x ((y,k):ys)
            | x == y = (y,k+1):ys
            | otherwise = (y,k):inc x ys  

chess :: Int -> Int -> [String]
chess x y
  | even (x*y) = mix (replicate (div (x*y) 2) "#") (replicate (div (x*y) 2) ".") 
  | otherwise = mix (replicate ((div (x*y) 2) + 1) "#") (replicate (div (x*y) 2) ".")  

mix :: [String] -> [String] -> [String]
mix (x:xs) (y:ys) = x:y:mix xs ys  
mix x [] = x
mix [] y = y

chess' :: Int -> Int -> String
chess' 1 1 = "#\n"
chess' x y = chess''' y (concat $ chess x y) 

chess'' :: Int -> String -> String
chess'' n xs = if length xs <= n then xs else take n xs ++ "\n" ++ chess'' n (drop n xs)   

chess''' :: Int -> String -> String
chess''' n xs = xs' ++ "\n"
    where xs' = chess'' n xs


