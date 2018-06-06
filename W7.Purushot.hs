--author @Purushottam_Thapa
--
module W7 where

-- Final week!
--
-- Think of this as the exam. You must get 4/10 exercises to pass the course.
--
-- Good luck.
--
-- NB. Do not add any imports!

import Data.List
import Control.Monad
import Control.Monad.Trans.State

------------------------------------------------------------------------------
-- Ex 1: Count how many numbers in the input list are in the given
-- low-high range (inclusive)
--
-- Examples:
--   countRange 5 8 [] ==> 0
--   countRange 1 3 [1,2,3,4,5] ==> 3

countRange :: Int -> Int -> [Int] -> Int
countRange low high [] = 0
countRange low high xs = length $ filter (\x -> x>=low && x<=high) xs 

------------------------------------------------------------------------------
-- Ex 2: Build a string that looks like an n*m chessboard:
--
--   #.#.#.#.
--   .#.#.#.#
--   #.#.#.#.
--
-- Examples:
--   chess 1 1 ==> "#\n"
--   chess 3 5 ==> "#.#.#\n.#.#.\n#.#.#\n"
--
-- Hint: it's easier to see how the chess board looks like if you run
--   putStr (chess 3 5)
-- in GHCi

chess :: Int -> Int -> String
chess 1 1 = "#\n" 
chess x y = chess'' y (concat $ mix' x y)

mix :: [String] -> [String] -> [String]
mix (x:xs) (y:ys) = x:y:mix xs ys  
mix x [] = x
mix [] y = y

mix' :: Int -> Int -> [String]
mix' x y
  | even (x*y) = mix (replicate (div (x*y) 2) "#") (replicate (div (x*y) 2) ".") 
  | otherwise = mix (replicate ((div (x*y) 2) + 1) "#") (replicate (div (x*y) 2) ".")  

chess' :: Int -> String -> String
chess' n xs = if length xs <= n then xs else take n xs ++ "\n" ++ chess' n (drop n xs)   

chess'' :: Int -> String -> String
chess'' n xs = xs' ++ "\n"
    where xs' = chess' n xs

   



------------------------------------------------------------------------------
-- Ex 3: Implement the function palindromify that chops a character
-- off the front _and_ back of a string until the result is a
-- palindrome.
--
-- Examples:
--   palindromify "ab" ==> ""
--   palindromify "aaay" ==> "aa"
--   palindromify "xabbay" ==> "abba"
--   palindromify "abracacabra" ==> "acaca"

palindromify :: String -> String
palindromify x
  | length x == 1 = x
  | otherwise = if (x == reverse x) then x else palindromify $ tail (init x) 

------------------------------------------------------------------------------
-- Ex 4: Remove all repetitions of elements in a list. That is, if an
-- element occurs in the input list 2 or more times in a row, replace
-- this with 1 occurrence.
--
-- DO NOT use any library list functions like head, tail, (++) and so on.
-- USE ONLY recursion and pattern matching to process the list.
--
-- It's ok to use (==) or compare obviously. If-then-else and guards
-- are fine too as long as you pattern match the list.
--
-- Examples:
--   unrepeat [True,True,True,True] => [True]
--   unrepeat [1,1,2,1,3,3,3] => [1,2,1,3]

unrepeat :: Eq a => [a] -> [a]
unrepeat [] = []
unrepeat [x] = [x]
unrepeat (x:y:xs) 
  | x /= y = x:unrepeat (y:xs)
  | otherwise = unrepeat (y:xs)

------------------------------------------------------------------------------
-- Ex 5: Given a list of Either String Int, sum all the integers.
-- Return Nothing if no integers were present in the list.
--
-- Examples:
--   sumEithers [Left "fail", Left "xxx"] ==> Nothing
--   sumEithers [Left "fail", Right 1, Left "xxx", Right 2] ==> Just 3

sumEithers :: [Either String Int] -> Maybe Int
sumEithers [] = Nothing
sumEithers (x:xs) = if (sumEithers' x) 
                      then if (sumEithers'' (x:xs) == 0) 
                                then Just 0 
                                else Just (sumEithers'' (x:xs))  
                      else sumEithers xs   

sumEithers'' :: [Either String Int] -> Int 
sumEithers'' [] = 0 
sumEithers'' (x:xs) = if (sumEithers' x) then getValue x + sumEithers'' xs else sumEithers'' xs 

sumEithers' :: Either String Int -> Bool
sumEithers' (Right _)  = True
sumEithers'  _         = False 

getValue :: Either String Int -> Int
getValue (Right x) = x
getValue _         = 0  

------------------------------------------------------------------------------
-- Ex 6: Define the data structure Shape with values that can be
-- either circles or rectangles. A circle has just a radius, and a
-- rectangle has a width and a height.
--
-- Use _two_ constructors, one for circles, one for rectangles.
--
-- Implement the function area that computes the area of a Shape
--
-- Also implement the functions circle and rectangle that create
-- circles and rectangles (don't worry if this feels stupid, I need
-- these for the tests :)
--
-- All dimensions should be Doubles.

data Shape = Circle Double | Rectangle Double Double  
  deriving Show -- leave this line in place

circle :: Double -> Shape
circle r = Circle r 

rectangle :: Double -> Double -> Shape
rectangle b l = Rectangle b l

area :: Shape -> Double
area (Circle r) = pi * r^2 
area (Rectangle b l) = l * b

------------------------------------------------------------------------------
-- Ex 7: Here's a Card type for a deck of cards with just two suits
-- and a joker. Implement Eq and Ord instances for Card.
--
-- The Ord instance should order cards such that
--   - Cards of the same suit are ordered according to value
--   - Suits are ordered Heart > Spade
--   - Joker is the largest card
--
-- Examples:
--   Spade 1 == Spade 2 ==> False
--   sort [Heart 3, Spade 2, Joker, Heart 1] ==> [Spade 2,Heart 1,Heart 3,Joker]

data Card = Heart Int | Spade Int | Joker
  deriving Show

instance Eq Card where
  Joker   == Joker   = True
  Spade x == Spade y = x == y 
  Heart x == Heart y = x == y 
  _       == _       = False 

instance Ord Card where
  Spade x <= Heart y = True
  Spade x <= Joker = True
  Heart x <= Joker = True
  Heart x <= Heart y = x <= y  
  Spade x <= Spade y = x <= y
  x <= y = x == y 

------------------------------------------------------------------------------
-- Ex 8: Here's a type Twos for things that always come in pairs. It's
-- like a list, but it has an even number of elements (and is also
-- never empty).
--
-- Implement a Functor instance for Twos.

data Twos a = End a a | Continue a a (Twos a)
  deriving (Show, Eq)

instance Functor Twos where
  fmap f (End x y) = End (f x) (f y) 
  fmap f (Continue x y g) = Continue (f x) (f y) (fmap f g)
------------------------------------------------------------------------------
-- Ex 9: Use the state monad to update the state with the sum of the
-- even numbers in a list. Do this by implementing the step function
-- below so that the sumEvens operation works correctly.
--
-- Examples:
--   execState (sumEvens [1,2,3,4]) 0
--   6

step :: Int -> State Int ()
step x = if even x then (modify (+x)) else return () 

sumEvens :: [Int] -> State Int ()
sumEvens is = forM_ is step

------------------------------------------------------------------------------
-- Ex 10: Here's a type Env for values that depend on an environment
-- (represented here by just a String). You'll also find some
-- utilities and example operations of type Env.
--
-- Your job is to define Functor and Monad instances for Env.
--
-- Examples of how the instances should work:
--
--
--   runEnv (fmap (+1) (return 3)) "env" ==> 4
--   runEnv (fmap (*2) envLength) "boing" ==> 10
--   runEnv (return 3) "env" ==> 3
--   runEnv (envLength >>= multiply) "xyz" ==> "xyzxyzxyz"
--   runEnv (greet >>= \g -> return ("The greeting is: "++g)) "bob"
--     ==> "The greeting is: Hello, bob"
--
-- Hint: consider W5 ex16

data Env a = MkEnv (String -> a)

runEnv :: Env a -> String -> a
runEnv (MkEnv f) str = f str

-- return a greeting for the name in the environment
greet :: Env String
greet = MkEnv (\name -> "Hello, "++name)

-- return the length of the environment
envLength :: Env Int
envLength = MkEnv (\name -> length name)

-- return a string consisting of n copies of the env
multiply :: Int -> Env String
multiply n = MkEnv (\name -> concat (replicate n name))

instance Functor Env where
  fmap f (MkEnv g) = MkEnv (f.g)  

instance Monad Env where
  e >>= f =  MkEnv g
      where g str = let x = runEnv e str
                        e2 = f x
                        y = runEnv e2 str
                    in y 
  return x = MkEnv (\s -> x) 

-- Disregard this instance. In recent versions of the Haskell standard
-- library, all Monads must also be Applicative. These exercises don't
-- really cover Applicative.
instance Applicative Env where
  pure = return
  (<*>) = ap
