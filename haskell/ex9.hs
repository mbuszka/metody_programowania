{-# LANGUAGE BangPatterns #-}
import Data.Char (intToDigit)
import Data.List (unfoldr, sort)

{- exercise 1

  reverse [] = []
  reverse (x:xs) = reverse xs ++ [x]

  rev = aux [] where
    aux ys [] = ys
    aux ys (x:xs) = aux (x:ys) xs

  reverse [] = [] = aux [] []  = rev []
  reverse (x:xs) = reverse xs ++ [x] = rev xs ++ [x] = aux [] xs ++ [x]
    = aux [x] xs = aux [] (x:xs) = rev (x:xs)

   exercise 2


Lemma xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
Proof
  1. [] ++ ([] ++ []) = [] ++ [] = ([] ++ []) ++ []
  2. x:xs ++ (ys ++ zs) = x:(xs ++ (ys ++ zs)) = x:((xs ++ ys) ++ zs )
      = x:(xs ++ ys) ++ zs = (x:xs ++ ys) ++ zs
  QED

Theorem reverse . reverse = id
Proof
  reverse (reverse (x1:xs)) = reverse (reverse xs ++ [x1]) = ... =
    = reverse ( [xn] ++ ... ++ [x1] ) -- bo xs jest skończone
    = reverse ( [xn, ... , x1 ] )
    = reverse ( xn:[xn-1, ... , x1] )
    = reverse ( [xn-1, ... , x1] ) ++ [xn]
    = ... = [x1] ++ ... ++ [xn]
    = x1:xs
 -}

-- exercise 4
-- fib :: Integer -> Integer
-- fib n = fibs !! (fromIntegral n) where
--   fibs = 1:1:zipWith (+) fibs (tail fibs)
fib :: Integer -> Integer
fib n = go n (0,1)
  where
    go !n (!a, !b) | n==0      = a
                   | otherwise = go (n-1) (b, a+b)


-- exercise 5

roots :: (Double, Double, Double) -> [Double]
roots (a,b,c) = let delta = b*b - 4*a*c in case compare delta 0 of
  LT -> []
  EQ -> [-b - (sqrt delta) / 2 / a]
  GT -> [-b - (sqrt delta) / 2 / a, -b + (sqrt delta) / 2 / a]

-- exercise 6

integerToString :: Integer -> String
integerToString = reverse . unfoldr f where
  f 0 = Nothing
  f x = Just (intToDigit $ fromEnum x `mod` 10, x `div` 10)

-- exercise 7

newtype FSet a = FSet (a -> Bool)

member :: Ord a => a -> FSet a -> Bool
member x (FSet f) = f x

empty :: FSet a
empty = FSet (\_ -> False)

singleton :: Ord a => a -> FSet a
singleton x = FSet (\y -> x == y)

fromList :: Ord a => [a] -> FSet a
fromList l = FSet (f $ sort l) where
  f (x:xs) y = case compare y x of
               LT -> False
               EQ -> True
               GT -> f xs y

intersection :: Ord a => FSet a -> FSet a -> FSet a
intersection (FSet f) (FSet g) = FSet h where
  h x = f x && g x

union :: Ord a => FSet a -> FSet a -> FSet a
union (FSet f) (FSet g) = FSet h where
  h x = f x || g x
