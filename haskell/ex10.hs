-- exercise 1

nat2 :: [(Integer, Integer)]
nat2 = [(x, c - x) | c <- [0..], x <- [0..c]]

-- exercise 2

halve :: [a] -> ([a], [a])
halve []       = ([], [])
halve [x]      = ([x], [])
halve (x:y:xs) = (x:ys, y:zs) where
      (ys, zs) = halve xs

merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge (x:xs, y:ys) = case compare x y of
  LT -> x:(merge (xs, y:ys))
  _  -> y:(merge (x:xs, ys))

cross :: (a -> c, b -> d) -> (a,b) -> (c,d)
cross (f,g) = pair (f . fst, g . snd)

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

msort []  = []
msort [x] = [x]
msort xs =
  merge . cross (msort, msort) . halve $ xs

-- exercise 3

mergeUnique :: Ord a => [a] -> [a] -> [a]
mergeUnique [] ys = ys
mergeUnique xs [] = xs
mergeUnique (x:xs) (y:ys) = case x `compare` y of
  LT -> x:(mergeUnique xs (y:ys))
  EQ -> mergeUnique xs (y:ys)
  GT -> y:mergeUnique (x:xs) ys

d235 :: [Integer]
d235 = 1:(mergeUnique d23 (map (*5) d235)) where
  d23 = (mergeUnique (map (*2) d235) (map (*3) d235))

-- exercise 5

class MMonoid a where
  (***) :: a -> a -> a
  e :: a
infixl 6 ***

instance MMonoid Integer where
  (***) = (+)
  e = 0

infixr 7 ^^^
(^^^) :: MMonoid a => a -> Integer -> a
a ^^^ n = if n <= 0 then e else case n `mod` 2 of
  1 -> a *** rest
  0 -> rest
  where
    rest = an *** an
    an   = a ^^^ (n `div` 2)

-- exercise 6

data Mt2x2 a = Mt2x2 a a a a
instance Show a => Show (Mt2x2 a) where
  show (Mt2x2 a11 a12 a21 a22) = show [a11, a12, a21, a22]

instance Num a => MMonoid (Mt2x2 a) where
  e = Mt2x2 1 0 0 1
  (***) (Mt2x2 a11 a12 a21 a22) (Mt2x2 b11 b12 b21 b22) = Mt2x2 c11 c12 c21 c22 where
    c11 = a11 * b11 + a12 * b21
    c12 = a11 * b12 + a12 * b22
    c21 = a21 * b11 + a22 * b21
    c22 = a21 * b12 + a22 * b22

fib :: Integer -> Integer
fib n = f where
  Mt2x2 _ f _ _ = (Mt2x2 0 1 1 1) ^^^ n
