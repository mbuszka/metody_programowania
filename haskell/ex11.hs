module Main where
main :: IO ()
main = return ()
{-- exercise 1

scanr f a = map (foldr f a) . tails

case []
  scanr f a [] = map (foldr f a) $ tails a = map (foldr f a) [[]] =
               = [foldr f a []] = [a]

case x:xs
  scanr f a (x:xs) = map (foldr f a) (tails (x:xs))
                   = map (foldr f a) ((x:xs):tails xs)
                   = foldr f a (x:xs):map (foldr f a) (tails xs)
                   = f x (foldr f a xs): map (foldr f a) (tails xs)
                   = f x (foldr f a xs): scanr f a xs
                   = f x (head ys) : ys where
                       ys = scanr f a xs
--}

-- exercise 2

ssm :: Ord a => [a] -> [a]
ssm = reverse . foldl aux [] where
  aux []     x = [x]
  aux (y:ys) x = if x > y then x:y:ys else y:ys

{-- exercise 3 not finished

t = y * z
(x, y)^z = (x+t, t)

foldl1 (+) . scanl (*) a = fst . foldl (^) (a,a)
case []
  foldl1 (+) $ scanl (*) a [] = foldl1 (+) [a] = a
  fst $ foldl (^) (a,a) [] = fst (a,a) = a

case (x:xs)
  left  = foldl1 (+) $ scanl (*) a (x:xs)
        = foldl1 (+) $ a:scanl (*) (a*x) xs
        = foldl (+) a $ scanl (*) (a*x) xs

  right = fst $ foldl (^) (a,a) (x:xs)
        = fst $ foldl (^) ((a,a)^x) xs
        = fst $ foldl (^) (a+t, t) xs where t = a * x
        = fst $ foldl (^) (a+a*x, a*x) xs

lemma foldl (+) a $ scanl (*) (a*x) (y:ys) = fst $ foldl (^) (a+a*x, a*x) (y:ys)
proof
  left  = foldl (+) a $ scanl (*) (a*x) (y:ys)
        = foldl (+) a $ a*x:scanl (*) (a*x*y) ys
        = foldl (+) (a+a*x) $ scanl (*) (a*x*y) ys

  right = fst $ foldl (^) (a+a*x, a*x) (y:ys)
        = fst $ foldl (^) ((a+a*x, a*x)^y) ys
        = fst $ foldl (^) (a+a*x+a*x*y, a*x*y) ys

--}

{-- exercise 4

foldr f e xs = foldl (flip f) e (reverse xs)

case []
  left  = foldr f e [] = e
  right = foldl (flip f) e (revererse []) = foldl (flip f) e [] = e

case x:xs
  left  = foldr f e (x:xs) = f x (foldr f e xs)
  right = foldl (flip f) e (reverse x:xs)
        = foldl (flip f) e (reverse xs ++ [x])
        = lemma =
        = flip f (foldl (flip f) e (reverse xs)) x
        = f x (foldl (flip f) e (reverse xs))
        = inductive assumption =
        = f x (foldr f e xs)

  lemma
    foldl f e (xs ++ [a]) = f (foldl f e xs) a
  proof
  case []
    left  = foldl f e ([] ++ [a]) = foldl f e [a] = foldl f (f e a) [] = f e a
    right = f (foldl f e []) a = f e a

  case (x:xs)
    left  = foldl f e ((x:xs) ++ [a])
          = foldl f e x:(xs ++ [a])
          = foldl f (f e x) (xs ++ [a])
          = inductive assumption =
          = f (foldl f (f e x) xs) a

    right = f (foldl f e (x:xs)) a
          = f (foldl f (f e x) xs) a
  QED
QED

--}

-- exercise 5

mLengthr :: Integral b => [a] -> b
mLengthr = foldr (\_ y -> y + 1) 0

mLengthl :: Integral b => [a] -> b
mLengthl = foldl (\y _ -> y + 1) 0

(+++) :: [a] -> [a] -> [a]
(+++) = flip $ foldr (:)

mConcat :: [[a]] -> [a]
mConcat = foldr (++) []

mReverse :: [a] -> [a]
mReverse = foldl (flip (:)) []

mSum :: Num a => [a] -> a
mSum = foldl (+) 0

{-- exercise 6

1, 2 :: Num a => a
(*)  :: Num a => a -> a -> a
sin  :: Floating a => a -> a
map  :: (a -> b) -> [a] -> [b]

f :: Num ((a -> b) -> [a] -> [b]) => c -> (a -> b) -> [a] -> [b]
f x = map -1 x

g :: Num (a -> b) => [a] -> [b]
g x = map (-1) x

h :: Num [a] => a -> [[a]]
h x = [x] : [1]

i :: (Floating c, Num (a -> c)) => (a -> c) -> (a -> c)
i x = x * sin . 1 --}

-- exercise 7

loop :: a
loop = loop

ones :: [Integer]
ones = 1 : ones

-- head $ 1 : loop                              (i)
-- fst (1, loop)                                (i)
-- length [loop, loop, loop]                    (i)
-- length ones                                  (ii)  because its left fold
-- sum ones                                     (iii) because its right fold
-- last ones                                    (ii)
-- last [1..]                                   (ii)
-- let f [] = 0; f (_:xs) = 2 + f xs in f ones  (iii) because its right fold
