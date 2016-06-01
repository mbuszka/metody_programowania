module Main where
import Data.Maybe (fromMaybe)
import Control.Monad
main :: IO ()
main = return ()
-- exercise 1

permi1 :: [a] -> [[a]]
permi1 [] = [[]]
permi1 (x:xs) = concatMap (inserts x) (permi1 xs) where
  inserts a ls = map (f . flip splitAt ls) [0 .. (length ls)] where
    f (l, r) = l ++ [a] ++ r

permi2 :: [a] -> [[a]]
permi2 [] = [[]]
permi2 (x:xs) = [ l | perm <- permi2 xs, l <- inserts x perm ] where
  inserts a ls = [ l ++ [a] ++ r | (l,r) <- pairs ] where
    pairs = [splitAt n ls | n <- [0 .. (length ls)]]

permi3 :: [a] -> [[a]]
permi3 []     = [[]]
permi3 (x:xs) = do
  p <- permi3 xs
  inserts x p where
    inserts a ls = do
      n <- [0 .. (length ls)]
      let (l, r) = splitAt n ls
      return $ l ++ [a] ++ r

-- better

insert :: MonadPlus m => a -> [a] -> m [a]
insert e [] = return [e]
insert e (x:xs) = return (e:x:xs)
                  `mplus`
                  do
                    tmp <- insert e xs
                    return $ x:tmp


permi3' :: MonadPlus m => [a] -> m [a]
permi3' []  = return []
permi3' (x:xs) = do
  tmp <- permi3' xs
  insert x tmp


-- exercise 2

perms1 :: [a] ->[[a]]
perms1 []     = [[]]
perms1 xxs = concatMap f selections where
  selections = selects xxs
  f (y, xs) = map (y:) (perms1 xs)
  selects (x:xs) = (x, xs): map (\(y, ys) -> (y, x:ys)) (selects xs)
  selects []     = []

perms2 :: [a] -> [[a]]
perms2 []  = [[]]
perms2 xxs = [y:ys | (y,xs) <- selects xxs, ys <- perms2 xs]
  where
    selects (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selects xs]
    selects []     = []

perms3 :: [a] -> [[a]]
perms3 []  = [[]]
perms3 xxs = do
  (y, xs) <- selects xxs
  ys <- perms2 xs
  return $ y:ys where
    selects [] = []
    selects (x:xs) = (x, xs) : do
      (y, ys) <- selects xs
      return (y, x:ys)

-- exercise 3

sublist1 :: [a] -> [[a]]
-- sublist1 []     = [[]]
-- sublist1 (x:xs) = concatMap (\ls -> [x:ls, ls]) $ sublist1 xs
sublist1 = foldr (\x -> concatMap (\ls -> [x:ls, ls])) [[]]


sublist2 :: [a] -> [[a]]
sublist2 []     = [[]]
sublist2 (x:xs) = [a | rest <- sublist2 xs, a <- [x:rest, rest]]

sublist3 :: [a] -> [[a]]
sublist3 []     = [[]]
sublist3 (x:xs) = do
  rest <- sublist3 xs
  [x:rest, rest]


-- exercise 4

prod :: [Integer] -> Integer
prod = foldr (\n p -> if n == 0 then 0 else p * n) 1

prodM :: [Integer] -> Integer
prodM xs = fromMaybe 0 (foldr f (Just 1) xs) where
    f n mp = if n == 0 then Nothing else do
      p <- mp
      return $ n * p

-- exercise 5

data Cyclist a = Elem (Cyclist a) a (Cyclist a)

fromList :: [a] -> Cyclist a
fromList [] = error "list should contain at least one element"
fromList xs = let (f, l) = go l xs f in f where
  go prev []     next = (next, prev)
  go prev (y:ys) next = let this          = Elem prev y rest
                            (rest, last') = go this ys next
                        in  (this, last')

forward :: Cyclist a -> Cyclist a
forward (Elem _ _ next) = next

backward :: Cyclist a -> Cyclist a
backward (Elem prev _ _) = prev

label :: Cyclist a -> a
label (Elem _ x _) = x

-- exercise 6

enumInts' :: Cyclist Integer
enumInts' = aux 0 where
  aux n = Elem (aux (n-1)) n (aux $ n+1)

enumInts :: Cyclist Integer
enumInts = let
  zero = Elem neg 0 pos
  (infNeg, neg) = gobkw infPos [-1, -2 ..] zero
  (pos, infPos) = gofwd zero   [1 .. ]     infNeg
  in zero where
    gobkw _ []     _       = error ""
    gobkw prev (y:ys) next = let this = Elem rest y next
                                 (first, rest) = gobkw prev ys this
                             in  (first, this)
    gofwd _    []     _    = error ""
    gofwd prev (y:ys) next = let this = Elem prev y rest
                                 (rest, last') = gofwd this ys next
                             in  (this, last')

-- exercise 7

newtype Cyc a b = Cyc (Cyclist a -> (b, Cyclist a))
instance Functor (Cyc a) where
  fmap f (Cyc g) = Cyc (\c -> let c' = g c in (f . fst $ c', snd c'))

instance Applicative (Cyc a) where
  pure x = Cyc (\c -> (x, c))
  Cyc f <*> Cyc g = Cyc (\c ->
    let (phi, c') = f c
        (x,  c'') = g c'
    in  (phi x, c''))

instance Monad (Cyc a) where
  Cyc f >>= phi = Cyc (\c ->
    let (x, c') = f c
        Cyc g = phi x
    in  g c')

runCyc :: Cyclist a -> Cyc a b -> b
runCyc c (Cyc f) = let (x, _) = f c in x

fwd :: Cyc a ()
fwd = Cyc (\c -> ((), forward c))

bkw :: Cyc a ()
bkw = Cyc (\c -> ((), backward c))

lbl :: Cyc a a
lbl = Cyc (\c -> (label c, c))

example :: Integer
example = runCyc enumInts (do
  bkw
  bkw
  bkw
  bkw
  x <- lbl
  fwd
  fwd
  y <- lbl
  fwd
  z <- lbl
  return (x+y+z))

example2 :: Integer
example2 = runCyc enumInts (do
  fwd
  fwd
  fwd
  x <- lbl
  fwd
  fwd
  y <- lbl
  bkw
  z <- lbl
  return $ x * y * z)


example3 :: Integer
example3 = runCyc enumInts (do
  _ <- replicateM 10000000 (do fwd; bkw)
  lbl)

example3' :: Integer
example3' = runCyc enumInts' (do
  _ <- replicateM 10000000 (do fwd; bkw)
  lbl)
