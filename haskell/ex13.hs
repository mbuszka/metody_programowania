{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad(mplus)
import System.IO.Unsafe(unsafePerformIO)

main :: IO ()
main = return ()

-- exercise 2

newtype Random a = Random (Int -> (a, Int))

instance Functor Random where
  g `fmap` Random f = Random $ \s -> let (a, n) = f s in (g a, n)

instance Applicative Random where
  pure i = Random $ \s -> (i, s)
  Random phi <*> Random g = Random $ \s -> let (f, s') = phi s
                                               (a, s'') = g s' in (f a, s'')

instance Monad Random where
  Random f >>= phi = Random $ \s -> let (a, s') = f s
                                        Random g = phi a
                                        (b, s'') = g s' in (b, s'')

init :: Int -> Random ()
init n = Random $ const ((), n)

random :: Random Int
random = Random f where
  f s = (a, ns) where
    a = if ns > 0 then ns else ns + 2147483647
    ns = 16807 * m - 2836 * d
    (d, m) = s `divMod` 127773

run :: Random a -> a
run (Random f) = fst $ f 0

-- exercise 3

type SSC a = StateCompute String a

runSSC :: SSC a -> String -> a
runSSC (SC f) s = fst $ f s

getc :: SSC Char
getc = SC $ \(s:ss) -> (s, ss)

ungetc :: Char -> SSC ()
ungetc c = SC $ \s -> ((), c:s)

isEOS :: SSC Bool
isEOS = SC $ \s -> (null s, s)

countLines :: String -> Int
countLines = runSSC $ lines' 0 where
  lines' :: Int -> SSC Int
  lines' n = do
    b <- isEOS
    if b
      then return n
      else do
        ch <- getc
        lines' (if ch == '\n' then n+1 else n)


-- exercise 4

newtype StateCompute s a = SC (s -> (a, s))

exec :: StateCompute s a -> (s -> (a, s))
exec (SC f) = f

instance Functor (StateCompute s) where
  fmap f (SC g) = SC (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (StateCompute s) where
  pure a = SC (\s -> (a, s))
  SC phi <*> SC g = SC $ \s ->
    let (f, s')  = phi s
        (a, s'') = g s'
    in  (f a, s'')

instance Monad (StateCompute s) where
  SC f >>= phi = SC $ \s ->
    let (a, s') = f s
    in  exec (phi a) s'

-- exercise 5

tails1 :: [a] -> [[a]]
tails1 [] = [[]]
tails1 ss = ss:tails1 (tail ss)

tails2 :: [a] -> [[a]]
tails2 [] = [[]]
tails2 ss = ss:[s | s <- tails2 (tail ss)]

tails3 :: [a] -> [[a]]
tails3 ss = return ss `mplus` tails3 (tail ss)

-- exercise 6

data Term sig var = Var var | FunSym sig [Term sig var] deriving Show

instance Functor (Term sig) where
  fmap f (Var v) = Var $ f v
  fmap f (FunSym sig vars) = FunSym sig $ map (fmap f) vars

instance Applicative (Term sig) where
  pure = Var
  (<*>) = undefined

instance Monad (Term sig) where
  Var a           >>= f = f a
  FunSym sig vars >>= f = FunSym sig $ map (>>= f) vars

-- exercise 7

data Request = PutStrLn String | ReadLine
data Response = OK | OKStr String
type Dialog = [Response] -> [Request]

dialogToIOMonad :: Dialog -> IO ()
dialogToIOMonad dialog = do
  let responses = map (unsafePerformIO . respond) . dialog $ responses
  _ <- mapM (\_ -> return ()) responses
  -- responses <- mmap requests
  -- requests  <- return $ dialog responses
  return ()
  where
    mmap :: [Request] -> IO [Response]
    mmap []      = return []
    mmap (r:rqs) = do
      rp  <- respond r
      rps <- mmap rqs
      return $ rp:rps
    respond :: Request -> IO Response
    respond req =
      case req of
        PutStrLn s -> do
          putStrLn s
          return OK
        ReadLine   -> do
          l <- getLine
          return $ OKStr l


example :: Dialog
example resps = ReadLine :
  (case resps of
    OKStr str : resps -> if str == ""
      then []
      else (PutStrLn . reverse $ str) :
        (case resps of
          OK : resps -> example resps))
