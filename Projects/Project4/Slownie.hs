{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Slownie (Rodzaj(..), Waluta(..), slownie) where

import           Data.List  (unfoldr)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Waluty(Waluta(..), Rodzaj(..))
import           Numbers

data Case   = NomSin | NomPl  | GenPl  deriving Eq

type Exponent = Integer
type Number = (Integer, Exponent)
type RichNumber = (Integer, Exponent, Case)

class Token a where
  def :: Integer -> a

instance Token Number where
  def n = (n, 0)

instance Token RichNumber where
  def n = (n, 0, GenPl)

upperBound :: Integer
upperBound = 10 ^ 6000 - 1

slownie :: Waluta -> Integer -> String
slownie c n = number ++ currency where
  toks = tokenize toThousands $ abs n
  (_, _, grammCase) = head toks
  number
    | n == 0    = "zero "
    | n <  0    = "minus " ++ num
    | otherwise = num
  num = if abs n > upperBound
    then "mnóstwo "
    else unwords $ filter (not . null) $ translate $ reverse toks
  currency = case grammCase of
    NomSin -> mianownikPoj c
    NomPl  -> mianownikMn  c
    GenPl  -> dopelniaczMn c

tokenize :: (Token a, Token b) => (b -> Maybe (a, b)) -> Integer -> [a]
tokenize f n = if n == 0 then [def n] else
   unfoldr f (def n)

toThousands :: Number -> Maybe (RichNumber, Number)
toThousands (n, e)
  | n <= 0    = Nothing
  | otherwise = Just ((n `mod` 1000, e, c), (n `div` 1000, e + 1))
  where
    teen = n `mod` 100
    digit = n `mod` 10
    c
      | n == 1                   = NomSin
      | teen < 20 && teen  >= 10 = GenPl
      | digit >=2 && digit <= 4  = NomPl
      | otherwise                = GenPl

inThousands :: Number -> Maybe (Number, Number)
inThousands (n, e)
  | n <= 0              = Nothing
  | e == 0 && teen < 20 = Just ((teen, e), (n `div` 100, e + 2))
  | otherwise           = Just ((n `mod` 10, e), (n `div` 10, e + 1))
  where teen = n `mod` 100

toBig :: Number -> Maybe (Number, Number)
toBig (n, e)
  | n <= 0    = Nothing
  | otherwise = Just ((n `mod` 10, e), (n `div` 10, e + 1))

translate :: [RichNumber] -> [String]
translate = map f where
  f (n, e, c) = if n == 0 then "" else number ++ bignum where
    number = if e /= 0 && n == 1 then "" else unwords num ++ " "
    num    = reverse $ map translateNumber $ tokenize inThousands n
    bignum = if e == 0 then "" else bigNumeral c e

bigNumeral :: Case -> Exponent -> String
bigNumeral c e
  | k == 0 = case c of
      NomSin -> "tysiąc"
      NomPl  -> "tysiące"
      GenPl  -> "tysięcy"
  | otherwise = prefix ++ case c of
      NomSin -> if i == 0 then "lion"   else "liard"
      NomPl  -> if i == 0 then "liony"  else "liardy"
      GenPl  -> if i == 0 then "lionów" else "liardów"
  where
    (k, i) = e `divMod` 2
    prefix = if k < 10 then fromMaybe "" $ M.lookup k latinSmall
                       else concatMap translateBig $ tokenize toBig k

translateBig :: Number -> String
translateBig (n, e)
  | e == 0 && n == 0 = ""
  | e == 0 = fromMaybe "" $ M.lookup n latinOneNine
  | e == 1 = fromMaybe "" $ M.lookup n latinTenNinety
  | e == 2 = fromMaybe "" $ M.lookup n latinHundredNinehundred

translateNumber :: Number -> String
translateNumber (n, e)
  | e == 0 && n < 20 = fromMaybe "" $ M.lookup n oneNineteen
  | e == 1 && n == 2 = "dwadzieścia"
  | e == 1 && n == 3 = "trzydzieści"
  | e == 1 && n == 4 = "czterdzieści"
  | e == 1           = fromMaybe "" (M.lookup n oneNineteen) ++ "dziesiąt"
  | n == 1           = "sto"
  | n == 2           = "dwieście"
  | n <= 4           = fromMaybe "" (M.lookup n oneNineteen) ++ "sta"
  | otherwise        = fromMaybe "" (M.lookup n oneNineteen) ++ "set"
