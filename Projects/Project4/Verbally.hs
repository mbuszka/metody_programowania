{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Slownie (Rodzaj(..), Waluta(..), slownie) where
import           Data.List  (unfoldr)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)

data Rodzaj = Meski  | Zenski | Nijaki deriving Show
data Case   = NomSin | NomPl  | GenPl  deriving Eq

data Waluta = Waluta {
  mianownikPoj :: String,
  mianownikMn  :: String,
  dopelniaczMn :: String,
  rodzaj       :: Rodzaj
} deriving Show

type Exponent = Integer
type Number = (Integer, Exponent)
type RichNumber = (Integer, Exponent, Case)

class Token a where
  def :: Integer -> a

instance Token Number where
  def n = (n, 0)
instance Token RichNumber where
  def n = (n, 0, GenPl)

slownie :: Waluta -> Integer -> String
slownie c n = number ++ currency where
  toks = tokenize toThousands $ abs n
  (_, _, grammCase) = head toks
  number
    | n <  0    = "minus " ++ num
    | n == 0    = "zero "
    | otherwise = num
  num = unwords $ filter (not . null) $ translate $ reverse toks
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

oneNineteen :: M.Map Integer String
oneNineteen = M.fromList
  [(1, "jeden"),       (2, "dwa"),           (3, "trzy")
  ,(4, "cztery"),      (5, "pięć"),          (6, "sześć")
  ,(7, "siedem"),      (8, "osiem"),         (9, "dziewięć")
  ,(10, "dziesięć"),   (11, "jedenaście"),   (12, "dwanaście")
  ,(13, "trzynaście"), (14, "czternaście"),  (15, "piętnaście")
  ,(16, "szesnaćie"),  (17, "siedemnaście"), (18, "osiemnaście")
  ,(19, "dziewiętnaście")]

latinSmall :: M.Map Integer String
latinSmall = M.fromList
  [(1, "mi"),     (2, "bi"),     (3, "try")
  ,(4, "kwadry"), (5, "kwinty"), (6, "seksty")
  ,(7, "septy"),  (8, "okty"),   (9, "noni")]

latinOneNine :: M.Map Integer String
latinOneNine = M.fromList
  [(1, "un"),      (2, "do"),   (3, "tri")
  ,(4, "kwatuor"), (5, "kwin"), (6, "seks")
  ,(7, "septen"),  (8, "okto"), (9, "nowem")]

latinTenNinety :: M.Map Integer String
latinTenNinety = M.fromList
  [(1, "decy"),     (2, "wicy"),     (3, "trycy")
  ,(4, "kwadragi"), (5, "kwintagi"), (6, "seksginty")
  ,(7, "septagi"),  (8, "oktagi"),   (9, "nonagi")]

latinHundredNinehundred :: M.Map Integer String
latinHundredNinehundred = M.fromList
  [(1, "centy"),    (2, "ducenty"), (3, "trycenty")
  ,(4, "kwadryge"), (5, "kwinge"),  (6, "sescenty")
  ,(7, "septynge"), (8, "oktynge"), (9, "nonge")]

-- data written by Maciej Kucharski

aud = Waluta "dolar australijski" "dolary australijskie" "dolarów australijskich" Meski
bgn = Waluta "lew" "lewy" "lewów" Meski
brl = Waluta "real" "reale" "reali" Meski
byr = Waluta "rubel białoruski" "ruble białoruskie" "rubli białoruskich" Meski
cad = Waluta "dolar kanadyjski" "dolary kanadyjskie" "dolarów kanadyjskich" Meski
chf = Waluta "frank szwajcarski" "franki szwajcarskie" "franków szwajcarskich" Meski
cny = Waluta "yuan renminbi" "yuany renminbi" "yuanów renminbi" Meski
czk = Waluta "korona czeska" "korony czeskie" "koron czeskich" Zenski
dkk = Waluta "korona duńska" "korony duńskie" "koron duńskich" Zenski
eur = Waluta "euro" "euro" "euro" Nijaki
gbp = Waluta "funt szterling" "funty szterlingi" "funtów szterlingów" Meski
hkd = Waluta "dolar Hongkongu" "dolary Hongkongu" "dolarów Hongkongu" Meski
hrk = Waluta "kuna" "kuny" "kun" Zenski
huf = Waluta "forint" "forinty" "forintów" Meski
idr = Waluta "rupia indonezyjska" "rupie indonezyjskie" "rupii indonezyjskich" Zenski
isk = Waluta "korona islandzka" "korony islandzkie" "koron islandzkich" Zenski
jpy = Waluta "jen" "jeny" "jenów" Meski
krw = Waluta "won południowokoreański" "wony południowokoreańskie" "wonów południowokoreańskich" Meski
mxn = Waluta "peso meksykańskie" "peso meksykańskie" "peso meksykańskich" Nijaki
myr = Waluta "ringgit" "ringgity" "ringgitów" Meski
nok = Waluta "korona norweska" "korony norweskie" "koron norweskich" Zenski
nzd = Waluta "dolar nowozelandzki" "dolary nowozelandzkie" "dolarów nowozelandzkich" Meski
php = Waluta "peso filipińskie" "peso filipińskie" "peso filipińskich" Nijaki
pln = Waluta "złoty" "złote" "złotych" Meski
ron = Waluta "lej rumuński" "leje rumuńskie" "lei rumuńskich" Meski
rub = Waluta "rubel rosyjski" "ruble rosyjskie" "rubli rosyjskich" Meski
sdr = Waluta "specjalne prawo ciągnienia" "specjalne prawa ciągnienia" "specjalnych praw ciągnienia" Nijaki
sek = Waluta "korona szwedzka" "korony szwedzkie" "koron szwedzkich" Zenski
sgd = Waluta "dolar singapurski" "dolary singapurskie" "dolarów singapurskich" Meski
thb = Waluta "bat" "baty" "batów" Meski
try = Waluta "lira turecka" "liry tureckie" "lir tureckich" Zenski
uah = Waluta "hrywna" "hrywny" "hrywien" Zenski
usd = Waluta "dolar amerykański" "dolary amerykańskie" "dolarów amerykańskich" Meski
zar = Waluta "rand" "randy" "randów" Meski
