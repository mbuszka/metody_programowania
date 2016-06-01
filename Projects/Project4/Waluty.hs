module Waluty (Waluta(..), Rodzaj(..), parseCurrency) where

import Data.Char(toLower)

data Rodzaj = Meski  | Zenski | Nijaki deriving Show

data Waluta = Waluta {
  mianownikPoj :: String,
  mianownikMn  :: String,
  dopelniaczMn :: String,
  rodzaj       :: Rodzaj
} deriving Show

-- data written by Maciej Kucharski

parseCurrency :: String -> Maybe Waluta
parseCurrency s = case map toLower s of
  "aud" -> Just $ Waluta "dolar australijski" "dolary australijskie" "dolarów australijskich" Meski
  "bgn" -> Just $ Waluta "lew" "lewy" "lewów" Meski
  "brl" -> Just $ Waluta "real" "reale" "reali" Meski
  "byr" -> Just $ Waluta "rubel białoruski" "ruble białoruskie" "rubli białoruskich" Meski
  "cad" -> Just $ Waluta "dolar kanadyjski" "dolary kanadyjskie" "dolarów kanadyjskich" Meski
  "chf" -> Just $ Waluta "frank szwajcarski" "franki szwajcarskie" "franków szwajcarskich" Meski
  "cny" -> Just $ Waluta "yuan renminbi" "yuany renminbi" "yuanów renminbi" Meski
  "czk" -> Just $ Waluta "korona czeska" "korony czeskie" "koron czeskich" Zenski
  "dkk" -> Just $ Waluta "korona duńska" "korony duńskie" "koron duńskich" Zenski
  "eur" -> Just $ Waluta "euro" "euro" "euro" Nijaki
  "gbp" -> Just $ Waluta "funt szterling" "funty szterlingi" "funtów szterlingów" Meski
  "hkd" -> Just $ Waluta "dolar Hongkongu" "dolary Hongkongu" "dolarów Hongkongu" Meski
  "hrk" -> Just $ Waluta "kuna" "kuny" "kun" Zenski
  "huf" -> Just $ Waluta "forint" "forinty" "forintów" Meski
  "idr" -> Just $ Waluta "rupia indonezyjska" "rupie indonezyjskie" "rupii indonezyjskich" Zenski
  "isk" -> Just $ Waluta "korona islandzka" "korony islandzkie" "koron islandzkich" Zenski
  "jpy" -> Just $ Waluta "jen" "jeny" "jenów" Meski
  "krw" -> Just $ Waluta "won południowokoreański" "wony południowokoreańskie" "wonów południowokoreańskich" Meski
  "mxn" -> Just $ Waluta "peso meksykańskie" "peso meksykańskie" "peso meksykańskich" Nijaki
  "myr" -> Just $ Waluta "ringgit" "ringgity" "ringgitów" Meski
  "nok" -> Just $ Waluta "korona norweska" "korony norweskie" "koron norweskich" Zenski
  "nzd" -> Just $ Waluta "dolar nowozelandzki" "dolary nowozelandzkie" "dolarów nowozelandzkich" Meski
  "php" -> Just $ Waluta "peso filipińskie" "peso filipińskie" "peso filipińskich" Nijaki
  "pln" -> Just $ Waluta "złoty" "złote" "złotych" Meski
  "ron" -> Just $ Waluta "lej rumuński" "leje rumuńskie" "lei rumuńskich" Meski
  "rub" -> Just $ Waluta "rubel rosyjski" "ruble rosyjskie" "rubli rosyjskich" Meski
  "sdr" -> Just $ Waluta "specjalne prawo ciągnienia" "specjalne prawa ciągnienia" "specjalnych praw ciągnienia" Nijaki
  "sek" -> Just $ Waluta "korona szwedzka" "korony szwedzkie" "koron szwedzkich" Zenski
  "sgd" -> Just $ Waluta "dolar singapurski" "dolary singapurskie" "dolarów singapurskich" Meski
  "thb" -> Just $ Waluta "bat" "baty" "batów" Meski
  "try" -> Just $ Waluta "lira turecka" "liry tureckie" "lir tureckich" Zenski
  "uah" -> Just $ Waluta "hrywna" "hrywny" "hrywien" Zenski
  "usd" -> Just $ Waluta "dolar amerykański" "dolary amerykańskie" "dolarów amerykańskich" Meski
  "zar" -> Just $ Waluta "rand" "randy" "randów" Meski
  _     -> Nothing
