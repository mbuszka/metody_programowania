module Numbers(oneNineteen
              ,one
              ,two
              ,latinSmall
              ,latinOneNine
              ,latinTenNinety
              ,latinHundredNinehundred
              ) where

import qualified Data.Map as M
import           Waluty(Rodzaj(..))

one :: Rodzaj -> String
one r = case r of
  Zenski -> "jedna"
  Meski  -> "jeden"
  Nijaki -> "jedno"

two :: Rodzaj ->  String
two r = case r of
  Zenski -> "dwie"
  _      -> "dwa"

oneNineteen :: M.Map Integer String
oneNineteen = M.fromList
  [(1, "jeden"),       (2, "dwa"),           (3, "trzy")
  ,(4, "cztery"),      (5, "pięć"),          (6, "sześć")
  ,(7, "siedem"),      (8, "osiem"),         (9, "dziewięć")
  ,(10, "dziesięć"),   (11, "jedenaście"),   (12, "dwanaście")
  ,(13, "trzynaście"), (14, "czternaście"),  (15, "piętnaście")
  ,(16, "szesnaście"), (17, "siedemnaście"), (18, "osiemnaście")
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
