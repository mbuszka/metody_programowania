module Main where

import System.Environment(getArgs)
import Waluty(Waluta(..), parseCurrency)
import Slownie(slownie)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
  then
    putStrLn "użycie programu:\n\nslownie <liczba> <waluta>\n"
  else
    let n = read $ head args
        c = parseCurrency $ args !! 1
    in case c of
      Just currency -> putStrLn $ slownie currency n
      Nothing       -> putStrLn "nieznana waluta"
  return ()
