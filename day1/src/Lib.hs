module Lib ( day01, solve1, solve2) where
import Data.Char

day01 :: IO ()
day01 = putStrLn "Advent of Code day 1. Run `stack test`."

solve1 :: [String] -> Int
solve1 = sum . map assembleIntStrict

solve2 :: [String] -> Int
solve2 = sum . map assembleInt

assembleIntStrict :: String -> Int
assembleIntStrict x = read ([getFirstDigitStrict x] ++ [getFirstDigitStrict $ reverse x])

assembleInt :: String -> Int
assembleInt x = read ([getFirstDigit x] ++ [getFirstDigitReversed $ reverse x])

getFirstDigitStrict :: String -> Char
getFirstDigitStrict [] = error "Invalid input"
getFirstDigitStrict (x:xs)
  | isDigit x = x
  | otherwise = getFirstDigitStrict xs

getFirstDigit :: String -> Char
getFirstDigit [] = error "Invalid input"
getFirstDigit ('o':'n':'e':_) = '1'
getFirstDigit ('t':'w':'o':_) = '2'
getFirstDigit ('t':'h':'r':'e':'e':_) = '3'
getFirstDigit ('f':'o':'u':'r':_) = '4'
getFirstDigit ('f':'i':'v':'e':_) = '5'
getFirstDigit ('s':'i':'x':_) = '6'
getFirstDigit ('s':'e':'v':'e':'n':_) = '7'
getFirstDigit ('e':'i':'g':'h':'t':_) = '8'
getFirstDigit ('n':'i':'n':'e':_) = '9'
getFirstDigit (x:xs)
  | isDigit x = x
  | otherwise = getFirstDigit xs

getFirstDigitReversed :: String -> Char
getFirstDigitReversed [] = error "Invalid input"
getFirstDigitReversed ('e':'n':'o':_) = '1'
getFirstDigitReversed ('o':'w':'t':_) = '2'
getFirstDigitReversed ('e':'e':'r':'h':'t':_) = '3'
getFirstDigitReversed ('r':'u':'o':'f':_) = '4'
getFirstDigitReversed ('e':'v':'i':'f':_) = '5'
getFirstDigitReversed ('x':'i':'s':_) = '6'
getFirstDigitReversed ('n':'e':'v':'e':'s':_) = '7'
getFirstDigitReversed ('t':'h':'g':'i':'e':_) = '8'
getFirstDigitReversed ('e':'n':'i':'n':_) = '9'
getFirstDigitReversed (x:xs)
  | isDigit x = x
  | otherwise = getFirstDigitReversed xs
