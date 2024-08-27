module Main where

import Data.Ratio
import GHC.Unicode (isDigit)
import System.IO (isEOF)
import Data.List (intercalate)

readNumbersImpl :: String -> String -> [Integer]
readNumbersImpl "" "" = []
readNumbersImpl s "" = [read $ reverse s]
readNumbersImpl s (x:xs) | ((isDigit x) || (x == '-')) = readNumbersImpl (x:s) xs
                         | otherwise = (read $ reverse s :: Integer):(readNumbersImpl [] xs)

readNumbers :: String -> [Integer]
readNumbers = readNumbersImpl ""

explore :: [Ratio Integer] -> [Ratio Integer]
explore (x:xs) = x:(explore xs)
explore [] = (0 % 1):(explore [])

sumGF :: [Ratio Integer] -> [Ratio Integer] -> [Ratio Integer]
sumGF x y = map (uncurry (+)) $ zip x y

prodGFImpl :: [Ratio Integer] -> [Ratio Integer] -> [Ratio Integer] -> [Ratio Integer] -> [Ratio Integer]
prodGFImpl xh yh (x:xs) (y:ys) = (foldl (+) (0 % 1) . map (uncurry (*)) . zip (x:xh) $ reverse (y:yh)):(prodGFImpl (x:xh) (y:yh) xs ys)

prodGF :: [Ratio Integer] -> [Ratio Integer] -> [Ratio Integer]
prodGF = prodGFImpl [] []

makeFElem :: Ratio Integer -> Integer -> Integer -> [Ratio Integer]
makeFElem p ci k = foldl prodGF (explore [p]) . map (\i -> explore [(i - ci) % i, 1 % i]) $ [1..k]

makeF :: Integer -> Integer -> [Ratio Integer] -> [Ratio Integer]
makeF r k p = foldl sumGF (explore [0 % 1]) . map (\(pd, i) -> makeFElem (pd * (1 % (r ^ i))) i k) $ zip (take (fromIntegral (k + 1)) p) [0..k]

inputLines :: IO [String]
inputLines = fmap reverse (go []) where 
        go xs = do  
            done <- isEOF
            if (done)
                then (return xs)
                else do 
                    inp <- getLine 
                    go (inp:xs)
  
main = do
    input <- inputLines
    let [r, k] = take 2 . readNumbers $ head input
    let p = explore . map (\x -> (x % 1)) . readNumbers . head . tail $ input
    putStrLn (intercalate " " . map (\x -> (show $ numerator x) ++ "/" ++ (show $ denominator x)) . take (fromIntegral (k + 1)) $ makeF r k p)