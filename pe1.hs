module PE1 where

import Text.Printf


-- This function takes a Double and rounds it to 2 decimal places as requested in the PE --
getRounded :: Double -> Double
getRounded x = read s :: Double
               where s = printf "%.2f" x

-------------------------------------------------------------------------------------------
----------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------

convertTL :: Double -> String -> Double
convertTL money currency 
                    | currency == "USD" = getRounded (money / 8.18)
                    | currency == "EUR" = getRounded (money / 9.62)
                    | currency == "BTC" = getRounded (money / 473497.31)

-------------------------------------------------------------------------------------------

countOnWatch :: [String] -> String -> Int -> Int
countOnWatch schedule employee days = cnt (filt schedule employee days)

filt :: [String] -> String -> Int -> [String]
filt lst employee days = filter (== employee) (take days lst)

cnt :: [String] -> Int
cnt lst = length lst
-------------------------------------------------------------------------------------------

encrypt :: Int -> Int
encrypt x = toNumber ([ops(x) | x <- digits(x)])

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

ops :: Int -> Int
ops x 
    | ((x `mod` 3) == 0) = x-1 
    | ((x `mod` 4) == 0 && x*2 < 10) = x*2 
    | ((x `mod` 4) == 0 && x*2 >= 10) = (x*2) `mod` 10 
    | ((x `mod` 5) == 0 && x+3 < 10) = x+3 
    | ((x `mod` 5) == 0 && x+3 < 10) = (x+3) `mod` 10 
    | otherwise = if x+4 >= 10 then (x+4) `mod` 10
                  else x+4
    
toNumber :: [Int] -> Int    
toNumber lst = (lst !! 0 * 1000) + (lst !! 1 * 100) + (lst !! 2 * 10) + (lst !! 3)
-------------------------------------------------------------------------------------------

compoundInterests :: [(Double, Int)] -> [Double]
compoundInterests investments = [calc x | x <-  investments] 
                            
                            
calc :: (Double, Int) -> Double
calc inv 
        | (fst inv) >= 10000 && (snd inv) >= 2 = getRounded ((fst inv)*(1+(0.115/12))^(12*(snd inv)))
        | (fst inv) >= 10000 && (snd inv) < 2 = getRounded ((fst inv)*(1+(0.105/12))^(12*(snd inv)))
        | (fst inv) < 10000 && (snd inv) >= 2 = getRounded ((fst inv)*(1+(0.095/12))^(12*(snd inv)))
        | (fst inv) < 10000 && (snd inv) < 2 = getRounded ((fst inv)*(1+(0.090/12))^(12*(snd inv)))
        
                            
                            
                            
