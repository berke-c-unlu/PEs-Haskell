module PE2 where

---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------

-- Note: undefined is a value that causes an error when evaluated. Replace it with
-- a viable definition! Name your arguments as you like by changing the holes: _

--------------------------
-- Part I: Time to inf up!

-- naturals: The infinite list of natural numbers. That's it!
naturals :: [Integer]
naturals = [0..]

-- interleave: Interleave two lists, cutting off on the shorter list.
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave x [] = []
interleave [] y = []
interleave (x:xs) (y:ys) =  x:  y: (interleave (xs) (ys))

-- integers: The infinite list of integers. Ordered as [0, -1, 1, -2, 2, -3, 3, -4, 4...].
integers :: [Integer]
integers = interleave [0..] (map ((-1)*) [1..]) 

--------------------------------
-- Part II: SJSON Prettification

-- splitOn: Split string on first occurence of character.
splitOn :: Char -> String -> (String, String)
splitOn c [] = ("","")
splitOn c xs = do
                let first = takeWhile (/= c) xs
                let second = dropWhile (/= c) xs
                if second == [] then (first,"")
                else (first,tail second)

-- tokenizeS: Transform an SJSON string into a list of tokens.
tokenizeS :: String -> [String]
tokenizeS (x:xs)
                | length (x:xs) == 0 = []
                | length (x:xs) == 1 && x /= '\n' = [[x]] 
                | xs == [] = []
                | x == '{' = ["{"] ++ tokenizeS xs
                | x == '}' = ["}"] ++ tokenizeS xs
                | x == ':' = [":"] ++ tokenizeS xs
                | x == '.' = ["."] ++ tokenizeS xs
                | x == ',' = [","] ++ tokenizeS xs
                | x == '\'' = [fst (splitOn x xs)] ++ tokenizeS (snd (splitOn x xs))
                | otherwise = tokenizeS xs

-- prettifyS: Prettify SJSON, better tokenize first!
prettifyS :: String -> String
prettifyS str = prettify_helper (tokenizeS str) 0
          
       
prettify_helper [] _ = ""          
prettify_helper (x:xs) ind_level = if x == "{" 
                                    then 
                                    "{" 
                                    ++ "\n" 
                                    ++ [' ' |x <- [1..4 * (ind_level + 1)]] 
                                    ++ prettify_helper (xs) (ind_level + 1)
                                else if x == "}" 
                                    then 
                                    "\n"  
                                    ++ [' ' |x <- [1..4 * (ind_level - 1)]] 
                                    ++ "}" 
                                    ++ prettify_helper (xs) (ind_level - 1)
                                else if x == "," 
                                    then 
                                    "," 
                                    ++ "\n" 
                                    ++ [' ' |x <- [1..4 * (ind_level)]] 
                                    ++ prettify_helper (xs) (ind_level)
                                else if x == ":" 
                                    then
                                    ":" 
                                    ++ " " 
                                    ++ prettify_helper (xs) (ind_level)
                                else 
                                    "'" 
                                    ++ x 
                                    ++ "'" 
                                    ++ prettify_helper (xs) (ind_level)


-- Good luck to you, friend and colleague!

