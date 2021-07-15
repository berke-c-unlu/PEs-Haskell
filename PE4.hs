module PE4 where

import Data.Maybe -- up to you if you want to use it or not

-- Generic DictTree definition with two type arguments
data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show

-- Lightweight Char wrapper as a 'safe' Digit type
newtype Digit = Digit Char deriving (Show, Eq, Ord) -- derive equality and comparison too!

-- Type aliases
type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]


---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------


----------
-- Part I:
-- Some Maybe fun! Basic practice with an existing custom datatype.

-- toDigit: Safely convert a character to a digit
toDigit :: Char -> Maybe Digit
toDigit c = do
                let lst = ['0','1','2','3','4','5','6','7','8','9']
                if elem c lst then Just (Digit c)
                else Nothing
                
-- toDigits: Safely convert a bunch of characters to a list of digits.
--           Particularly, an empty string should fail.
toDigits :: String -> Maybe PhoneNumber
toDigits [] = Nothing
toDigits lst = if length lst > length (toDigits' lst) then Nothing
               else Just (toDigits' lst)
                    
toDigits' :: String -> [Digit]
toDigits' [] = []
toDigits' (x:xs) = do
                    let lst = ['0','1','2','3','4','5','6','7','8','9']
                    if elem x lst then (Digit x):toDigits' xs
                    else toDigits' xs
-----------
-- Part II:
-- Some phonebook business.

-- numContacts: Count the number of contacts in the phonebook...
numContacts :: DigitTree -> Int
numContacts (Leaf v) = 1
numContacts (Node []) = 0
numContacts (Node (x:xs)) = numContacts (snd (x)) + numContacts (Node xs)

-- getContacts: Generate the contacts and their phone numbers in order given a tree. 
getContacts :: DigitTree -> [(PhoneNumber, String)]
getContacts tree = (concat' (helper2 [] tree) (helper tree) )

-- finds names at the leaves
helper :: DigitTree -> [String]
helper (Leaf v) = [v]
helper (Node[]) = []
helper (Node (x:xs)) = helper (snd(x)) ++ helper (Node xs)

-- finds all possible paths
helper2:: [Digit] -> DigitTree -> [[Digit]]
helper2 lst (Leaf v) = [lst]
helper2 lst (Node[]) = []
helper2 lst (Node (x:xs)) = helper2 (lst++[fst(x)]) (snd(x))  ++ helper2 lst (Node xs)

concat' :: [[Digit]] -> [String] -> [(PhoneNumber,String)]
concat' digits names = zip digits names

-- autocomplete: Create an autocomplete list of contacts given a prefix
-- e.g. autocomplete "32" areaCodes -> 
--      [([Digit '2'], "Adana"), ([Digit '6'], "Hatay"), ([Digit '8'], "Osmaniye")]
autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
autocomplete str (Node (x:xs)) = if (toDigits str) == Nothing then []
                                 else getContacts(autocomplete' (toDigits' str) (Node (x:xs)))

autocomplete' :: [Digit] -> DigitTree -> DigitTree
autocomplete' str (Leaf x) = (Node[])
autocomplete' str (Node[]) = (Node[])
autocomplete' str (Node(x:xs))
                                | length str == 0 = Node[]
                                | length str == 1 && str !! 0 == fst(x) = (snd(x))
                                | length str == 1 && str !! 0 /= fst(x) = (autocomplete' str (Node xs))
                                | otherwise = if str !! 0 == fst(x) then (autocomplete' (tail str) (snd(x)))
                                              else  (autocomplete' (str) (Node xs))
                                            
-----------
-- Example Trees
-- Two example trees to play around with, including THE exampleTree from the text. 
-- Feel free to delete these or change their names or whatever!

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]

