--
-- Module      :  LanTokenizer
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module LanTokenizer where

import Data.Char

data TType =  Unknown | WhiteSpace | Comment
  | BlockStart | BlockEnd | StringStart | StringEnd | SubString
  | VariableStart | VariableEnd | Variable
  | ProcDelim | GuardDelim | Assign | Equals | NotEquals | Name | CommandEnd deriving (Eq, Show, Ord)
data Token = Token TType String deriving (Eq,Show,Ord)

-- | splits a string into a list of tokens
tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs)
  | x == '=' && not (null xs) && head xs == '=' = (Token Equals "==") : tokenize (tail xs)
  | x == '=' = (Token Assign "=") : tokenize xs
  | x == '!' && not (null xs) && head xs == '=' = (Token NotEquals "!=") : tokenize (tail xs)
  | x == '-' = (Token ProcDelim "-") : tokenize xs
  | x == ':' = (Token GuardDelim ":") : tokenize xs
  | x == ';' = (Token CommandEnd ";") : tokenize xs
  | x == '{' = (Token BlockStart "{") : tokenize xs
  | x == '}' = (Token BlockEnd "}") : tokenize xs
  | x == '"' = (Token StringStart "\"") : tokenizeString xs
  | isAlphaNum x = let (name, rest) = alNum (x:xs) in (Token Name name) : tokenize rest
  | isSpace x = let (w, rest) = space (x:xs) in (Token WhiteSpace w) : tokenize rest
  | x == '#' = let (w, rest) = comment (x:xs) in (Token Comment w) : tokenize rest
  | otherwise = (Token Unknown [x]) : tokenize xs

-- | inside of a string
tokenizeString :: String -> [Token]
tokenizeString "" = []
tokenizeString (x:xs)
  -- | x == '"' = (Token StringStart [x]) : tokenizeSubString xs
  -- | otherwise = (Token Unknown [x]) : tokenize xs
  -- where tokenizeSubString (x:xs)
  | x == '$' = (Token VariableStart "$") : tokenizeVariable xs
  | x == '"' = (Token StringEnd "\"") : tokenize xs
  | otherwise = let (s,s') = subString (x:xs) in (Token SubString s) : tokenizeString s'

-- | inside of a variable (inside a string)
tokenizeVariable :: String -> [Token]
tokenizeVariable "" = []
tokenizeVariable (x:xs)
  | x == '$' = (Token VariableEnd "$"): tokenizeString xs
  | isAlphaNum x = let (s,s') = alNum (x:xs) in (Token Variable s) : tokenizeVariable s'
  | otherwise = tokenizeString (x:xs)

-- | returns longest matching prefix and remainder
matchWhile :: (a -> Bool) -> [a] -> ([a],[a])
matchWhile f [] = ([],[])
matchWhile f (x:xs)
    | f x = let (s,s') = matchWhile f xs in (x:s,s')
    | otherwise = ([],x:xs)

alNum :: String -> (String, String)
alNum = matchWhile isAlphaNum

space = matchWhile isSpace

comment = matchWhile (\c -> c /= '\n')

-- | returns substring and remainder
subString :: String -> (String, String)
subString (x:xs)
  | x == '\\' && not (null xs) = let (s,r) = subString (tail xs) in (x:head xs:s, r)
  | x == '"' || x == '$' = ("",x:xs)
  | otherwise = let (s,r) = subString xs in (x:s, r)
subString "" = ("","")
