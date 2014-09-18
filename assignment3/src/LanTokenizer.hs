
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

type TValue = String
data TType =  UnknownToken | WhiteSpace
  | BlockStart | BlockEnd | StringStart | StringEnd | SubString
  | Variable | OpenVariable
  | ProcDelim | GuardDelim | Assign | Equals | NotEquals | Name | CommandEnd deriving (Eq, Show, Ord)
data Token = Token TType TValue deriving (Eq,Show,Ord)


-- | splits a string into a list of tokens
tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs)
  | x == '=' && length xs > 0 && head xs == '=' = (Token Equals "==") : tokenize (tail xs)
  | x == '=' = (Token Assign "=") : tokenize xs
  | x == '!' && length xs > 0 && head xs == '=' = (Token NotEquals "!=") : tokenize (tail xs)
  | x == '-' = (Token ProcDelim "-") : tokenize xs
  | x == ':' = (Token GuardDelim ":") : tokenize xs
  | x == ';' = (Token CommandEnd ";") : tokenize xs
  | x == '{' = (Token BlockStart "{") : tokenize xs
  | x == '}' = (Token BlockEnd "}") : tokenize xs
  | x == '"' = let (remaining, tokens) = tokenizeSubString xs "" [] in (Token StringStart "\"") : tokens ++ tokenize remaining
  | isAlphaNum x = let (remaining, tokens) = tokenizeName xs [x] in tokens : tokenize remaining
  | isSpace x = let (remaining, tokens) = tokenizeWhiteSpace xs [x] in tokens : tokenize remaining
  | otherwise = let (remaining, tokens) = tokenizeUnknown xs [x] in tokens : tokenize remaining

subStringTokensForBuffer :: String -> [Token]
subStringTokensForBuffer "" = []
subStringTokensForBuffer b = [Token SubString b]

-- | takes input string, substring buffer, string tokens and returns (remaining string, [found tokens])
tokenizeSubString :: String -> String -> [Token] -> (String, [Token])
tokenizeSubString "" b t = ("", t ++ subStringTokensForBuffer b) 
tokenizeSubString (x:xs) b t
  | x == '"' = (xs, t ++ subStringTokensForBuffer b ++ [Token StringEnd "\""] )
  | x == '$' = let (remaining, tokens) = tokenizeVariable xs "" in tokenizeSubString remaining "" (t ++ subStringTokensForBuffer b ++ [tokens])
  | x == '\\' && length xs > 0 = tokenizeSubString (tail xs) (b ++ [x, head xs]) t
  | otherwise = tokenizeSubString xs (b ++ [x]) t

-- | takes input string, var name buffer and returns (remaining string, found token)
tokenizeVariable :: String -> String -> (String, Token)
tokenizeVariable "" b = ("", Token OpenVariable ("$" ++b))
tokenizeVariable (x:xs) b
  | x == '$' = (xs, Token Variable ("$" ++ b ++ "$"))
  | x == '"' = (x:xs, Token OpenVariable ("$" ++ b))
--  | x == '\\' && length xs > 0 && (head xs == '$' || head xs == '"') = tokenizeVariable (tail xs) (b ++ [x, head xs])
  | otherwise = tokenizeVariable xs (b ++ [x])

-- | takes input string, name buffer and returns (remaining string, found token)
tokenizeName :: String -> String -> (String, Token)
tokenizeName "" b = ("", Token Name b)
tokenizeName (x:xs) b
  | isAlphaNum x = tokenizeName xs (b ++ [x])
  | otherwise = (x:xs, Token Name b)

tokenizeWhiteSpace :: String -> String -> (String, Token)
tokenizeWhiteSpace "" b = ("", Token WhiteSpace b);
tokenizeWhiteSpace (x:xs) b
  | x == ' ' || x == '\t' || x == '\n' = tokenizeWhiteSpace xs (b ++ [x])
  | otherwise = (x:xs, Token WhiteSpace b)

tokenizeUnknown :: String -> String -> (String, Token)
tokenizeUnknown "" b = ("", Token UnknownToken b);
tokenizeUnknown (x:xs) b
  | x == ' ' || x == '\t' || x == '\n' = (x:xs, Token UnknownToken b)
  | otherwise = tokenizeUnknown xs (b ++ [x])



