-----------------------------------------------------------------------------
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

type TValue = String
data TType =  ErrorToken Token | ReservedToken | UnknownToken | WhiteSpace | BlockStart | BlockEnd | StringStart | StringEnd | SubString | Variable | OpenVariable | ProcDelim | GuardDelim | Assign | Equals | NotEquals | Name | CommandEnd deriving (Eq,Show)
data Token = Token TType TValue deriving (Eq,Show)


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
  | x == '"' = let p = tokenizeSubString xs "" [] in (Token StringStart "\"") : (snd p) ++ tokenize (fst p)
  | x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' = let p = tokenizeName xs [x] in (snd p) : tokenize (fst p)
  | x == ' ' || x == '\t' || x == '\n' = let p = tokenizeWhiteSpace xs [x] in (snd p) : tokenize (fst p)
  | otherwise = let p = tokenizeUnknown xs [x] in (snd p) : tokenize (fst p)

-- | takes input string, substring buffer, string tokens and returns (remaining string, [found tokens])
tokenizeSubString :: String -> String -> [Token] -> (String, [Token])
tokenizeSubString "" b t = ("", t ++ [Token SubString b])
tokenizeSubString (x:xs) b t
  | x == '"' = (xs, t ++ [Token SubString b, Token StringEnd "\""] )
  | x == '$' = let v = tokenizeVariable xs "" in tokenizeSubString (fst v) "" (t ++ [Token SubString b] ++ [snd v])
  | otherwise = tokenizeSubString xs (b ++ [x]) t

-- | takes input string, var name buffer and returns (remaining string, found token)
tokenizeVariable :: String -> String -> (String, Token)
tokenizeVariable "" b = ("", Token OpenVariable ("$" ++b))
tokenizeVariable (x:xs) b
  | x == '$' = (xs, Token Variable ("$" ++ b ++ "$"))
  | x == '"' = (xs, Token OpenVariable ("$" ++ b))
  | otherwise = tokenizeVariable xs (b ++ [x])

-- | takes input string, name buffer and returns (remaining string, found token)
tokenizeName :: String -> String -> (String, Token)
tokenizeName "" b = ("", Token Name b)
tokenizeName (x:xs) b
  | x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' || x >= '0' && x <= '9' = tokenizeName xs (b ++ [x])
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
