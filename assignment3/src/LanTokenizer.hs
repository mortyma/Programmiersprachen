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
data TType =  BlockStart | BlockEnd | StringStart | StringEnd | SubString | Variable | OpenVariable | ProcDelim | GuardDelim | Assign | Equals | NotEquals | Name | CommandEnd deriving (Eq,Show)
data Token = Token TType TValue deriving (Eq,Show)


-- | TODO: whitespaces, so i can rebuild the text...
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
  | otherwise = tokenize xs -- ^ for now ignore whatever is unknown

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
  | otherwise = ((x:xs), Token Name b)

