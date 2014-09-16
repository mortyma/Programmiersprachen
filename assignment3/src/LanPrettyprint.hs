-----------------------------------------------------------------------------
--
-- Module      :  LanPrettyprint
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

module LanPrettyprint where

import LanTokenizer

colorReset      = "\x1b[0m"
colorProc       = "\x1b[30;1m"
colorVar        = "\x1b[35m"
colorInVar      = "\x1b[35;1m"
colorBlock      = "\x1b[38;5;150m"
colorUnfinished = "\x1b[4m"
colorError      = "\x1b[41m"
colorReserved   = "\x1b[34m"
colorString     = "\x1b[32m"


pretty :: String -> String
pretty x = foldl (\e f -> e ++ prettyToken f) [] $ parseProc $ tokenize x


-- | prettify tokens
prettyToken :: Token -> String
prettyToken (Token UnknownToken v) = colorError ++ v ++ colorReset
prettyToken (Token OpenVariable v) = colorUnfinished ++ colorVar ++ v ++ colorReset
prettyToken (Token (ErrorToken (Token _ v)) _) = colorError ++ v ++ colorReset
prettyToken (Token BlockStart v) = colorBlock ++ v ++ colorReset
prettyToken (Token BlockEnd v) = colorBlock ++ v ++ colorReset
prettyToken (Token ReservedToken v) = colorReserved ++ v ++ colorReset
prettyToken (Token ProcName v) = colorProc ++ v ++ colorReset
prettyToken (Token ProcVar v) = colorVar ++ v ++ colorReset
prettyToken (Token Variable v) = colorInVar ++ v ++ colorReset
prettyToken (Token SubString v) = colorString ++ v ++ colorReset
prettyToken (Token StringEnd v) = colorString ++ v ++ colorReset
prettyToken (Token (String l) _ ) = colorString ++ foldl (\e f -> e ++ prettyToken f) "" l ++ colorReset
prettyToken (Token (OpenString l) _ ) = colorUnfinished ++ colorString ++  foldl (\e f -> e ++ prettyTokenOpenString f) "" l ++ colorReset
prettyToken (Token _ v) = v


-- | prettify inside strings
prettyTokenOpenString :: Token -> String
prettyTokenOpenString (Token SubString v) = colorUnfinished ++ colorString ++ v ++ colorReset
prettyTokenOpenString (Token OpenVariable v) = colorUnfinished ++ colorVar ++ v ++ colorReset
prettyTokenOpenString (Token Variable v) = colorUnfinished ++ colorInVar ++ v ++ colorReset
prettyTokenOpenString (Token _ v) = v


-- TODO: löschen wenn fertig!
parse :: [Token] -> [Token]
parse [] = []
parse ((Token Name v):xs)
  | v == "exec" || v == "split" || v == "finally" = (Token ReservedToken v) : parse xs
parse (x:xs) = x : parse xs


-- | input: if min one was found: expect a name token, else mark it as error and try again, continue with input vars
parseProc :: [Token] -> [Token]
parseProc [] = []
parseProc ((Token Name v):xs) = (Token ProcName v) : parseProcIn xs False
parseProc ((Token t v):xs) = (Token (ErrorToken (Token t v)) v) : parseProc xs


--TODO: verwendung prüfen
-- | input, found-flag: expect one or more names, else continue with delimiter
parseProcIn :: [Token] -> Bool -> [Token]
parseProcIn [] _                          = []
parseProcIn ((Token WhiteSpace v):xs) b   = (Token WhiteSpace v) : parseProcIn xs b -- continue
parseProcIn ((Token Name v):xs) _         = (Token ProcVar v) : parseProcIn xs True -- continue
parseProcIn ((Token ProcDelim v):xs) True = (Token ProcDelim v) : parseProcOut xs False -- next step
parseProcIn (x:xs) b                      = (Token (ErrorToken x) "name expected") : parseProcIn xs b -- failure and continue


--TODO: verwendung prüfen
-- | input, found-flag: expect one or more strings
parseProcOut :: [Token] -> Bool -> [Token]
parseProcOut [] _                         = []
parseProcOut ((Token WhiteSpace v):xs) b  = (Token WhiteSpace v) : parseProcOut xs b -- continue
parseProcOut ((Token StringStart v):xs) _ = let t = parseString xs [(Token StringStart v)] in (fst t) : parseProcOut (snd t) True -- parse string
parseProcOut ((Token BlockStart v):xs) True = (Token BlockStart v) : parse xs -- next step
parseProcOut (x:xs) b                     = (Token (ErrorToken x) "name expected") : parseProcOut xs b -- failure and continue


-- TODO: variablen verwendung prüfen
-- | input, tokenbuffer returns (new token, remaining tokens): collect all string parts
parseString :: [Token] -> [Token] -> (Token, [Token])
parseString [] b                          = ((Token (OpenString b) "string open"), []) -- not closed
parseString ((Token StringEnd v):xs) b    = ((Token (String (b ++ [(Token StringEnd v)])) ""), xs) -- return string
parseString ((Token SubString v):xs) b    = parseString xs $ b ++ [(Token SubString v)] -- continue
parseString ((Token Variable v):xs) b     = parseString xs $ b ++ [(Token Variable v)] -- continue
parseString ((Token OpenVariable v):xs) b = parseString xs $ b ++ [(Token OpenVariable v)] -- continue
parseString (x:xs) b    = parseString xs $ b ++ [(Token (ErrorToken x) "unknown token")] -- should not happen


