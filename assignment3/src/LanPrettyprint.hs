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

import qualified Data.Map as Map
import LanTokenizer

colorReset      = "\x1b[0m"
colorProc       = "\x1b[30;1m"
colorVar        = "\x1b[35m"
colorInVar      = "\x1b[35;1m"
colorBlock      = "\x1b[38;5;150m"
colorUnfinished = "\x1b[4m"
colorError      = "\x1b[41m"
colorUnused     = "\x1b[43m"
colorReserved   = "\x1b[34m"
colorString     = "\x1b[32m"


pretty :: String -> String
pretty x = foldl (\e f -> e ++ prettyToken f) [] $ let t = parseProc $ tokenize x in markToken t "" (findToken2 t "" (findToken t "" Map.empty))


-- | prettify tokens
prettyToken :: Token -> String
prettyToken (Token UnknownToken v) = colorError ++ v ++ colorReset
prettyToken (Token OpenVariable v) = colorUnfinished ++ colorVar ++ v ++ colorReset
prettyToken (Token (ErrorToken (Token _ v)) _) = colorError ++ v ++ colorReset
-- prettyToken (Token (UnusedVar (Token _ v)) _) = colorUnused ++ v ++ colorReset -- does not work right :/
prettyToken (Token BlockStart v) = colorBlock ++ v ++ colorReset
prettyToken (Token BlockEnd v) = colorBlock ++ v ++ colorReset
prettyToken (Token ReservedToken v) = colorReserved ++ v ++ colorReset
prettyToken (Token ProcName v) = colorProc ++ v ++ colorReset
prettyToken (Token ProcVar v) = colorVar ++ v ++ colorReset
prettyToken (Token Name v) = colorVar ++ v ++ colorReset
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


-- | prefix, name
tokenKey :: String -> String -> String
tokenKey p n = p ++ "%%" ++ n

-- | input tokens, prefix, dict: look for variables(in strings), put them in a dict
findToken :: [Token] -> String -> Map.Map String Bool -> Map.Map String Bool
findToken [] _ d = d
findToken ((Token Variable v):xs) p d = findToken ((Token OpenVariable ("**" ++ tokenKey p (init (tail v)) ++ "**")):    xs) p (Map.insert (tokenKey p (init (tail v))) False d)
findToken ((Token (String l) _ ):xs) p d = findToken xs p (findToken l p d)
findToken ((Token ProcName v):xs) p d = findToken xs v d
findToken ((Token BlockEnd v):xs) p d = findToken xs "" d
findToken (x:xs) p d = findToken xs p d


-- | input tokens, prefix, dict: look for names or procvars and mark the dict entry true if the names are in it
findToken2 :: [Token] -> String -> Map.Map String Bool -> Map.Map String Bool
findToken2 [] _ d = d
findToken2 ((Token (String l) _ ):xs) p d = findToken2 xs p (findToken2 l p d)
findToken2 ((Token Name v):xs) p d = let b = Map.lookup (tokenKey p v) d in
  case b of
  Nothing -> findToken2 xs p d -- ignore (cant mark :( )
  Just x ->
    if x == False
    then findToken2 xs p (Map.insert (tokenKey p v) True d) -- mark true
    else findToken2 xs p d
findToken2 ((Token ProcVar v):xs) p d = let b = Map.lookup (tokenKey p v) d in
  case b of
  Nothing -> findToken2 xs p d -- ignore (cant mark :( )
  Just x ->
    if x == False
    then findToken2 xs p (Map.insert (tokenKey p v) True d) -- mark true
    else findToken2 xs p d
findToken2 ((Token ProcName v):xs) p d = let b = Map.lookup (tokenKey p v) d in
  case b of
  Nothing -> findToken2 xs p d -- ignore (cant mark :( )
  Just x ->
    if x == False
    then findToken2 xs p (Map.insert (tokenKey p v) True d) -- mark true
    else findToken2 xs p d
findToken2 ((Token ProcName v):xs) p d = findToken2 xs v d
findToken2 ((Token BlockEnd v):xs) p d = findToken2 xs "" d
findToken2 (x:xs) p d = findToken2 xs p d


-- | mark bound
markToken :: [Token] -> String -> Map.Map String Bool -> [Token]
markToken [] _ _ = []
markToken ((Token Name v):xs) p d = let b = Map.lookup (tokenKey p v) d in
  case b of
  Nothing -> (Token (ErrorToken (Token Name v)) "unused var"):markToken xs p d -- mark unused
  Just x ->
    if x == False
    then (Token (UnusedVar (Token Name v)) "undefined var"):markToken xs p (Map.insert (p ++ v) False d) -- not nice....
    else (Token Name v):markToken xs p d
markToken ((Token ProcVar v):xs) p d = let b = Map.lookup (tokenKey p v) d in
  case b of
  Nothing -> (Token (ErrorToken (Token ProcVar v)) "unused var"):markToken xs p d -- mark unused
  Just x ->
    if x == False
    then (Token (UnusedVar (Token ProcVar v)) "undefined var"):markToken xs p (Map.insert (p ++ v) False d) -- not nice....
    else (Token ProcVar v):markToken xs p d
markToken ((Token ProcName v):xs) p d = (Token ProcName v):markToken xs v d
markToken ((Token BlockEnd v):xs) p d = (Token BlockEnd v):markToken xs "" d
markToken (x:xs) p d = x:markToken xs p d


-- | input: if min one was found: expect a name token, else mark it as error and try again, continue with input vars
parseProc :: [Token] -> [Token]
parseProc [] = []
parseProc ((Token Name v):xs)             = (Token ProcName v) : parseProcIn xs
-- parseProc ((Token ProcDelim v):xs)        = (Token ProcDelim v) : parseProcOut xs False -- next step
parseProc (x:xs)                          = (Token (ErrorToken x) "unexpected token") : parseProc xs


--TODO: verwendung prüfen
-- | input, found-flag: possible many names, else continue with delimiter
parseProcIn :: [Token] -> [Token]
parseProcIn []                           = []
parseProcIn ((Token WhiteSpace v):xs)    = (Token WhiteSpace v) : parseProcIn xs  -- continue
parseProcIn ((Token Name v):xs)          = (Token ProcVar v) : parseProcIn xs  -- continue
parseProcIn ((Token ProcDelim v):xs)     = (Token ProcDelim v) : parseProcOut xs False -- next step
parseProcIn (x:xs)                       = (Token (ErrorToken x) "name expected") : parseProcIn xs  -- failure and continue


--TODO: verwendung prüfen
-- | input, found-flag: expect one or more strings
parseProcOut :: [Token] -> Bool -> [Token]
parseProcOut [] _                         = []
parseProcOut ((Token WhiteSpace v):xs) b  = (Token WhiteSpace v) : parseProcOut xs b -- continue
--parseProcOut ((Token StringStart v):xs) _ = let t = parseString ((Token StringStart v):xs) in (fst t) : parseProcOut (snd t) True -- parse string
parseProcOut ((Token StringStart v):xs) _ = let t = parseString xs [(Token StringStart v)] in (fst t) : parseProcOut (snd t) True -- parse string
parseProcOut ((Token BlockStart v):xs) True = (Token BlockStart v) : parseGuardOrCommand xs -- next step
parseProcOut (x:xs) b                     = (Token (ErrorToken x) "name expected") : parseProcOut xs b -- failure and continue


-- TODO: variablen verwendung prüfen
-- | input, tokenbuffer returns (new token, remaining tokens): collect all string parts
parseString :: [Token] -> [Token] -> (Token, [Token])
parseString [] b                          = ((Token (OpenString b) "string open"), []) -- not closed
parseString ((Token StringEnd v):xs) b    = ((Token (String (b ++ [(Token StringEnd v)])) ""), xs) -- return string
parseString ((Token StringStart v):xs) b  = parseString xs $ b ++ [(Token StringStart v)] -- continue
parseString ((Token WhiteSpace v):xs) b   = parseString xs $ b ++ [(Token WhiteSpace v)] -- continue
parseString ((Token SubString v):xs) b    = parseString xs $ b ++ [(Token SubString v)] -- continue
parseString ((Token Variable v):xs) b     = parseString xs $ b ++ [(Token Variable v)] -- continue
parseString ((Token OpenVariable v):xs) b = parseString xs $ b ++ [(Token OpenVariable v)] -- continue
parseString (x:xs) b                      = parseString xs $ b ++ [(Token (ErrorToken x) "unknown token")] -- should not happen


--parseGuard :: [Token] -> Bool -> [Token]
--parseGuard [] _                           = [] --TODO fehler ausgeben oder laar lossn!?
--parseGuard ((Token WhiteSpace v):xs) b    = (Token WhiteSpace v) : parseGuard xs b
--parseGuard ((Token Name v):xs) False      =
--  | v == "finally" = [(Token ReservedToken v)] -- return
--  | otherwise = (Token Name v) : parseGuard True xs -- continue
--parseGuard ((Token Equals v):xs) True     = (Token Equals v) : parseString xs []


--parseGuard :: [Token] -> [Token]
--parseGuard []                             = [] --TODO fehler ausgeben oder laar lossn!?
--parseGuard ((Token WhiteSpace v):xs)      = (Token WhiteSpace v) : parseGuard xs
--parseGuard ((Token Name v):xs)
--  | v == "finally"                        = (Token ReservedToken v) : parseGuardDelim xs -- check guarddelim
--  | otherwise                             = (Token Name v) : parseCompare xs -- check compare
--parseGuard (x:xs)                         = (Token (ErrorToken x) "unknown token") : parseGuard xs
--
--
--parseCompare :: [Token] -> [Token]
--parseCompare []                           = []
--parseCompare ((Token WhiteSpace v):xs)    = (Token WhiteSpace v) : parseCompare xs
--parseCompare ((Token Equals v):xs)        = let t = parseString xs [] in (Token Equals v) : (fst t) : parseGuardDelim (snd t)
--
--
--parseGuardDelim :: [Token] -> [Token]
--parseGuardDelim l = parse l


--parseString :: [Token] -> ([Token], [Token])
--parseString []                            = ([], []) -- not closed
--parseString ((Token WhiteSpace v):xs)     = let t = parseString xs in ((Token WhiteSpace v) : (fst t), (snd t))
--parseString ((Token StringStart v):xs)    = let t = parseString xs in ([Token (String ((Token StringStart v) : (fst t))) ""], (snd t))
--parseString ((Token Variable v):xs)       = let t = parseString xs in ((Token Variable v) : (fst t), (snd t))
--parseString ((Token OpenVariable v):xs)   = let t = parseString xs in ((Token OpenVariable v) : (fst t), (snd t))
--parseString ((Token StringEnd v):xs)      = ([(Token StringEnd v)], xs)
--parseString (x:xs)                        = ([(Token (ErrorToken x) "unknown token")], xs)


parseGuardOrCommand :: [Token] -> [Token]
parseGuardOrCommand []                          = []
parseGuardOrCommand ((Token WhiteSpace v):xs)   = (Token WhiteSpace v):parseGuardOrCommand xs
parseGuardOrCommand ((Token Name v):xs)
  | v == "finally"                              = (Token ReservedToken v):parseGuardDelim xs -- check guarddelim
  | otherwise                                   = (Token Name v):parseGuardOrCommand2 xs -- check compare
parseGuardOrCommand ((Token BlockEnd v):xs)     = (Token BlockEnd v):parseProc xs
parseGuardOrCommand (x:xs)                      = (Token (ErrorToken x) "unknown token") : parseGuardOrCommand xs


parseGuardOrCommand2 :: [Token] -> [Token]
parseGuardOrCommand2 []                         = []
parseGuardOrCommand2 ((Token WhiteSpace v):xs)  = (Token WhiteSpace v):parseGuardOrCommand2 xs
parseGuardOrCommand2 ((Token Name v):xs)        = (Token Name v):parseCommandName2 xs
parseGuardOrCommand2 ((Token Assign v):xs)      = (Token Assign v):parseCommandName1 xs
parseGuardOrCommand2 ((Token NotEquals v):xs)   = (Token NotEquals v):parseGuardEq xs
parseGuardOrCommand2 ((Token Equals v):xs)      = (Token Equals v):parseGuardEq xs
parseGuardOrCommand2 ((Token GuardDelim v):xs)  = (Token GuardDelim v):parseGuardOrCommand xs
parseGuardOrCommand2 (x:xs)                     = (Token (ErrorToken x) "unknown token") : parseGuardOrCommand2 xs


parseGuardEq :: [Token] -> [Token]
parseGuardEq []                                 = []
parseGuardEq ((Token WhiteSpace v):xs)          = (Token WhiteSpace v):parseGuardEq xs
parseGuardEq ((Token StringStart v):xs)         = let t = parseString xs [(Token StringStart v)] in (fst t) : parseGuardDelim (snd t) -- parse string -- TODO: prüfen
parseGuardEq (x:xs)                             = (Token (ErrorToken x) "unknown token") : parseGuardOrCommand2 xs


parseGuardDelim :: [Token] -> [Token]
parseGuardDelim []                              = []
parseGuardDelim ((Token WhiteSpace v):xs)       = (Token WhiteSpace v):parseGuardDelim xs
parseGuardDelim ((Token GuardDelim v):xs)       = (Token GuardDelim v):parseGuardOrCommand xs
parseGuardDelim (x:xs)                          = (Token (ErrorToken x) "unknown token") : parseGuardOrCommand2 xs



-- | exactly 1 name found (after assign)
parseCommandName1 :: [Token] -> [Token]
parseCommandName1 []                            = []
parseCommandName1 ((Token WhiteSpace v):xs)     = (Token WhiteSpace v):parseCommandName1 xs
parseCommandName1 ((Token StringStart v):xs)    = let t = parseString xs [(Token StringStart v)] in (fst t):parseCommandStrings (snd t) -- parse string -- TODO: prüfen
parseCommandName1 ((Token Name v):xs)
  | v == "exec"                                 = (Token ReservedToken v):parseCommandExec xs 0
  | otherwise                                   = (Token Name v):parseCommandName3 xs 2
parseCommandName1 (x:xs)                        = (Token (ErrorToken x) "unknown token") : parseCommandName1 xs

parseCommandStrings :: [Token] -> [Token]
parseCommandStrings []                          = []
parseCommandStrings ((Token WhiteSpace v):xs)   = (Token WhiteSpace v):parseCommandStrings xs
parseCommandStrings ((Token StringStart v):xs)  = let t = parseString xs [(Token StringStart v)] in (fst t):parseCommandStrings (snd t) -- parse string -- TODO: prüfen
parseCommandStrings ((Token CommandEnd v):xs)      = (Token CommandEnd v):parseGuardOrCommand xs
parseCommandStrings (x:xs)                      = (Token (ErrorToken x) "unknown token") : parseCommandStrings xs


-- | 2 names found
parseCommandName2 :: [Token] -> [Token]
parseCommandName2 [] = []
parseCommandName2 ((Token WhiteSpace v):xs) = (Token WhiteSpace v):parseCommandName2 xs
parseCommandName2 ((Token Name v):xs)  = (Token Name v):parseCommandName3 xs 0
parseCommandName2 ((Token Assign v):xs)  = (Token Assign v):parseCommandName2P xs
parseCommandName2 (x:xs)                          = (Token (ErrorToken x) "unknown token") : parseCommandName2 xs


-- | exactly 2 names (after assign)
parseCommandName2P :: [Token] -> [Token]
parseCommandName2P [] = []
parseCommandName2P ((Token WhiteSpace v):xs) = (Token WhiteSpace v):parseCommandName2P xs
parseCommandName2P ((Token Name v):xs)
  | v == "exec" = (Token ReservedToken v):parseCommandExec xs 0
  | v == "split" = (Token ReservedToken v):parseCommandSplit xs 0
  | otherwise = (Token Name v):parseCommandName3 xs 2
parseCommandName2P (x:xs)                          = (Token (ErrorToken x) "unknown token") : parseCommandName2P xs


-- | stage0 = no string, stage1 = string
parseCommandExec :: [Token] -> Int -> [Token]
parseCommandExec [] s = []
parseCommandExec ((Token WhiteSpace v):xs) s  = (Token WhiteSpace v):parseCommandExec xs s
parseCommandExec ((Token StringStart v):xs) 0 = let t = parseString xs [(Token StringStart v)] in (fst t) : parseCommandExec (snd t) 1 -- parse string
parseCommandExec ((Token StringStart v):xs) 1 = let t = parseString xs [(Token StringStart v)] in (fst t) : parseCommandExec (snd t) 2 -- parse string
parseCommandExec ((Token CommandEnd v):xs) 1  = (Token CommandEnd v):parseGuardOrCommand xs
parseCommandExec ((Token CommandEnd v):xs) 2  = (Token CommandEnd v):parseGuardOrCommand xs
parseCommandExec (x:xs) s                     = (Token (ErrorToken x) "unknown token") : parseCommandExec xs s


-- | stage0 = no str, stage1 = 1 str, stage2 = 2 str
parseCommandSplit :: [Token] -> Int -> [Token]
parseCommandSplit [] s = []
parseCommandSplit ((Token WhiteSpace v):xs) s = (Token WhiteSpace v):parseCommandSplit xs s
parseCommandSplit ((Token StringStart v):xs) 0 = let t = parseString xs [(Token StringStart v)] in (fst t) : parseCommandSplit (snd t) 1 -- parse string
parseCommandSplit ((Token StringStart v):xs) 1 = let t = parseString xs [(Token StringStart v)] in (fst t) : parseCommandSplit (snd t) 2 -- parse string
parseCommandSplit ((Token CommandEnd v):xs) 2 = (Token CommandEnd v):parseGuardOrCommand xs
parseCommandSplit (x:xs) s                         = (Token (ErrorToken x) "unknown token") : parseCommandSplit xs s


-- | input, stage: stage0 =names, 1= assign, 2=name after
parseCommandName3 :: [Token] -> Int -> [Token]
parseCommandName3 [] s                        = []
parseCommandName3 ((Token WhiteSpace v):xs) s = (Token WhiteSpace v):parseCommandName3 xs s
parseCommandName3 ((Token Name v):xs) 0       = (Token Name v):parseCommandName3 xs 0
parseCommandName3 ((Token Name v):xs) 1       = (Token Name v):parseCommandName3 xs 2
parseCommandName3 ((Token Assign v):xs) 0     = (Token Assign v):parseCommandName3 xs 1
parseCommandName3 ((Token StringStart v):xs) 2 = let t = parseString xs [(Token StringStart v)] in (fst t) : parseCommandName3 (snd t) 2 -- parse string
parseCommandName3 ((Token CommandEnd v):xs) 2 = (Token CommandEnd v):parseGuardOrCommand xs
parseCommandName3 (x:xs) s                    = (Token (ErrorToken x) "unknown token") : parseCommandName3 xs s

