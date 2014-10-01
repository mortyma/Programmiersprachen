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

module LanPrettyprint2 where

import qualified Data.Map as Map
import LanTokenizer

data VarState = Used | Declared | Fine deriving (Eq,Show,Ord)
data VarDict = VD String (Map.Map String VarState) deriving (Eq,Show,Ord)

data Tree b = Null | Node b [Tree b] deriving (Eq, Ord, Show)

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


-- | next token will be sibling
sibling :: Token -> ([Tree Token], [Token], VarDict) -> ([Tree Token], [Token], VarDict)
sibling t (tr, r, d) = (Node t []:tr, r, d)

-- | second token is child of first, what comes after is sibling - used for markers/errors
siblingChild :: Token -> Token -> ([Tree Token], [Token], VarDict) -> ([Tree Token], [Token], VarDict)
siblingChild t1 t2 (tr, r, d) = (Node t1 [Node t2 []]:tr, r, d)

-- | next token will be child
child :: Token -> ([Tree Token], [Token], VarDict) -> ([Tree Token], [Token], VarDict)
child t (tr, r, d) = ([Node t tr], r, d)

-- | pipe the input to the next function
continue :: ([Token] -> VarDict -> ([Tree Token], [Token], VarDict)) -> ([Tree Token], [Token], VarDict) -> ([Tree Token], [Token], VarDict)
continue f (tr, t, d) = f t d

-- | see continue
continueBool :: ([Token] -> VarDict -> Bool -> ([Tree Token], [Token], VarDict)) -> Bool -> ([Tree Token], [Token], VarDict) -> ([Tree Token], [Token], VarDict)
continueBool f bo (tr, t, d) = f t d bo


-- | between child and sibingchild with errortoken..
checkOpenVar :: Token -> ([Tree Token], [Token], VarDict) -> ([Tree Token], [Token], VarDict)
checkOpenVar t (tr, r, d)
  | cV (last tr) == True = ([Node t tr], r, d)
  | otherwise =  (Node (Token ErrorToken "variable not closed")[Node t []]:tr, r, d)

cV :: Tree Token -> Bool
cV (Node (Token VariableEnd _)[]) = True
cV _ = False



-- | namespace, name
tokenKey :: String -> String -> String
tokenKey ns n =  '%' : ns ++ "%%" ++ n


-- | mark token as unused if it was not found as 'fine'
--markIfUnused :: Token -> VarDict -> Token
--markIfUnused (Token tt v) d = let b = Map.lookup v d in
--  case b of
--  Nothing -> Token (UnusedVar (Token tt v)) "never used"
--  Just x ->
--    if x == Fine
--    then (Token tt v)
--    else Token (UnusedVar (Token tt v)) "never used"

-- | insert a name into the dictionary and check if it was already used somewhere
insertName :: String -> VarDict -> VarDict
insertName v (VD ns d) = let b = Map.lookup v d in
  case b of
  Nothing -> VD ns (Map.insert (tokenKey ns v) Declared d)
  Just x ->
    if x == Used
    then VD ns (Map.insert (tokenKey ns v) Fine d)
    else VD ns d

insertVar :: String -> VarDict -> VarDict
insertVar v (VD ns d) = let b = Map.lookup v d in
  case b of
  Nothing -> VD ns (Map.insert (tokenKey ns v) Used d)
  Just x ->
    if x == Declared
    then VD ns (Map.insert (tokenKey ns v) Fine d)
    else VD ns d


pretty :: String -> String
pretty x = foldl (\e f -> e ++ prettyToken f) [] $ prettyTree $ (\(a,_,_) -> a) (parseProc  (tokenize x) (VD "" Map.empty))


prettyTree :: [Tree Token] -> [Token]
prettyTree [] = [Token Name "qwe"]
prettyTree ((Node t st):xs) = prettyTree st ++ [t] ++ prettyTree xs

-- | prettify tokens
prettyToken :: Token -> String
prettyToken (Token Unknown v) = colorError ++ v ++ colorReset
--prettyToken (Token OpenVariable v) = colorUnfinished ++ colorVar ++ v ++ colorReset
--prettyToken (Token (ErrorToken (Token _ v)) _) = colorError ++ v ++ colorReset
--prettyToken (Token (UnusedVar (Token _ v)) _) = colorUnused ++ v ++ colorReset -- does not work right :/
prettyToken (Token BlockStart v) = colorBlock ++ v ++ colorReset
prettyToken (Token BlockEnd v) = colorBlock ++ v ++ colorReset
--prettyToken (Token ReservedToken v) = colorReserved ++ v ++ colorReset
prettyToken (Token ProcName v) = colorProc ++ v ++ colorReset
prettyToken (Token ProcVar v) = colorVar ++ v ++ colorReset
prettyToken (Token Name v) = colorVar ++ v ++ colorReset
prettyToken (Token Variable v) = colorInVar ++ v ++ colorReset
prettyToken (Token SubString v) = colorString ++ v ++ colorReset
prettyToken (Token StringEnd v) = colorString ++ v ++ colorReset
--prettyToken (Token (String l) _ ) = colorString ++ foldl (\e f -> e ++ prettyToken f) "" l ++ colorReset
--prettyToken (Token (OpenString l) _ ) = colorUnfinished ++ colorString ++  foldl (\e f -> e ++ prettyTokenOpenString f) "" l ++ colorReset
prettyToken (Token _ v) = v

-- | prettify inside strings
prettyTokenOpenString :: Token -> String
prettyTokenOpenString (Token SubString v) = colorUnfinished ++ colorString ++ v ++ colorReset
--prettyTokenOpenString (Token (ErrorToken (Token _ v)) _) = colorError ++ v ++ colorReset
--prettyTokenOpenString (Token (UndeclaredVar (Token _ v)) _) = colorUnused ++ v ++ colorReset -- does not work right :/
--prettyTokenOpenString (Token OpenVariable v) = colorUnfinished ++ colorVar ++ v ++ colorReset
prettyTokenOpenString (Token Variable v) = colorUnfinished ++ colorInVar ++ v ++ colorReset
prettyTokenOpenString (Token _ v) = v

-------------------------------------------------------------------

-- | input: if min one was found: expect a name token, else mark it as error and try again, continue with input vars
parseProc :: [Token] -> VarDict -> ([Tree Token], [Token], VarDict)
parseProc [] d                            = ([], [],  d)
parseProc ((Token Whitespace v):xs) d     = sibling (Token Whitespace v) $ parseProc xs d  -- continue
parseProc ((Token Name v):xs) d           = continue parseProc $ child (Token ProcName v) $ parseProcIn xs d
-- parseProc ((Token ProcDelim v):xs) d      = continue parseProc $ child (Token ProcDelim v) $ parseProcOut xs d False -- next step
parseProc (x:xs) d                        = siblingChild (Token ErrorToken "unexpected token") x  $ parseProc xs d


-- | input, found-flag: possible many names, else continue with delimiter
parseProcIn :: [Token] -> VarDict -> ([Tree Token], [Token], VarDict)
parseProcIn [] d                         = ([], [], d)
parseProcIn ((Token Whitespace v):xs) d  = sibling (Token Whitespace v) $ parseProcIn xs d  -- continue
parseProcIn ((Token Name v):xs) (VD ns dd) = sibling (Token ProcVar v) $ parseProcIn xs $ insertName v (VD v dd)  -- continue, change namespace
parseProcIn ((Token ProcDelim v):xs) d   = sibling (Token ProcDelim v) $ parseProcOut xs d False -- next step
parseProcIn (x:xs) d                     = siblingChild (Token ErrorToken "name expected") x $ parseProcIn xs d -- failure and continue


-- | input, found-flag: expect one or more strings
parseProcOut :: [Token] -> VarDict -> Bool -> ([Tree Token], [Token], VarDict)
parseProcOut [] d _                           = ([], [], d)
parseProcOut ((Token Whitespace v):xs) d b    = sibling (Token Whitespace v) $ parseProcOut xs d b -- continue
parseProcOut ((Token StringStart v):xs) d _   = continueBool parseProcOut True $ child (Token StringStart v) $ parseString xs d
parseProcOut ((Token BlockStart v):xs) d True = continue parseProc $ child (Token BlockStart v) $ parseGuardOrCommand xs d -- next step
parseProcOut (x:xs) d b                       = siblingChild (Token ErrorToken "string expected") x $ parseProcOut xs d b -- failure and continue


parseString :: [Token] -> VarDict -> ([Tree Token], [Token], VarDict)
parseString [] d                              = ([], [], d)
parseString ((Token Whitespace v):xs) d       = sibling (Token Whitespace v) $ parseString xs d
parseString ((Token StringEnd v):xs) d        = ([Node (Token StringEnd v)[]], xs, d)
--parseString ((Token StringStart v):xs) d      = sibling (Token StringStart v) $ parseString xs d -- continue
parseString ((Token SubString v):xs) d        = sibling (Token SubString v) $ parseString xs d
parseString ((Token VariableStart v):xs)   d  = continue parseString $ checkOpenVar (Token VariableStart v) $ parseVar xs d
---parseString ((Token OpenVariable v):xs) b  = parseString xs $ b ++ [(Token OpenVariable v)] -- continue
parseString (x:xs)   d                        = siblingChild (Token ErrorToken "string expected") x $ parseString xs d -- failure and continue


parseVar :: [Token] -> VarDict -> ([Tree Token], [Token], VarDict)
parseVar [] d                                 = ([], [], d)
parseVar ((Token VariableEnd v):xs) d         = ([Node (Token VariableEnd v)[]], xs, d)
parseVar ((Token Variable v):xs) d            = sibling (Token Variable v) $ parseVar xs $ insertVar v d
parseVar (x:xs) d                             = siblingChild (Token ErrorToken "string expected") x $ parseVar xs d -- failure and continue


parseGuardOrCommand :: [Token] -> VarDict ->  ([Tree Token], [Token], VarDict)
parseGuardOrCommand [] d                          = ([], [], d)
--parseGuardOrCommand ((Token WhiteSpace v):xs)   = (Token WhiteSpace v):parseGuardOrCommand xs
--parseGuardOrCommand ((Token Name v):xs)
--  | v == "finally"                              = (Token ReservedToken v):parseGuardDelim xs -- check guarddelim
--  | otherwise                                   = (Token Name v):parseGuardOrCommand2 xs -- check compare
--parseGuardOrCommand ((Token BlockEnd v):xs)     = (Token BlockEnd v):parseProc xs
--parseGuardOrCommand (x:xs)                      = (Token (ErrorToken x) "unknown token") : parseGuardOrCommand xs


--parseString :: [Token] -> [Token] -> (Token, [Token])
--parseString [] b                          = ((Token (OpenString b) "string open"), []) -- not closed
--parseString ((Token StringEnd v):xs) b    = ((Token (String (b ++ [(Token StringEnd v)])) ""), xs) -- return string
--parseString ((Token StringStart v):xs) b  = parseString xs $ b ++ [(Token StringStart v)] -- continue
--parseString ((Token Whitespace v):xs) b   = parseString xs $ b ++ [(Token Whitespace v)] -- continue
--parseString ((Token SubString v):xs) b    = parseString xs $ b ++ [(Token SubString v)] -- continue
--parseString ((Token Variable v):xs) b     = parseString xs $ b ++ [(Token Variable v)] -- continue
---parseString ((Token OpenVariable v):xs) b = parseString xs $ b ++ [(Token OpenVariable v)] -- continue
--parseString (x:xs) b                      = parseString xs $ b ++ [(Token (ErrorToken x) "unknown token")] -- should not happen

--parseString :: [Token] -> [Token] -> VarDict ->  ((Token, [Token]), VarDict)
--parseString [] b d                         = (((Token (OpenString b) "string open"), []), d) -- not closed
--parseString ((Token StringEnd v):xs) b d   = (((Token (String (b ++ [(Token StringEnd v)])) ""), xs), d) -- return string
--parseString ((Token StringStart v):xs) b d = (parseString xs (b ++ [(Token StringStart v)]) d) -- continue
--parseString ((Token Whitespace v):xs) b d  = (parseString xs (b ++ [(Token Whitespace v)]) d) -- continue
--parseString ((Token SubString v):xs) b d   = (parseString xs (b ++ [(Token SubString v)]) d) -- continue
--parseString ((Token Variable v):xs) b d    = (parseString xs (b ++ [(Token Variable v)]) (insertVar (Token Variable v) d)) -- continue
---parseString ((Token OpenVariable v):xs) b = parseString xs $ b ++ [(Token OpenVariable v)] -- continue
--parseString (x:xs) b d                     = (parseString xs (b ++ [(Token (ErrorToken x) "unknown token")]) d) -- should not happen
