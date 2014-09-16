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
c1              = "\x1b[35m"
c2              = "\x1b[38;5;150m"
colorError      = "\x1b[41m"
colorReserved   = "\x1b[34m"

pretty :: String -> String
pretty x = foldl (\e f -> e ++ prettyToken f) [] $ parse $ tokenize x

prettyToken :: Token -> String
prettyToken (Token UnknownToken v) = colorError ++ v ++ colorReset
prettyToken (Token BlockStart v) = c2 ++ v ++ colorReset
prettyToken (Token BlockEnd v) = c2 ++ v ++ colorReset
prettyToken (Token ReservedToken v) = colorReserved ++ v ++ colorReset
prettyToken (Token (ErrorToken (Token _ v)) _) = colorError ++ v ++ colorReset
prettyToken (Token _ v) = v

parse :: [Token] -> [Token]
parse [] = []
parse ((Token Variable v):xs) = (Token (ErrorToken (Token Variable v)) v) : parse xs
parse ((Token Name v):xs)
  | v == "exec" || v == "split" || v == "finally" = (Token ReservedToken v) : parse xs
parse (x:xs) = x : parse xs

--parseProc :: [Token] -> [Token]
