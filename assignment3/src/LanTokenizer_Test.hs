-----------------------------------------------------------------------------
--
-- Module      :  LanTokenizer_Test
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

module LanTokenizer_Test (
  runTest
) where


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit
import LanTokenizer

testTokenize = TestLabel "test for tokenizer" $ TestList [
    TestCase $ assertEqual "empty input"
      ([])
      (tokenize ""),
    TestCase $ assertEqual "string \"foo\""
      ([Token StringStart "\"", Token SubString "foo", Token StringEnd "\""])
      (tokenize "\"foo\""),
    TestCase $ assertEqual "string with var \"fo$x$o\""
      ([Token StringStart "\"", Token SubString "fo", Token Variable "$x$",  Token SubString "o", Token StringEnd "\""])
      (tokenize "\"fo$x$o\""),
    TestCase $ assertEqual "string with var and spaces \"fo$x$ o\""
      ([Token StringStart "\"", Token SubString "fo", Token Variable "$x$", Token SubString " o", Token StringEnd "\""])
      (tokenize "\"fo$x$ o\""),
   TestCase $ assertEqual "equals =="
      ([Token Equals "=="])
      (tokenize "=="),
   TestCase $ assertEqual "not equals !="
      ([Token NotEquals "!="])
      (tokenize "!="),
   TestCase $ assertEqual "assign ="
      ([Token Assign "="])
      (tokenize "="),
   TestCase $ assertEqual "some unknown token x = &&"
      ([Token Name "x", Token WhiteSpace " ", Token Assign "=", Token WhiteSpace " ", Token UnknownToken "&&"])
      (tokenize "x = &&"),
   TestCase $ assertEqual "mixed line a b =   \"foo\""
      ([Token Name "a", Token WhiteSpace " ", Token Name "b", Token WhiteSpace " ", Token Assign "=",
        Token WhiteSpace "   ", Token StringStart "\"", Token SubString "foo", Token StringEnd "\""])
      (tokenize "a b =   \"foo\""),
   TestCase $ assertEqual "short prog (maxnum)"
      ([Token Name "maxnum", Token WhiteSpace " ", Token Name "a", Token WhiteSpace " ", Token Name "b",
        Token WhiteSpace " ", Token ProcDelim "-", Token WhiteSpace " ",
        Token StringStart "\"", Token SubString "", Token Variable "$max$", Token SubString "", Token StringEnd "\"",
        Token WhiteSpace " ", Token BlockStart "{", Token WhiteSpace " ", Token Name "ab", Token WhiteSpace " ",
        Token Assign "=", Token WhiteSpace " ", Token Name "exec", Token WhiteSpace " ",
        Token StringStart "\"", Token SubString "test ", Token Variable "$a$", Token SubString " ",
        Token Variable "$b$", Token SubString " -le ", Token Variable "$b$", Token SubString "",
        Token StringEnd "\"", Token CommandEnd ";", Token WhiteSpace " ", Token BlockEnd "}"])
      (tokenize "maxnum a b - \"$max$\" { ab = exec \"test $a$ $b$ -le $b$\"; }")
  ]

testTokenizeVariable = TestLabel "test for variable tokenizer" $ TestList [
    TestCase $ assertEqual "empty input"
      ("", Token OpenVariable "$")
      (tokenizeVariable "" ""),
    TestCase $ assertEqual "var only 'x$'"
      ("", Token Variable "$x$")
      (tokenizeVariable "x$" ""),
    TestCase $ assertEqual "var and other stuff 'x$ foo'"
      ("foo", Token Variable "$x$")
      (tokenizeVariable "x$foo" ""),
    TestCase $ assertEqual "open var"
      ("\" 123", Token OpenVariable "$xy")
      (tokenizeVariable "xy\" 123" "")
  ]

testTokenizeName = TestLabel "test for name tokenizer" $ TestList [
    TestCase $ assertEqual "empty input"
      ("", Token Name "")
      (tokenizeName "" ""),
    TestCase $ assertEqual "name foO123"
      ("", Token Name "foO123")
      (tokenizeName "foO123" ""),
    TestCase $ assertEqual "name foO 123"
      (" 123", Token Name "foO")
      (tokenizeName "foO 123" "")
  ]
runTest = defaultMain $ concatMap hUnitTestToTests [
    testTokenizeVariable, testTokenize, testTokenizeName
  ]



