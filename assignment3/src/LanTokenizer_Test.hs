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
      ([Token StringStart "\"", Token SubString "fo", Token VariableStart "$",Token Variable "x", Token VariableEnd "$", Token SubString "o", Token StringEnd "\""])
      (tokenize "\"fo$x$o\""),
    TestCase $ assertEqual "string with var and spaces \"fo$x$ o\""
      ([Token StringStart "\"", Token SubString "fo", Token VariableStart "$",Token Variable "x", Token VariableEnd "$", Token SubString " o", Token StringEnd "\""])
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
      ([Token Name "x", Token Whitespace " ", Token Assign "=", Token Whitespace " ", Token Unknown "&", Token Unknown "&"])
      (tokenize "x = &&"),
   TestCase $ assertEqual "whitespace"
      ([Token Whitespace "\v\t "])
      (tokenize "\v\t "),
   TestCase $ assertEqual "double-\\"
      ([Token StringStart "\"", Token SubString "\\\\", Token VariableStart "$", Token StringEnd "\""])
      (tokenize "\"\\\\$\""),
    TestCase $ assertEqual "no empty SubString"
      ([Token StringStart "\"", Token StringEnd "\""])
      (tokenize "\"\""),
   TestCase $ assertEqual "mixed line a b =   \"foo\""
      ([Token Name "a", Token Whitespace " ", Token Name "b", Token Whitespace " ", Token Assign "=",
        Token Whitespace "   ", Token StringStart "\"", Token SubString "foo", Token StringEnd "\""])
      (tokenize "a b =   \"foo\""),
   TestCase $ assertEqual "short prog (maxnum)"
      ([Token Name "maxnum", Token Whitespace " ", Token Name "a", Token Whitespace " ", Token Name "b",
        Token Whitespace " ", Token ProcDelim "-", Token Whitespace " ",
        Token StringStart "\"", Token VariableStart "$", Token Variable "max", Token VariableEnd "$", Token StringEnd "\"",
        Token Whitespace " ", Token BlockStart "{", Token Whitespace " ", Token Name "ab", Token Whitespace " ",
        Token Assign "=", Token Whitespace " ", Token Name "exec", Token Whitespace " ",
        Token StringStart "\"", Token SubString "test ", Token VariableStart "$", Token Variable "a", Token VariableEnd "$", Token SubString " ",
        Token VariableStart "$", Token Variable "b", Token VariableEnd "$", Token SubString " -le ", Token VariableStart "$", Token Variable "b",Token VariableEnd "$", 
        Token StringEnd "\"", Token CommandEnd ";", Token Whitespace " ", Token BlockEnd "}"])
      (tokenize "maxnum a b - \"$max$\" { ab = exec \"test $a$ $b$ -le $b$\"; }"),
    TestCase $ assertEqual "name foO123"
      ([Token Name "foO123"])
      (tokenize "foO123"),
    TestCase $ assertEqual "whitespace tab"
      ([Token Name "foO", Token Whitespace "\t"])
      (tokenize "foO\t"),
    TestCase $ assertEqual "comment"
      ([Token Name "f", Token Comment "#$\t ==\\\\\"", Token Whitespace "\n ", Token Name "b"])
      (tokenize "f#$\t ==\\\\\"\n b")
  ]

testTokenizeVariable = TestLabel "test for variable tokenizer" $ TestList [
    TestCase $ assertEqual "empty input"
      ([])
      (tokenizeVariable ""),
    TestCase $ assertEqual "var only 'x$'"
      ([Token Variable "x", Token VariableEnd "$"])
      (tokenizeVariable "x$"),
    TestCase $ assertEqual "var and other stuff 'x$ foo'"
      ([Token Variable "x", Token VariableEnd "$", Token SubString "foo"])
      (tokenizeVariable "x$foo"),
    TestCase $ assertEqual "open var"
      ([Token Variable "xy", Token StringEnd "\"",Token Whitespace " ", Token Name "123"])
      (tokenizeVariable "xy\" 123")
  ]

runTest = defaultMain $ concatMap hUnitTestToTests [
    testTokenize,
    testTokenizeVariable
  ]



