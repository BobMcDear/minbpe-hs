{-# LANGUAGE OverloadedStrings #-}

module TestRegex (testTrainTokenizer, testEncode, testDecode) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict.InsOrd as Map
import Test.HUnit (Test(TestCase), assertEqual)
import BPE.Base
import BPE.Regex

text :: BS.ByteString
text = "aaabdaaabac an1 bn1_ 1_"

testTrainTokenizer :: Test
testTrainTokenizer = TestCase $ assertEqual "train tokenizer" expected actual
    where expectedMerges = Map.fromList [ ((97, 97), 256)
                                         , ((256, 97), 257)
                                         , ((257, 98), 258)
                                         , ((258, 100), 259) ]
          expectedVocab = mergesToVocab expectedMerges
          expected = (expectedMerges, expectedVocab)
          actual = trainTokenizer 260 gpt4pattern text

testEncode :: Test
testEncode = TestCase $ assertEqual "encode" expected actual
    where (merges, _) = trainTokenizer 260 gpt4pattern text
          expected = [259, 258, 97, 99, 32, 97, 110, 49, 32, 98, 110, 49, 95, 32, 49, 95]
          actual = encode merges gpt4pattern Map.empty text

testDecode :: Test
testDecode = TestCase $ assertEqual "decode" expected actual
    where (_, vocab) = trainTokenizer 260 gpt4pattern text
          seq = [259, 258, 97, 99, 32, 97, 110, 49, 32, 98, 110, 49, 95, 32, 49, 95, 260]
          expected = BS.append text "<eos>"
          actual = decode vocab (Map.fromList [(260, "<eos>")]) seq
