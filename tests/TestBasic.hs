module TestBasic (testTrainTokenizer, testEncode, testDecode) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict.InsOrd as Map
import Test.HUnit (Test(TestCase), assertEqual)
import BPE.Base
import BPE.Basic

text :: BS.ByteString
text = BSU.fromString "aaabdaaabac añ1 bñ1_ 1_"

testTrainTokenizer :: Test
testTrainTokenizer = TestCase $ assertEqual "train tokenizer" expected actual
    where expectedMerges = Map.fromList [ ((97, 97), 256)
                                        , ((256, 97), 257)
                                        , ((257, 98), 258)
                                        , ((195, 177), 259) ]
          expectedVocab = mergesToVocab expectedMerges
          expected = (expectedMerges, expectedVocab)
          actual = trainTokenizer 260 text

testEncode :: Test
testEncode = TestCase $ assertEqual "encode" expected actual
    where (merges, _) = trainTokenizer 260 text
          expected = [258, 100, 258, 97, 99, 32, 97, 259, 49, 32, 98, 259, 49, 95, 32, 49, 95]
          actual = encode merges text

testDecode :: Test
testDecode = TestCase $ assertEqual "decode" text actual
    where (_, vocab) = trainTokenizer 260 text
          seq = [258, 100, 258, 97, 99, 32, 97, 259, 49, 32, 98, 259, 49, 95, 32, 49, 95]
          actual = decode vocab seq
