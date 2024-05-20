{-# LANGUAGE OverloadedStrings #-}

module TestBase (testSaveAndLoad) where

import System.Directory (removeFile)
import Test.HUnit (Test(TestCase), assertEqual)
import BPE.Base
import BPE.Basic

testSaveAndLoad :: Test
testSaveAndLoad = TestCase $ do
    let expected@(merges, vocab) = trainTokenizer 260 "aaabdaaabac añ1 bñ1_ 1_"
    saveMergesAndVocab "tmp_test" merges vocab
    actual <- loadMergesAndVocab "tmp_test.merges"
    removeFile "tmp_test.merges"
    removeFile "tmp_test.vocab"
    assertEqual "save and load" expected actual
