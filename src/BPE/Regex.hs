-- Regex package for MinBPE.
{-
Copyright (c) 2024 Borna Ahmadzadeh

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}

module BPE.Regex
    ( Pattern
    , SpecialTokens
    , InvSpecialTokens
    , gpt2pattern
    , gpt4pattern
    , trainTokenizer
    , encodeOrdinary
    , encode
    , decode
    ) where

import qualified Data.ByteString as BS
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.List (foldl')
import Data.Maybe (fromJust)
import Text.Regex.PCRE
import BPE.Base
import qualified BPE.Basic

type Pattern = BS.ByteString
type SpecialTokens = InsOrdHashMap BS.ByteString Id
type InvSpecialTokens = InsOrdHashMap Id BS.ByteString

-- GPT2 splitting pattern
gpt2pattern :: Pattern
gpt2pattern = "'(?:[sdmt]|ll|ve|re)| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"

-- GPT4 splitting pattern
gpt4pattern :: Pattern
gpt4pattern = "'(?i:[sdmt]|ll|ve|re)|[^\\r\\n\\p{L}\\p{N}]?+\\p{L}+|\\p{N}{1,3}| ?[^\\s\\p{L}\\p{N}]++[\\r\\n]*|\\s*[\\r\\n]|\\s+(?!\\S)|\\s+"

-- Find all matches of a regex pattern in a text
findAll :: Pattern -> BS.ByteString -> [BS.ByteString]
findAll pattern text = getAllTextMatches $ text =~ pattern

-- Recursively finds the most frequent pair, merges it, and updates the merges & vocabulary
trainTokenizerHelper :: Int -> Merges -> Vocab -> Id -> [Seq] -> (Merges, Vocab)
trainTokenizerHelper vocabSize merges vocab id seqList
    | id == vocabSize = (merges, vocab)
    | otherwise       = trainTokenizerHelper vocabSize newMerges newVocab (id + 1) mergedList
    where pairCounts = foldl' pairCount Map.empty seqList
          pairToMerge@(id1, id2) = maxByVal pairCounts
          mergedList = map (mergePair pairToMerge id) seqList
          newMerges = Map.insert pairToMerge id merges
          newVocab = Map.insert id (BS.concat $ map fromJust [Map.lookup id1 vocab, Map.lookup id2 vocab]) vocab

-- Recursively finds the most frequent pair, merges it, and updates the merges & vocabulary
trainTokenizer :: Int -> Pattern -> BS.ByteString -> (Merges, Vocab)
trainTokenizer vocabSize pattern = trainTokenizerHelper vocabSize merges vocab 256
                                 . map initSeq256
                                 . findAll pattern
    where merges = Map.empty
          vocab = mergesToVocab merges initVocab256

-- Recursively merges pairs with the smallest merge ID, ignoring special tokens
encodeOrdinary :: (BS.ByteString -> Seq) -> Merges -> Pattern -> BS.ByteString -> Seq
encodeOrdinary initSeq merges pattern = concat . map (BPE.Basic.encode initSeq merges ) . findAll pattern

-- Recursively merges pairs with the smallest merge ID, raising an error for special tokens
encode :: (BS.ByteString -> Seq) -> Merges -> Pattern -> SpecialTokens -> BS.ByteString -> Seq
encode initSeq merges pattern specialTokens text
    | noSpecial = encodeOrdinary initSeq merges pattern text
    | otherwise = error "Input cannot contain special tokens."
    where noSpecial = all (\special -> not $ text =~ special) (Map.keys specialTokens)

-- Decodes the input into a string using vocabulary look-up with support for special tokens
decode :: Vocab -> InvSpecialTokens -> Seq -> BS.ByteString
decode vocab invSpecialTokens = BS.concat . map lookupId
    where lookupId id = fromJust $ maximum $ map (Map.lookup id) [vocab, invSpecialTokens]
