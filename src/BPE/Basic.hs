-- Basic package for MinBPE.
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

module BPE.Basic
    ( trainTokenizer
    , encode
    , decode
    ) where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.List (minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import BPE.Base

-- Recursively finds the most frequent pair, merges it, and updates the merges & vocabulary
trainTokenizerHelper :: Int -> Merges -> Vocab -> Id -> Seq -> (Merges, Vocab)
trainTokenizerHelper vocabSize merges vocab id seq
    | id == vocabSize = (merges, vocab)
    | otherwise       = trainTokenizerHelper vocabSize newMerges newVocab (id + 1) merged
    where pairCounts = pairCount Map.empty seq
          pairToMerge@(id1, id2) = maxByVal pairCounts
          merged = mergePair pairToMerge id seq
          newMerges = Map.insert pairToMerge id merges
          newVocab = Map.insert id (BS.concat $ map fromJust [Map.lookup id1 vocab, Map.lookup id2 vocab]) vocab

-- Recursively finds the most frequent pair, merges it, and updates the merges & vocabulary
trainTokenizer :: Int -> BS.ByteString -> (Merges, Vocab)
trainTokenizer vocabSize = trainTokenizerHelper vocabSize merges vocab 256 . initSeq256
    where merges = Map.empty
          vocab = mergesToVocab merges initVocab256

-- Recursively merges pairs with the smallest merge ID
encodeHelper :: Merges -> Seq -> Seq
encodeHelper _ [] = []
encodeHelper _ [x] = [x]
encodeHelper merges seq
    | Map.member pairToMerge merges = encodeHelper merges merged
    | otherwise                     = seq
    where pairCounts = pairCount Map.empty seq
          compId = comparing (\pair ->  Map.lookupDefault inf pair merges)
          pairToMerge = minimumBy compId (reverse $ Map.keys pairCounts)
          merged = mergePair pairToMerge (fromJust $ Map.lookup pairToMerge merges) seq

-- Recursively merges pairs with the smallest merge ID
encode :: (BS.ByteString -> Seq) -> Merges -> BS.ByteString -> Seq
encode initSeq merges text = encodeHelper merges (initSeq text)

-- Decodes the input into a string using vocabulary look-up
decode :: Vocab -> Seq -> BS.ByteString
decode vocab = BS.concat . map lookupId
    where lookupId id = fromJust $ Map.lookup id vocab
