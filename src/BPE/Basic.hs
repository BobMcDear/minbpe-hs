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
trainTokenizer vocabSize = trainTokenizerHelper vocabSize merges vocab 256 . textToSeq
    where merges = Map.empty
          vocab = mergesToVocab merges

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
encode :: Merges -> BS.ByteString -> Seq
encode merges = encodeHelper merges . textToSeq

-- Decodes the input into a string using vocabulary look-up
decode :: Vocab -> Seq -> BS.ByteString
decode vocab = BS.concat . map lookupId
    where lookupId id = fromJust $ Map.lookup id vocab
