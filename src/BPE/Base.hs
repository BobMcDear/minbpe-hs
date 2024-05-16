module BPE.Base
    ( Id
    , Seq
    , Pair
    , Merges
    , Vocab
    , textToSeq
    , maxByVal
    , invMap
    , inf
    , pairCount
    , mergePair
    , saveMergesAndVocab
    , mergesToVocab
    , loadMergesAndVocab
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (isControl, ord)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Hashable (Hashable)
import Data.List (foldl', maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Word (Word8)
import Numeric (showHex)
import System.FilePath (addExtension)

type Id = Int
type Seq = [Id]
type Pair = (Id, Id)
type Merges = InsOrdHashMap Pair Id
type Vocab = InsOrdHashMap Id BS.ByteString

-- Converts a text to a list of integral byte values
textToSeq :: BS.ByteString -> Seq
textToSeq = map fromIntegral . BS.unpack

-- Finds the key in an ordered map with the maximum value
maxByVal :: (Eq k, Hashable k, Ord v) => InsOrdHashMap k v -> k
maxByVal omap = maximumBy compVal (reverse $ Map.keys omap)
    where compVal = comparing (\k -> Map.lookup k omap)

-- Inverts a map
invMap :: (Eq v, Hashable v) => InsOrdHashMap k v -> InsOrdHashMap v k
invMap = Map.fromList . map (\(k, v) -> (v, k)) . Map.toList

-- Integral infinity
inf :: Int
inf = maxBound

-- Counts the occurences of each consequtive pair in the input
pairCount :: InsOrdHashMap Pair Int -> Seq -> InsOrdHashMap Pair Int
pairCount pairCounts [] = pairCounts
pairCount pairCounts [x] = pairCounts
pairCount pairCounts xs = pairCount (Map.insertWith (+) pair 1 pairCounts) (tail xs)
    where pair = (xs !! 0, xs !! 1)

-- Replaces every occurence of a pair in the input with a new ID
mergePair :: Pair -> Id -> Seq -> Seq
mergePair _ _ [] = []
mergePair _ _ [x] = [x]
mergePair pair id (x1:x2:xs)
    | (x1, x2) == pair = id:(mergePair pair id xs)
    | otherwise        = x1:(mergePair pair id (x2:xs))

-- Replaces control characters with their hexadecimal Unicode code points
replaceControlChrs :: BS.ByteString -> String
replaceControlChrs = foldl' (\newChrs chr -> newChrs ++ escapeControl chr) "" . BSU.toString
    where escapeControl chr
              | isControl chr = "\\u" ++ replicate (4 - (length hex)) '0' ++ hex
              | otherwise     = [chr]
              where hex = showHex (ord chr) ""

-- Prettifies vocabulary for human readability
prettifyVocab :: Merges -> Vocab -> String
prettifyVocab merges vocab = Map.foldlWithKey' (\pretty id str -> pretty ++ getPretty id str) "" vocab
    where invMerges = invMap merges
          getPretty id str
              | id < 256  = rendered
              | otherwise = rendered1 ++ " " ++ rendered2 ++ " -> " ++ rendered
              where rendered = replaceControlChrs str ++ " " ++ show id ++ "\n"
                    (id1, id2) = fromJust $ Map.lookup id invMerges
                    rendered1 = replaceControlChrs $ fromJust $ Map.lookup id1 vocab
                    rendered2 = replaceControlChrs $ fromJust $ Map.lookup id2 vocab

-- Saves merges and vocabulary to disk
saveMergesAndVocab :: FilePath -> Merges -> Vocab -> IO ()
saveMergesAndVocab path merges vocab = do
    writeFile (addExtension path "merges") (show $ Map.toList merges)
    writeFile (addExtension path "vocab") (prettifyVocab merges vocab)

-- Builds vocabulary given merges
mergesToVocab :: Merges -> Vocab
mergesToVocab = Map.foldlWithKey' updateVocab initVocab
    where initVocab = Map.fromList [(id, BS.pack [fromIntegral id :: Word8]) | id <- [0..255]]
          getPair vocab id1 id2 = BS.concat $ map fromJust [Map.lookup id1 vocab, Map.lookup id2 vocab]
          updateVocab vocab (id1, id2) id = Map.insert id (getPair vocab id1 id2) vocab

-- Loads merges from disk and reconstructs vocabulary
loadMergesAndVocab :: FilePath -> IO (Merges, Vocab)
loadMergesAndVocab path = do
    merges <- fmap (Map.fromList . read) (readFile path)
    return (merges, mergesToVocab merges)
