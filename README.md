# minbpe-hs

• **[Introduction](#introduction)**<br>
• **[Installation](#installation)**<br>
• **[Usage](#usage)**<br>
• **[Tests](#tests)**<br>
• **[Parity With Python](#parity-with-python)**<br>

## Introduction

minbpe-hs is a port of Andrej Karpathy's concise byte-level byte pair encoding (BPE) implementation, [minbpe](https://github.com/karpathy/minbpe), to Haskell. During training, BPE compresses a sequence by repeatedly finding the most frequent pair of elements in the input and merging them into a new token whilst maintaining a record of the merges and the tokens. Encoding is performed by recursively merging pairs with the smallest merge index until there remain no pairs to be merged, and encoded representations can be decoded backed into the original through vocabulary look-ups. Given its recursive nature, BPE lends itself naturally to functional programming, and the objective of this repository is to offer an alternative functional formulation of it that is orthogonal to typical implementations in imperative languages such as Python or Rust to aid learners in gaining a firmer grasp on BPE by vieweing it through a different lens.

Similar to minbpe, ```BPE.Basic``` and ```BPE.Regex``` both supply the same fundamental functionalities of training, encoding, and decoding, but the former treats a text corpus as one continuous sequence whereas the latter, in addition to supporting special tokens, divides it into smaller segments using a regex pattern and considers merges only within these segments. ```BPE.Base``` contains core operations employed by the two tokenizers and also provides storing & loading utilities. There is no counterpart to minbpe's ```GPT4Tokenizer``` as [tiktoken](https://github.com/openai/tiktoken) does not expose a Haskell frontend.

## Installation

Please run ```cabal install``` to install minbpe-hs. Alternatively, you can pass the source directory of this repository to GHC via the ```-i``` flag when compiling code that uses minbpe-hs (e.g., ```ghc -isrc MyProgram.hs```).

## Usage

[BPE's Wikipedia entry](https://en.wikipedia.org/wiki/Byte_pair_encoding) discusses a simple example that can be reproduced using minbpe-hs as follows.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import BPE.Base
import BPE.Basic

main :: IO ()
main = do
    let (merges, vocab) = trainTokenizer (256 + 3) "aaabdaaabac" -- 256 initial tokens (bytes), 3 merges
    putStrLn $ show $ encode merges "aaabdaaabac" -- [258, 100, 258, 97, 99]
    putStrLn $ show $ decode vocab [258, 100, 258, 97, 99] -- "aaabdaaabac"
    saveMergesAndVocab "toy" merges vocab -- Saves toy.merges and toy.vocab
```

More examples can be found in the [```examples/```](https://github.com/BobMcDear/minbpe-hs/tree/main/examples) folder. On average, the training portions of the examples run 2-3x faster than minbpe, and encoding long strings of text can be 5-6x times faster.

## Tests

The test suite can be run using ```cabal test``` or by directly executing ```tests/Main.hs```, assuming the test dependencies have already been installed.

## Parity With Python

Generally, the results of minbpe-hs are identical to those of the equivalent Python implementation (i.e., minbpe), with one major exception: [```regex-pcre```](https://hackage.haskell.org/package/regex-pcre), the Haskell regex package used by this project, does not properly handle non-ASCII characters in that it matches the UTF-8-encoded bytes of a single Unicode character separately, so the output of ```BPE.Regex``` would be incompatible with that of minbpe's ```RegexTokenizer``` should the input not be purely ASCII.
