import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict.InsOrd as Map
import BPE.Base
import BPE.Regex

specialTokens :: SpecialTokens
specialTokens = Map.fromList [(BSU.fromString "<bos>", 513)]

sampleText :: String
sampleText = "The American singer-songwriter Taylor Swift has received numerous industry awards and honorary accolades."

main :: IO ()
main = do
    text <- BS.readFile "taylorswift.txt"
    let (merges, vocab) = trainTokenizer 512 gpt4pattern text
    saveMergesAndVocab "regex_taylorswift" merges vocab
    (merges, vocab) <- loadMergesAndVocab "regex_taylorswift.merges"
    let encoded = encode merges gpt4pattern specialTokens (BSU.fromString sampleText)
    putStrLn $ "Original text: " ++ sampleText
    putStrLn $ "Encoded text: " ++ show encoded
    let decoded = decode vocab (invMap specialTokens) (513:encoded)
    putStrLn $ "Decoded text (plus special <bos> token): " ++ show decoded
