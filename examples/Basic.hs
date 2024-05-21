import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import BPE.Base
import BPE.Basic

sampleText :: String
sampleText = "The American singer-songwriter Taylor Swift has received numerous industry awards and honorary accolades."

main :: IO ()
main = do
    text <- BS.readFile "taylorswift.txt"
    let (merges, vocab) = trainTokenizer 512 text
    saveMergesAndVocab "basic_taylorswift" merges vocab
    (merges, vocab) <- loadMergesAndVocab "basic_taylorswift.merges"
    let encoded = encode merges (BSU.fromString sampleText)
    putStrLn $ "Original text: " ++ sampleText
    putStrLn $ "Encoded text: " ++ show encoded
    let decoded = decode vocab encoded
    putStrLn $ "Decoded text: " ++ show decoded
