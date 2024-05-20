import Test.HUnit (Test(TestList), runTestTTAndExit)
import qualified TestBase
import qualified TestBasic
import qualified TestRegex

main :: IO ()
main = runTestTTAndExit $ TestList [ TestBase.testSaveAndLoad
                                   , TestBasic.testTrainTokenizer
                                   , TestBasic.testEncode
                                   , TestBasic.testDecode
                                   , TestRegex.testTrainTokenizer
                                   , TestRegex.testEncode
                                   , TestRegex.testDecode ]
