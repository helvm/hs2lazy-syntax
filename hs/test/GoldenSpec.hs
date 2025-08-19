module GoldenSpec (test_golden) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as BL
import System.FilePath (takeBaseName, replaceExtension)

--main :: IO ()
--main = defaultMain =<< tests

--tests :: IO TestTree
--tests = do
--  yamlFiles <- findByExtension [".yaml"] "."
--  return $ testGroup "YamlToJson golden tests"
--    [ goldenVsString
--        (takeBaseName yamlFile) -- test name
--        jsonFile -- golden file path
--        (LBS.readFile yamlFile) -- action whose result is tested
--    | yamlFile <- yamlFiles
--    , let jsonFile = replaceExtension yamlFile ".json"
--    ]

--tests :: IO TestTree
--tests = do
--  golden <- goldenVsString
--    "foo output matches golden file"
--    "test/golden/foo.golden"
--    (return $ BL.pack $ map (fromIntegral . fromEnum) "Hello, world!\n")
--  return $ testGroup "Golden tests" [golden]
--
--tests :: IO TestTree
test_golden :: TestTree
test_golden = testGroup "Golden tests"
  [ goldenVsString
      "foo output matches golden file"
      ".golden/foo.golden"
      (return $ BL.pack $ map (fromIntegral . fromEnum) "Hello, world!\n")
  ]