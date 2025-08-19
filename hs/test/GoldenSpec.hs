module GoldenSpec (test_golden) where

import Run

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS

import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"] "examples")

transform :: LBS.ByteString -> IO LBS.ByteString
transform = pure
--transform = pure . LBS.fromString . run . LBS.toString
--transform source = do
--  let src = LBS.toString source
--  putStrLn src
--  pure $ LBS.fromString $ run src

test_golden :: TestTree
test_golden =
  testGroup "Golden tests"
    [ goldenVsString
        (takeBaseName inFile)
        (".golden" </> takeBaseName inFile <.> "lazy")
        (transform =<< LBS.readFile inFile)
    | inFile <- inputFiles
    ]