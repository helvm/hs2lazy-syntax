module GoldenSpec (test_golden) where

import Run

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LBS

import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)

inputFiles :: [FilePath]
inputFiles = unsafePerformIO (findByExtension [".hs"]  ("examples" </> "apps"))

preludeIO :: IO String
preludeIO = LBS.toString <$> LBS.readFile ("examples" </> "libs" </> "hs2lazy-prelude.hs")

transform :: LBS.ByteString -> IO LBS.ByteString
transform source = do
  prelude <- preludeIO
  let src = LBS.toString source
  ski <- runIO $ prelude ++ src
  pure $ LBS.fromString $ ski

test_golden :: TestTree
test_golden =
  testGroup "Golden tests"
    [ goldenVsString
        (takeBaseName inFile)
        (".golden" </> takeBaseName inFile <.> "lazy")
        (transform =<< LBS.readFile inFile)
    | inFile <- inputFiles
    ]