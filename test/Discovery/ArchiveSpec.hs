{-# LANGUAGE TemplateHaskell #-}

module Discovery.ArchiveSpec (spec) where

import Control.Carrier.Finally (runFinally)
import Control.Effect.Lift (sendIO)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Discovery.Archive (extractZip, withArchive)
import Path (Abs, File, Path, mkRelDir, mkRelFile, toFilePath, (</>))
import Path.IO qualified as PIO
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

spec :: Spec
spec = do
  describe "extract zip archive to a temporary location" $ do
    target <- runIO simpleZipPath
    (extractedDir, extractedContentA, extractedContentB) <- runIO $
      runFinally . withArchive extractZip target $ \dir -> do
        contentA <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "a.txt")
        contentB <- sendIO . TIO.readFile . toFilePath $ dir </> $(mkRelDir "simple") </> $(mkRelFile "b.txt")
        pure (dir, contentA, contentB)
    tempDirExists <- runIO $ PIO.doesDirExist extractedDir

    it "should have extracted the correct contents" $ do
      extractedContentB `shouldBe` expectedContentB
      extractedContentA `shouldBe` expectedContentA

    it "should have cleaned up the temporary directory" $ do
      tempDirExists `shouldBe` False

simpleZipPath :: IO (Path Abs File)
simpleZipPath = PIO.resolveFile' "test/Discovery/testdata/simple.zip"

expectedContentA :: Text
expectedContentA = "6b5effe3-215a-49ec-9286-f0702f7eb529"

expectedContentB :: Text
expectedContentB = "8dea86e4-4365-4711-872b-6f652b02c8d9"
