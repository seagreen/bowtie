module Main where

import Bowtie.Blueprint
import Bowtie.Lib.Prelude
import System.Directory
import System.FilePath (takeExtension, (</>))
import Test.Hspec

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

dir :: FilePath
dir =
  "example"

main :: IO ()
main = do
  blueprintExamples <- getBlueprintExamples

  hspec do
    describe "blueprint" $
      for_ blueprintExamples g

  where
    g :: FilePath -> Spec
    g path =
      it path do
        src <- TIO.readFile (dir </> path)
        case blueprint src of
          Left e ->
            expectationFailure (Text.unpack e)

          Right _ ->
            pure ()

getBlueprintExamples :: IO [FilePath]
getBlueprintExamples = do
  appPaths <- listDirectory dir
  let
    (blueprints, other) =
      List.partition (\path -> takeExtension path == ".md") appPaths

  when
    (other /= mempty)
    (panic ("blueprints not recognized: " <> show other))

  pure blueprints
