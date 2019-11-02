module Main where

import Bowtie.Lib.Prelude
import System.Directory
import System.FilePath (takeExtension, (</>))
import Test.Hspec

import qualified Bowtie.Example
import qualified Bowtie.Interpret as Interpret
import qualified Bowtie.Surface.InferSpec
import qualified Bowtie.Surface.Parse as Surface.Parse
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec as Mega

dir :: FilePath
dir =
  "example-app"

main :: IO ()
main = do
  -- hack, because changing dir to "../app" makes listDirectory fail.
  withCurrentDirectory ".." do
    appExamples <- (fmap.fmap) (\p -> dir </> p) getAppExamples
    hspec do
      describe
        "valid-syntax-examples"
        (for_ Bowtie.Example.validSyntax run)

      describe
        "invalid-syntax-examples"
        (for_ Bowtie.Example.invalidSyntax runInvalidSyntax)

      describe
        "well-typed-examples"
        (for_ Bowtie.Example.wellTyped run)

      describe
        "ill-typed-examples"
        (for_ Bowtie.Example.illTyped runIllTyped)

      Bowtie.Surface.InferSpec.spec

      describe "example-app" $
        for_ appExamples f
  where
    f :: FilePath -> Spec
    f path =
      it path do
        libFiles <- readDirectoryFiles "example-lib" -- TODO: wasted work
        appSource <- TIO.readFile path
        case Interpret.interpretProgram libFiles (path, appSource) of
          Left e ->
            expectationFailure (Text.unpack (Interpret.prettyError e))

          Right _ ->
            pure ()

run :: (FilePath, Text) -> Spec
run (name, src) =
  it name (Interpret.interpret src `shouldSatisfy` isRight)

runInvalidSyntax :: (FilePath, Text) -> Spec
runInvalidSyntax (name, src) =
  it name $
    case Surface.Parse.parse name src of
      Left e ->
        TIO.writeFile
          ("bowtie" </> "test" </> "invalid-syntax-examples" </> name)
          (Text.pack (Mega.errorBundlePretty e))

      Right _ ->
        expectationFailure "Unexpected Right"

runIllTyped :: (FilePath, Text) -> Spec
runIllTyped (name, src) =
  it name $
    case Interpret.sourcesToCore mempty ("<input>", src) of
      Left err ->
        case err of
          Interpret.ParseError _ ->
            expectationFailure
              ("Unexpected parse error: " <> Text.unpack (Interpret.prettyError err))

          Interpret.NameClash t ->
            expectationFailure
              ("Unexpected name clash error : " <> Text.unpack t)

          Interpret.TypeError e ->
            TIO.writeFile
              ("bowtie" </> "test" </> "ill-typed-examples" </> name)
              (show e)

      Right _ ->
        expectationFailure "Unexpected Right"

getAppExamples :: IO [FilePath]
getAppExamples = do
  appPaths <- listDirectory dir
  let
    (langs, other) =
      List.partition (\path -> takeExtension path == ".bowtie") appPaths

  when
    (other /= mempty)
    (panic ("examples not recognized: " <> show other))

  pure langs
