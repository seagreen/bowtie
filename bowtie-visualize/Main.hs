module Main (main) where

import Bowtie.Lib.Prelude
import Bowtie.Visualize
import Options.Applicative

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  Config name <- args
  appSource <- TIO.readFile name
  libFiles <- readDirectoryFiles "example-lib"
  constraints <- run libFiles (name, appSource)
  writeConstraints constraints

data Config = Config FilePath

args :: IO Config
args =
  customExecParser (prefs showHelpOnError) configParser

configParser :: ParserInfo Config
configParser =
  info (helper <*> parser) fullDesc
  where
    parser :: Parser Config
    parser =
      Config
        <$> argument str
            (  metavar "FILE"
            <> help "Path to source file"
            )
