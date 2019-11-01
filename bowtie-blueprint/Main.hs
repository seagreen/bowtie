module Main (main) where

import Bowtie.Blueprint
import Bowtie.Lib.Prelude
import Options.Applicative

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  Config name <- args
  src <- TIO.readFile name
  void (blueprintIO src)

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
