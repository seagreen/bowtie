module Main where

import Bowtie.Lib.Prelude
import Options.Applicative

import qualified Bowtie.Interpret as Interpret
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  Config name <- args
  appSource <- TIO.readFile name
  libFiles <- Interpret.getLibFiles "example-lib"
  untypedValue <- Interpret.interpretProgramIO libFiles (name, appSource)
  TIO.putStrLn (show untypedValue)

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