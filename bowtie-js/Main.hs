module Main where

import qualified Bowtie.Interpret as Interpret
import Bowtie.JS
import Bowtie.Lib.Prelude
import qualified Data.Text.IO as TIO
import Options.Applicative

main :: IO ()
main = do
  Config name <- args
  appSource <- TIO.readFile name
  libFiles <- readDirectoryFiles "example-lib"
  case Interpret.sourcesToCore libFiles (name, appSource) of
    Left e ->
      exitWithError (Interpret.prettyError e)
    Right (env, coreExpr) ->
      TIO.putStrLn (transpileCore env coreExpr)

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
        <$> argument
          str
          ( metavar "FILE"
              <> help "Path to source file"
          )
