module Bowtie.Visualize
  ( run
  , writeConstraints
  ) where

import Bowtie.Infer.Constraints
import Bowtie.Infer.Solve
import Bowtie.Infer.Solve (next, solveConstraint)
import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Bowtie.Surface.AST (AST(astTerms, astTypes))
import Bowtie.Type.Kindcheck (kindcheck)
import Bowtie.Visualize.GraphConstraints (graphConstraints)
import Control.Monad.State.Class
import System.Process.Typed

import qualified Bowtie.Infer.Constraints as Constraints
import qualified Bowtie.Interpret as Interpret
import qualified Bowtie.Surface.AST as Surface
import qualified Bowtie.Surface.Desugar as Surface.Desugar
import qualified Bowtie.Surface.Infer as Infer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text

run :: HashMap FilePath Text -> (FilePath, Text) -> IO [Constraints]
run libFiles appFile = do
  case Interpret.sourcesToAST libFiles appFile of
    Left e ->
      exitWithError (Interpret.prettyError e)

    Right ast -> do
      let
        env :: Environment
        env =
          kindcheck (astTypes ast)

        dsg :: Surface.Expr
        dsg =
          Surface.Desugar.extractResult (astTerms ast)

      let
        f :: ( MonadState Int m
             , CanSolveStuck m
             , CanUnifyError m
             , Infer.CanAssumptionsRemain m
             ) => m [Constraints]
        f = do
          (constraints, _) <- Infer.gatherConstraints env dsg
          solutionSteps constraints

      case Infer.runInfer f of
        Left e ->
          throwText (show e)

        Right constraints ->
          pure constraints

writeConstraints :: [Constraints] -> IO ()
writeConstraints cs =
  for_ (List.zip cs [1..]) f
  where
    f :: (Constraints, Natural) -> IO ()
    f (c, n) = do
      bts <- runCommand "dot" "-Tsvg" (encodeUtf8 (graphConstraints c))
      BS.writeFile (Text.unpack (show n <> ".svg")) bts

solutionSteps
  :: (MonadState Int m, CanSolveStuck m, CanUnifyError m)
  => Constraints
  -> m [Constraints]
solutionSteps cs = do
  case next cs of
    Nothing ->
      if Constraints.isEmpty cs
        then
          pure mempty

        else
          failSolveStuck

    Just (c, rest) -> do
      (_sub, rest2) <- solveConstraint c rest
      fmap (rest:) (solutionSteps rest2)

-- | Below should be in a lib somewhere
--
-- NOTE: Only used with trused input!
runCommand
  :: Text -- ^ Command injection vulnerability when passed untrusted input.
  -> Text -- ^ Command injection vulnerability when passed untrusted input.
  -> ByteString
  -> IO ByteString
runCommand cmd arg input = do
  fmap LBS.toStrict (readProcessStdout_ proc2)
  where
    -- Command with argument
    proc1 :: ProcessConfig () () ()
    proc1 =
      shell (Text.unpack cmd <> " " <> Text.unpack arg)

    -- Command with argument and stdin
    proc2 :: ProcessConfig () () ()
    proc2 =
      setStdin (byteStringInput (LBS.fromStrict input)) proc1
