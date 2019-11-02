{-# OPTIONS_GHC -fno-warn-unrecognised-pragmas #-}
-- Normally we use hlint to enforce importing Data.Text as Text,
-- but here we want to import it as X:
{-# HLINT ignore "Avoid restricted qualification" #-}

module Bowtie.Lib.Prelude
  ( module Bowtie.Lib.Prelude
  , module X
  ) where

-- Re-exports:

import Prelude as X hiding
  (error, foldl, id, lookup, map, readFile, show, writeFile)

import Control.Applicative as X
import Control.Monad as X
import Data.Either as X
import Data.Foldable as X
import Data.Maybe as X
import Data.Text.Encoding as X hiding (decodeUtf8)
import Data.Traversable as X
import Data.Void as X

import Control.DeepSeq as X (NFData)
import Data.ByteString as X (ByteString)
import Data.Hashable as X (Hashable)
import Data.HashMap.Strict as X (HashMap)
import Data.Set as X (Set)
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Numeric.Natural as X (Natural)

-- Local stuff:

import Control.Monad.Except
import GHC.Stack.Types (HasCallStack)
import System.Directory (listDirectory)
import System.Exit (exitFailure)
import System.FilePath (FilePath, (</>))
import System.IO (stderr)
import System.IO.Error (ioError, userError)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Prelude

{-# WARNING error "'error' remains in code" #-}
error :: HasCallStack => [Char] -> a
error =
  Prelude.error

panic :: HasCallStack => Text -> a
panic =
  error . Text.unpack

throwText :: Text -> IO a
throwText =
  ioError . userError . Text.unpack

exitWithError :: Text -> IO a
exitWithError e = do
  TIO.hPutStrLn stderr e
  exitFailure

show :: Show a => a -> Text
show =
  Text.pack . Prelude.show

-- | See: https://github.com/haskell/mtl/issues/60
-- Not released yet as of 2.2.2.
mapError
  :: MonadError e' m
  => (e -> e')
  -> ExceptT e m a
  -> m a
mapError f m = do
  res <- runExceptT (withExceptT f m)
  liftEither res

hashmapToSortedList :: Ord k => HashMap k v -> [(k, v)]
hashmapToSortedList =
  List.sortOn fst . HashMap.toList

readDirectoryFiles :: FilePath -> IO (HashMap FilePath Text)
readDirectoryFiles dir = do
  paths <- (fmap.fmap) (\p -> dir </> p) (listDirectory dir)
  fmap HashMap.fromList (for paths f)
    where
      f :: FilePath -> IO (FilePath, Text)
      f path = do
        t <- TIO.readFile path
        pure (path, t)

charToCodepoint :: Char -> Natural
charToCodepoint =
  fromIntegral . Char.ord
