{-# LANGUAGE TemplateHaskell #-}

module Bowtie.Example
  ( wellTyped,
    illTyped,
    validSyntax,
    invalidSyntax,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Prelude

-- | Return a [(Text, Text)] so the examples are always deterministically sorted.
--
-- (If we return a @HashMap Text Text@ it's too easy to use @HashMap.toList@,
-- which doesn't sort deterministically. Then users will be surprised
-- when their test examples change order. Ideally we'd use @OrderedMap@, but
-- we don't want this package to depend on the language implementation.)
wellTyped :: [(FilePath, Text)]
wellTyped =
  process $(makeRelativeToProject "well-typed-examples" >>= embedDir)

illTyped :: [(FilePath, Text)]
illTyped =
  process $(makeRelativeToProject "ill-typed-examples" >>= embedDir)

validSyntax :: [(FilePath, Text)]
validSyntax =
  process $(makeRelativeToProject "valid-syntax-examples" >>= embedDir)

invalidSyntax :: [(FilePath, Text)]
invalidSyntax =
  process $(makeRelativeToProject "invalid-syntax-examples" >>= embedDir)

-- | Internal
process :: [(FilePath, ByteString)] -> [(FilePath, Text)]
process =
  -- Not sure if sorting is necessary-- will file-embed always sort its results?
  sortOn fst . (fmap . fmap) f
  where
    f :: ByteString -> Text
    f bts =
      case decodeUtf8' bts of
        Left e ->
          error ("Invalid example: " <> show e)
        Right t ->
          t
