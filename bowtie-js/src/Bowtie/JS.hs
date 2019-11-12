{-# LANGUAGE QuasiQuotes #-}

module Bowtie.JS
  ( transpile
  , transpileCore
  , transpileAndExecute
  , appendConsoleLog
  , runTextCommand
  ) where

import Bowtie.JS.Imperativize (makeImp)
import Bowtie.JS.Serialize (serializeTop)
import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Data.String.QQ (s)
import Data.Text.Encoding (decodeUtf8)
import System.Process.Typed

import qualified Bowtie.Core.Expr as Core
import qualified Bowtie.Interpret as Interpret
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

-- | Internal.
builtinJsSource :: Text
builtinJsSource =
  [s|
function $arrayToListBuiltin(xs) {
  const reducer = (accumulator, currentValue) => ["Cons", currentValue, accumulator];
  return xs.reduceRight(reducer, ["Nil"]);
}

function $unicodeListizeBuiltin(t) {
  let xs = t.split('').map(c => c.codePointAt())
  return ["Unicode", $arrayToListBuiltin(xs)];
}

function $compareBuiltin(a, b) {
  if (a > b) {
    return ["GreaterThan"];
  } else if (b > a) {
    return ["LessThan"];
  } else {
    return ["Equal"];
  }
}
|]

transpile :: Text -> Either Interpret.BowtieError Text
transpile src = do
  (env, coreExpr) <- Interpret.sourcesToCore mempty ("<input>", src)
  pure (transpileCore env coreExpr)

transpileCore :: Environment -> Core.Expr -> Text
transpileCore env expr =
  let
    jsAST = makeImp env expr
  in
    "'use strict';\n\n" <> builtinJsSource <> "\n" <> serializeTop jsAST

transpileAndExecute :: Text -> IO Text
transpileAndExecute src = do
  let Right js = transpile src
  runTextCommand "node" js

appendConsoleLog :: Text -> Text
appendConsoleLog js =
  js <> "\n\nconsole.log(_result);"

-- * Below should be in a lib somewhere

-- | NOTE: Only used with trused input!
runTextCommand
  :: Text -- ^ Command injection vulnerability when passed untrusted input.
  -> Text -- ^ Command injection vulnerability when passed untrusted input.
  -> IO Text
runTextCommand cmd input = do
  res <- runCommand cmd "" (encodeUtf8 input)
  pure (decodeUtf8 res) -- todo

-- | NOTE: Only used with trused input!
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
