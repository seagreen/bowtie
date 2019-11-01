{-# LANGUAGE QuasiQuotes #-}

module Bowtie.JS
  ( transpile
  , transpileCore
  , transpileAndExecute
  , appendConsoleLog
  , runTextCommand
  ) where

import Bowtie.Core.Expr
import Bowtie.JS.Imperativize
import Bowtie.JS.Serialize
import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Data.String.QQ
import Data.Text.Encoding (decodeUtf8)
import System.Process.Typed

import qualified Bowtie.Interpret as Interpret
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

appendConsoleLog :: Text -> Text
appendConsoleLog js =
  js <> "\n\nconsole.log(result);"

app1 :: Text
app1 =
  [s|
function arrayToListYYY(xs) {
  const reducer = (accumulator, currentValue) => ["Cons", currentValue, accumulator];
  return xs.reduceRight(reducer, ["Nil"]);
}

function unicodeListize(t) {
  let xs = t.split('').map(c => c.codePointAt())
  return ["Unicode", arrayToListYYY(xs)];
}

function compareYYY(a, b) {
  if (a > b) {
    return ["GreaterThan"];
  } else if (b > a) {
    return ["LessThan"];
  } else {
    return ["Equal"];
  }
}
|]

transpile :: Text -> Either Interpret.IError Text
transpile src = do
  (env, coreExpr) <- Interpret.sourceToCore src
  pure (transpileCore env coreExpr)

transpileCore :: Environment -> Expr -> Text
transpileCore env expr =
  let
    jsAST = makeImp env expr
  in
    "'use strict';\n\n" <> app1 <> "\n" <> serializeTop jsAST

transpileAndExecute :: Text -> IO Text
transpileAndExecute src = do
  let Right js = transpile src
  runTextCommand "node" js

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
