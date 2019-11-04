module Bowtie.JS.Serialize
  ( serializeTop
  , serialize
  ) where

import Bowtie.JS.AST
import Bowtie.Lib.Prelude

import qualified Data.Text as Text

serializeTop :: AST -> Text
serializeTop ast =
  case ast of
    Block xs ->
      Text.intercalate "\n" (fmap serialize xs)

    _ ->
      serialize ast

serialize :: AST -> Text -- TODO: text builder
serialize topAst =
  case topAst of
    Var id ->
      serializeId id

    Lam id ast ->
      serializeId id <> " => " <> serialize ast

    App a1 a2 ->
      serialize a1 <> "(" <> serialize a2 <> ")"

    Assignment a1 a2 ->
      "const " <> serialize a1 <> " = " <> serialize a2 <> ";"

    Block asts ->
      Text.intercalate "\n" (fmap serialize asts)

    Return ast ->
      "return " <> serialize ast

    Array asts ->
      "[" <> Text.intercalate ", " (fmap serialize asts) <> "]"

    IndexArray ast index ->
      serialize ast <> "[" <> show index <> "]"

    IfThen a1 a2 ->
      "if (" <> serialize a1 <> ") { " <> serialize a2 <> "}"

    Else ast ->
      " else { " <> serialize ast <> " }"

    Throw ast ->
      "throw " <> serialize ast

    Equal a1 a2 ->
      serialize a1 <> " === " <> serialize a2

    LambdaUnit ast ->
      "(() => { " <> serialize ast <> "})()"

    JSInt n ->
      show n

    JSString t ->
      "\"" <> t <> "\""

    Compare ast1 ast2 ->
      "$compareBuiltin(" <> serialize ast1 <> ", " <> serialize ast2 <> ")"

    Plus ast1 ast2 ->
      "(" <> serialize ast1 <> " + " <> serialize ast2 <> ")"

    Multiply ast1 ast2 ->
      "(" <> serialize ast1 <> " * " <> serialize ast2 <> ")"

    ShowInt ast ->
      "$unicodeListizeBuiltin(" <> serialize ast <> ".toString())"

serializeId :: Id -> Text
serializeId (Id t) =
  "_" <> t
