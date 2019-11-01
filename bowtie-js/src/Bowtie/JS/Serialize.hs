module Bowtie.JS.Serialize
  ( serializeTop
  , serialize
  , serializeOperation
  , experize
  ) where

import Bowtie.JS.AST
import Bowtie.Lib.Id
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
      let
        addReturn ys =
          case reverse ys of
            y:rest ->
              reverse rest <> [Return y]

            _ ->
              ys
      in
        experize (Text.intercalate "\n" (fmap serialize (addReturn asts)))

    Return ast ->
      "return " <> serialize ast

    Array asts ->
      "[" <> Text.intercalate ", " (fmap serialize asts) <> "]"

    Case ast alts ->
      let
        mkAssign :: (Natural, Id) -> Text
        mkAssign (n, id) =
          "const " <> serializeId id <> " = $1[" <> show n <> "];\n"

        f :: Alt -> Text
        f (Alt id bindings expr) =
           -- unId not serializeId because it's ["Unit"] not [Unit].
          "if ($1[0] === \"" <> unId id <> "\") {"
            <> foldMap mkAssign (zip [1..] bindings)
            <> "return " <> serialize expr <> "} else "
      in
        "(() => { const $1 = " <> serialize ast <> ";\n"
          <> foldMap f alts
          <> " {throw \"no match\";} })()"

    JSInt n ->
      show n

    JSString t ->
      "\"" <> t <> "\""

    JSOp op ->
      serializeOperation op

experize :: Text -> Text
experize t =
  "(() => { " <> t <> "})()"

serializeOperation :: Operation -> Text
serializeOperation op =
  case op of
    Compare ast1 ast2 ->
      "$compareBuiltin(" <> serialize ast1 <> ", " <> serialize ast2 <> ")"

    Plus ast1 ast2 ->
      "(" <> serialize ast1 <> " + " <> serialize ast2 <> ")"

    Multiply ast1 ast2 ->
      "(" <> serialize ast1 <> " * " <> serialize ast2 <> ")"

    ShowInt ast ->
      "$unicodeListizeBuiltin(" <> serialize ast <> ".toString())"

    Panic expr -> -- Only works on Text
      experize ("throw " <> serialize expr)

serializeId :: Id -> Text
serializeId (Id t) =
  "_" <> t
