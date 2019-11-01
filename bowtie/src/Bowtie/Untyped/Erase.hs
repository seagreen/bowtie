module Bowtie.Untyped.Erase where

import Bowtie.Core.Expr
import Bowtie.Lib.Id
import Bowtie.Lib.Prelude

import qualified Bowtie.Untyped.Expr as U
import qualified Data.HashMap.Strict as HashMap

erase :: Expr -> U.Expr
erase topExpr =
  case topExpr of
    Var i ->
      U.Var i

    Lam id _ e ->
      U.Lam mempty id (erase e)

    App e1 e2 ->
      U.App (erase e1) (erase e2)

    Let decls e ->
      U.Let (fmap (erase . fst) decls) (erase e)

    Construct tag ->
      U.Construct tag mempty

    Case e matches ->
      let
        f :: Alt -> HashMap Id U.Match
        f (Alt i i2 expr) =
          HashMap.singleton i (U.Match i2 (erase expr))
      in
        U.Case (erase e) (foldMap f matches)

    EInt n ->
      U.EInt n

    EOp op ->
      U.EOp (eraseOperation op)

eraseOperation :: Operation -> U.Operation
eraseOperation op =
  case op of
    Compare e1 e2 ->
      U.Compare (erase e1) (erase e2)

    Plus e1 e2 ->
      U.Plus (erase e1) (erase e2)

    Multiply e1 e2 ->
      U.Multiply (erase e1) (erase e2)

    ShowInt e ->
      U.ShowInt (erase e)

    Panic e ->
      U.Panic (erase e)
