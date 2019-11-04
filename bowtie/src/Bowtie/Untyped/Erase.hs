module Bowtie.Untyped.Erase where

import Bowtie.Lib.Prelude
import Bowtie.Untyped.Expr

import qualified Bowtie.Core.Expr as Core
import qualified Data.HashMap.Strict as HashMap

erase :: Core.Expr -> Expr
erase topExpr =
  case topExpr of
    Core.Var i ->
      Var i

    Core.Lam id _ e ->
      Lam mempty id (erase e)

    Core.App e1 e2 ->
      App (erase e1) (erase e2)

    Core.Let decls e ->
      Let (fmap (erase . fst) decls) (erase e)

    Core.Construct tag ->
      Construct tag mempty

    Core.Case e matches ->
      let
        f :: Core.Alt -> HashMap Id Match
        f (Core.Alt i i2 expr) =
          HashMap.singleton i (Match i2 (erase expr))
      in
        Case (erase e) (foldMap f matches)

    Core.PrimInt n ->
      PrimInt n

    Core.PrimOp op ->
      PrimOp (eraseOperation op)

eraseOperation :: Core.Operation -> Operation
eraseOperation op =
  case op of
    Core.Compare e1 e2 ->
      Compare (erase e1) (erase e2)

    Core.Plus e1 e2 ->
      Plus (erase e1) (erase e2)

    Core.Multiply e1 e2 ->
      Multiply (erase e1) (erase e2)

    Core.ShowInt e ->
      ShowInt (erase e)

    Core.Panic e ->
      Panic (erase e)
