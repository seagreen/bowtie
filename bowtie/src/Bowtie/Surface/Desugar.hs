module Bowtie.Surface.Desugar
  ( desugar
  , extractResult
  , clusterLetBindings
  ) where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude hiding (all, rem)
import Bowtie.Surface.AST

import qualified Bowtie.Core.Expr as Core
import qualified Bowtie.Lib.Builtin as Builtin
import qualified Bowtie.Lib.OrderedMap as OrderedMap
import qualified Data.Graph as Graph
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

extractResult :: OrderedMap Id (Expr, Type) -> Expr
extractResult decls =
  case OrderedMap.lookup (Id "result") decls of
    Nothing ->
      panic "result id not found"

    Just (resultExpr, _typ) ->
      Let (OrderedMap.delete (Id "result") decls) resultExpr

-- | Used by both inferece and desugaring to core.
clusterLetBindings :: OrderedMap Id (Expr, Type) -> [[(Id, (Expr, Type))]]
clusterLetBindings decls =
  foldr f mempty components
  where
    components :: [Graph.SCC ((Expr, Type), Id, [Id])]
    components =
      Graph.stronglyConnCompR (fmap g (OrderedMap.toList decls))
      where
        g :: (Id, (Expr, Type)) -> ((Expr, Type), Id, [Id])
        g (id, (expr, typ)) =
          ((expr, typ), id, Set.toList (freeVars expr))

    f :: Graph.SCC ((Expr, Type), Id, ids) -> [[(Id, (Expr, Type))]] -> [[(Id, (Expr, Type))]]
    f grph acc =
      case grph of
        Graph.AcyclicSCC (expr, id, _) ->
          [(id, expr)] : acc

        -- A binding that refers to itself
        Graph.CyclicSCC [(expr, id, _)] ->
          [(id, expr)] : acc

        -- Mutually recursive bindings
        Graph.CyclicSCC bindings ->
          fmap (\(expr, id, _) -> (id, expr)) bindings : acc

desugar :: Expr -> Core.Expr
desugar topExpr =
  case topExpr of
    Var i ->
      Core.Var i

    Lam id mType e ->
      case mType of
        Nothing ->
          panic "desugar type is Nothing"

        Just typ ->
          Core.Lam id typ (desugar e)

    App e1 e2 ->
      Core.App (desugar e1) (desugar e2)

    Let decls e ->
      desugarLet decls e

    Construct tag ->
      Core.Construct tag

    Case e matches ->
      let
        f :: Alt -> Core.Alt
        f (Alt i i2 expr) =
          Core.Alt i i2 (desugar expr)
      in
        Core.Case (desugar e) (fmap f matches)

    IntLiteral n ->
      Core.PrimInt n

    TextLiteral t ->
      desugarText t

-- | See 'showIntBuiltin' for a similar function.
--
-- I tried to factor out the common logic into:
--
-- @Builtin.text :: forall expr. (expr -> expr -> expr) -> (Id -> expr) -> [expr] -> expr@
--
-- But the way untyped constructors are defined as @Construct Id [Expr]@
-- instead of @Construct Id@ made this difficult (and I'm not sure it was worth it anyway).
desugarText :: Text -> Core.Expr
desugarText =
  Core.App (Core.Construct Builtin.unicode) . toList'
  where
    toList' :: Text -> Core.Expr
    toList' =
      Text.foldr consCodepoint (Core.Construct Builtin.nil)

    consCodepoint :: Char -> Core.Expr -> Core.Expr
    consCodepoint c expr =
      let
        consCodePoint :: Core.Expr
        consCodePoint =
          Core.App
            (Core.Construct Builtin.cons)
            (Core.PrimInt (fromIntegral (charToCodepoint c)))
      in
        Core.App consCodePoint expr

-- | This one isn't used for inference, but just going to core.
desugarLet :: OrderedMap Id (Expr, Type) -> Expr -> Core.Expr
desugarLet decls body =
  foldr
    addLet
    (desugar body)
    (clusterLetBindings decls)
  where
    addLet :: [(Id, (Expr, Type))] -> Core.Expr -> Core.Expr
    addLet recursiveBindings acc =
      let
        f :: (Id, (Expr, Type)) -> (Id, (Core.Expr, Type))
        f (id, (expr, typ)) =
          (id, (desugarBinding id expr, typ))
      in
        Core.Let (HashMap.fromList (fmap f recursiveBindings)) acc

-- | Internal. Used by 'desugarLet'.
desugarBinding :: Id -> Expr -> Core.Expr
desugarBinding id expr =
  -- Intercept builtins, and replace their current
  -- definition (panic) with something else.
  --
  -- By doing this here we replace their definitions in the
  -- source code with a new value.
  -- The old way of doing it was to case on id in the Lam
  -- case of desugar, which replaced the call sites instead of
  -- the function definitions.
  case id of
    Id "compare" ->
      let
        a = Id "a"
        b = Id "b"
        aType = TVariable (Id "a")
      in
        Core.Lam
          a
          aType
          (Core.Lam
            b
            aType
            (Core.PrimOp
              (Core.Compare
                (Core.Var a)
                (Core.Var b))))

    Id "plus" ->
      let
        a = Id "a"
        b = Id "b"
        iType = TConstructor Builtin.int
      in
        Core.Lam
          a
          iType
          (Core.Lam
            b
            iType
            (Core.PrimOp
              (Core.Plus
                (Core.Var a)
                (Core.Var b))))

    Id "multiply" ->
      let
        a = Id "a"
        b = Id "b"
        iType = TConstructor Builtin.int
      in
        Core.Lam
          a
          iType
          (Core.Lam
            b
            iType
            (Core.PrimOp
              (Core.Multiply
                (Core.Var a)
                (Core.Var b))))

    Id "showInt" ->
      let
        a = Id "a"
        arrTyp = TArrow (TConstructor Builtin.int) (TConstructor Builtin.text)
      in
        Core.Lam
          a
          arrTyp
          (Core.PrimOp
            (Core.ShowInt
              (Core.Var a)))

    Id "panic" ->
      let
        a = Id "a"
        textType = TConstructor Builtin.text
      in
        Core.Lam
          a
          textType
          (Core.PrimOp
            (Core.Panic
              (Core.Var a)))

    _ ->
      desugar expr
