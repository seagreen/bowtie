module Bowtie.Infer.BottomUp where

import Bowtie.Infer.Assumptions (Assumptions)
import Bowtie.Infer.Constraints
import Bowtie.Lib.Environment
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Surface.AST
import Control.Monad.State.Class

import qualified Bowtie.Infer.Constraints as Constraints
import qualified Bowtie.Infer.Assumptions as Assumptions
import qualified Bowtie.Lib.Builtin as Builtin
import qualified Bowtie.Surface.Desugar as Desugar
import qualified Data.List as List
import qualified Data.Set as Set

bottomUp
  :: forall m. MonadState Int m
  => Environment
  -> Set Id
  -> Expr
  -> m (Assumptions, Constraints, Type)
bottomUp env ms topExpr =
  case topExpr of
    Var id -> do
      var <- fmap TVariable genVar
      pure (Assumptions.singleton id var, mempty, var)

    Lam id _mType expr -> do
      (a1, c1, t1) <- bottomUp env (Set.insert id ms) expr
      newTypeVar <- fmap TVariable genVar

      let
        newC :: Constraints
        newC =
          case Assumptions.lookup id a1 of
            Nothing ->
              mempty

            Just ts ->
              Constraints (Set.map (\t -> EqualityConstraint newTypeVar t) ts)

      pure (Assumptions.delete id a1, c1 <> newC, TArrow newTypeVar t1)

    App e1 e2 -> do
      (a1, c1, t1) <- bottomUp env ms e1
      (a2, c2, t2) <- bottomUp env ms e2
      var <- fmap TVariable genVar
      pure (a1 <> a2, c1 <> c2 <> Constraints.singleton (EqualityConstraint t1 (TArrow t2 var)), var)

    Let bindings expr ->
      bottomUpLet env ms bindings expr

    Construct id -> do
      var <- fmap TVariable genVar
      pure (Assumptions.singleton id var, mempty, var)

    Case expr alts ->
      bottomUpCase env ms expr alts

    EInt _ ->
      pure (mempty, mempty, TConstructor Builtin.int)

    TextLiteral _ ->
      pure (mempty, mempty, TConstructor Builtin.text)

bottomUpLet
  :: forall m. MonadState Int m
  => Environment
  -> Set Id
  -> OrderedMap Id (Expr, Type)
  -> Expr
  -> m (Assumptions, Constraints, Type)
bottomUpLet env ms bindings expr = do
  -- TODO: need to punch out some ms

  let
    bindingList :: [(Id, (Expr, Type))]
    bindingList =
      List.reverse (Desugar.flattenLetBindings bindings)

  (aBody, cBody, tBody) <- bottomUp env ms expr

  let
    f :: (Assumptions, Constraints) -> (Id, (Expr, Type)) -> m (Assumptions, Constraints)
    f (a2, c2) (id, (e, typeAnnotation)) = do
      (a1, c1, t1) <- bottomUp env ms e

      let
        newC :: Constraints
        newC =
          case Assumptions.lookup id (a1 <> a2) of -- todo: not sure if a1 <> a2 is right
            Nothing ->
              mempty

            Just ts ->
              -- using typeAnnotation instead of t1 here because it's less general
              Constraints (Set.map (\t -> ImplicitInstanceConstraint t ms typeAnnotation) ts)

                <> Constraints.singleton (ImplicitInstanceConstraint t1 ms typeAnnotation)

      -- note that we delete the id from BOTH a1 (in case of recursion) and a2
      pure (Assumptions.delete id (a1 <> a2), c1 <> c2 <> newC)

  (aNew, cNew) <- foldM f (aBody, cBody) bindingList
  pure (aNew, cNew, tBody)

bottomUpCase
  :: forall m. MonadState Int m
  => Environment
  -> Set Id
  -> Expr
  -> [Alt]
  -> m (Assumptions, Constraints, Type)
bottomUpCase env ms _expr alts = do -- TODO: use expr
  let
    -- Eg, for
    --
    -- case a of
    --   Nothing ->
    --     foo
    --
    --   Just b ->
    --     bar b
    --
    -- The b in Just b would be a binding
    -- The Just would be an id
    addConInfo :: Alt -> m (Alt, [(Id, Type)])
    addConInfo alt@(Alt conId bindings _) =
      case lookup conId env of
        Nothing ->
          panic "conid"

        Just (TypeScheme _ typ) -> do
          let
            g :: (Type, [(Id, Type)]) -> Id -> m (Type, [(Id, Type)])
            g (t, xs) binding =
              case t of
                TArrow targ t2 ->
                  pure (t2, (binding, targ) : xs)

                _ ->
                  panic "whoops"

          (_, args) <- foldM g (typ, mempty) bindings
          pure (alt, args)
  res :: [(Alt, [(Id, Type)])] <- for alts addConInfo

  let
    inferOne :: (Alt, [(Id, Type)]) -> m (Assumptions, Constraints, Type)
    inferOne (Alt _ _ ex, xs) = do
      let idds = fmap fst xs
      (aa, cc, t) <- bottomUp env (ms <> Set.fromList idds) ex
      let
        neC :: (Id, Type) -> Constraints -> Constraints
        neC (id, rt) oldc =
          case Assumptions.lookup id aa of
            Nothing ->
              oldc

            Just ts ->
              oldc <> Constraints (Set.map (\tc -> EqualityConstraint tc rt) ts)

        newC :: Constraints
        newC =
          foldr neC mempty xs
      pure (foldr Assumptions.delete aa idds, cc <> newC, t)
  res2 <- for res inferOne

  let
    go :: Type -> (Assumptions, Constraints) -> (Assumptions, Constraints, Type) -> m (Assumptions, Constraints)
    go t (a, c) (aa, cc, tt) =
      pure (a <> aa, c <> cc <> Constraints.singleton (EqualityConstraint tt t))
  case res2 of
    (aaa, ccc, t) : xs -> do
      (aZ, cZ) <- foldM (go t) (aaa, ccc) xs
      pure (aZ, cZ, t)

    [] ->
      panic "no cases"
