module Bowtie.Infer.BottomUp where

import Bowtie.Infer.Assumptions (Assumptions)
import Bowtie.Infer.Constraints
import Bowtie.Lib.Environment
import Bowtie.Lib.OrderedMap (OrderedMap)
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Surface.AST
import Control.Monad.State.Class

import qualified Bowtie.Infer.Assumptions as Assumptions
import qualified Bowtie.Infer.Constraints as Constraints
import qualified Bowtie.Lib.Builtin as Builtin
import qualified Bowtie.Lib.OrderedMap as OrderedMap
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

    IntLiteral _ ->
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
  (aBody, cBody, tBody) <- bottomUp env monomorphized expr
  (aNew, cNew) <- foldM f (aBody, cBody) bindingList
  pure (deleteAssumptions aNew, cNew <> newC aNew, tBody)
  where
    -- Monomorphized variables are those introduced by Lam.
    -- Remove the ones introduced by this Let.
    monomorphized :: Set Id
    monomorphized =
      Set.difference ms (Set.fromList (OrderedMap.keys bindings))

    bindingList :: [(Id, (Expr, Type))]
    bindingList =
      OrderedMap.toList bindings
      -- TODO: cluster by cycle
      --
      -- List.reverse (Desugar.clusterLetBindings bindings)

    f :: (Assumptions, Constraints) -> (Id, (Expr, Type)) -> m (Assumptions, Constraints)
    f (a2, c2) (_id, (e, typeAnnotation)) = do
      (a1, c1, t1) <- bottomUp env monomorphized e
      let
        annotation :: Constraints
        annotation =
          -- constrain the inferred type to be an instance of the explicit type annotation
          Constraints.singleton (ImplicitInstanceConstraint t1 monomorphized typeAnnotation)
      pure (a1 <> a2, c1 <> c2 <> annotation)

    deleteAssumptions :: Assumptions -> Assumptions
    deleteAssumptions as =
      foldr Assumptions.delete as (OrderedMap.keys bindings)

    newC :: Assumptions -> Constraints
    newC aa =
      let
        g :: (Id, (Expr, Type)) -> Constraints -> Constraints
        g (id, (_, typeAnnotation)) cc =
          case Assumptions.lookup id aa of
            Nothing ->
              cc

            Just ts ->
              -- using typeAnnotation because it's less general than the inferred type
              cc <> Constraints (Set.map (\t -> ImplicitInstanceConstraint t monomorphized typeAnnotation) ts)
      in
        foldr g mempty (OrderedMap.toList bindings)

bottomUpCase
  :: forall m. MonadState Int m
  => Environment
  -> Set Id
  -> Expr
  -> [Alt]
  -> m (Assumptions, Constraints, Type)
bottomUpCase env ms targetExpr alts = do
  (targetA, targetC, targetT) <- bottomUp env ms targetExpr

  processedAlts :: [(TypeScheme, [(Id, Type)], Expr)] <- for alts addConInfo
  let
    inferAlt :: (TypeScheme, [(Id, Type)], Expr) -> m (TypeScheme, Assumptions, Constraints, Type)
    inferAlt (scheme, bindings, expr) = do
      let bindingIds = fmap fst bindings
      (aa, cc, t) <- bottomUp env (ms <> Set.fromList bindingIds) expr
      let
        addC :: (Id, Type) -> Constraints -> Constraints
        addC (bindingId, bindingType) oldC =
          case Assumptions.lookup bindingId aa of
            Nothing ->
              oldC

            Just ts ->
              -- NOTE: should EqualityConstraint be an instance constraint?
              oldC <> Constraints (Set.map (\tc -> EqualityConstraint tc bindingType) ts)

        newC :: Constraints
        newC =
          foldr addC mempty bindings
      pure (scheme, foldr Assumptions.delete aa bindingIds, cc <> newC, t)
  res <- for processedAlts inferAlt

  let
    go
      :: Type
      -> (Assumptions, Constraints)
      -> (TypeScheme, Assumptions, Constraints, Type)
      -> m (Assumptions, Constraints)
    go t (a, c) (_, aa, cc, tt) =
      pure (a <> aa, c <> cc <> Constraints.singleton (EqualityConstraint tt t))
  case res of
    (ts, aaa, ccc, t) : xs -> do
      (aZ, cZ) <- foldM (go t) (aaa, ccc) xs
      pure
        ( targetA <> aZ
        , targetC <> Constraints.singleton (ExplicitInstanceConstraint targetT ts) <> cZ
        , t
        )

    [] ->
      panic "Case statement has no alternatives (this should have been caught by the parser)"
  where
    -- Eg, for
    --
    --   Just b -> b + 1
    --
    -- This would return
    --
    -- (TypeScheme 'a' 'Maybe')
    -- [('b', 'a')]
    -- (b + 1)
    addConInfo :: Alt -> m (TypeScheme, [(Id, Type)], Expr)
    addConInfo (Alt conId bindings expr) =
      case lookup conId env of
        Nothing ->
          panic ("Constructor id not found: " <> unId conId)

        Just (TypeScheme polyVars typ) -> do
          let
            g :: (Type, [(Id, Type)]) -> Id -> m (Type, [(Id, Type)])
            g (t, xs) binding =
              case t of
                TArrow targ t2 ->
                  pure (t2, (binding, targ) : xs)

                _ ->
                  -- TODO: find another way to report this than panic:
                  panic ("Alternative tries to bind too many variables: " <> unId conId)

          (finalType, args) <- foldM g (typ, mempty) bindings
          pure (TypeScheme polyVars finalType, args, expr)
