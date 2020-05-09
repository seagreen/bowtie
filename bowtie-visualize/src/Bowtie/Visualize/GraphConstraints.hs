{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bowtie.Visualize.GraphConstraints where

import Bowtie.Infer.Constraints
import qualified Bowtie.Infer.Constraints as Constraints
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Type.AST
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes as Attributes
import qualified Data.GraphViz.Printing as GP
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL

instance GV.PrintDot TypeScheme where
  unqtDot (TypeScheme _vars t) =
    GP.text (TL.fromStrict res)
    where
      res :: Text
      res =
        "\"" <> prettyType t <> "\""

prettyType :: Type -> Text
prettyType t =
  case t of
    TArrow t1 t2 ->
      prettyType t1 <> " -> " <> prettyType t2
    TConstructor (Id id) ->
      id
    TVariable (Id id) ->
      id
    TypeApp _ _ ->
      "TypeApp"

ff :: Set Constraint -> [(TypeScheme, TypeScheme, Constraint)]
ff cs =
  fmap f (Set.toList cs)
  where
    f :: Constraint -> (TypeScheme, TypeScheme, Constraint)
    f c =
      case c of
        EqualityConstraint t1 t2 ->
          (TypeScheme mempty t1, TypeScheme mempty t2, c)
        ExplicitInstanceConstraint t ts ->
          (TypeScheme mempty t, ts, c)
        ImplicitInstanceConstraint t1 _ t2 ->
          (TypeScheme mempty t1, TypeScheme mempty t2, c)

graphConstraints :: Constraints -> Text
graphConstraints cst@(Constraints cs) =
  TL.toStrict (GP.renderDot dot)
  where
    dot :: GP.DotCode
    dot =
      GP.toDot graphInDotFormat
    graphInDotFormat :: GV.DotGraph TypeScheme
    graphInDotFormat =
      GV.graphElemsToDot params nodes edges
    nodes :: [(TypeScheme, Text)]
    nodes =
      fmap (\c -> (c, "todo")) (Set.toList (Constraints.toTypeSchemes cst))
    edges :: [(TypeScheme, TypeScheme, Constraint)]
    edges =
      ff cs

params :: GV.GraphvizParams TypeScheme Text Constraint () Text
params =
  GV.nonClusteredParams
    { GV.fmtNode = nodeFmt,
      GV.fmtEdge = edgeFmt
    }
  where
    nodeFmt :: (TypeScheme, Text) -> [GV.Attribute]
    nodeFmt (_, _) = [] -- [GV.toLabel l]
    edgeFmt :: (TypeScheme, TypeScheme, Constraint) -> GV.Attributes
    edgeFmt (_, _, c) =
      case c of
        EqualityConstraint {} ->
          [Attributes.edgeEnds Attributes.NoDir]
        ExplicitInstanceConstraint {} ->
          [Attributes.color Attributes.Yellow]
        ImplicitInstanceConstraint {} ->
          [Attributes.color Attributes.Red]
