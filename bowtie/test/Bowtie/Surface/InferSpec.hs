{-# LANGUAGE QuasiQuotes #-}

module Bowtie.Surface.InferSpec where

import Bowtie.Infer.Substitution
import Bowtie.Lib.Environment
import Bowtie.Lib.Prelude
import Bowtie.Lib.TypeScheme
import Bowtie.Surface.AST
import Bowtie.Surface.Desugar
import Bowtie.Surface.Infer
import Bowtie.Type.Kindcheck
import qualified Data.HashMap.Strict as HashMap
import Test.Hspec
import Test.Shared

unitEnv :: Environment
unitEnv =
  Environment (HashMap.singleton (Id "Unit") (TypeScheme mempty (TConstructor (Id "Unit"))))

inferProgram :: AST -> Either TypeError Type
inferProgram ast = do
  (_, b, _) <-
    elaborate (kindcheck (astTypes ast)) (extractResult (astTerms ast))
  pure b

infer :: Environment -> Expr -> Either TypeError (Substitution, Type)
infer env expr =
  runInfer (inferType env expr)

spec :: Spec
spec = do
  describe "heeren-hage-unit-tests" do
    it "1" $
      infer mempty (IntLiteral 1) `shouldBe` Right (mempty, TConstructor (Id "Int"))
    it "abc" $
      infer mempty (TextLiteral "abc") `shouldBe` Right (mempty, TConstructor (Id "Text"))
    it "Unit" $
      fmap snd (infer unitEnv (Construct (Id "Unit")))
        `shouldBe` Right (TConstructor (Id "Unit"))
    it "\\x. x 0" do
      fmap snd (infer mempty [quotedExpr|\x. x 0|])
        `shouldBe` Right (TArrow (TArrow (TConstructor (Id "Int")) (TVariable (Id "1"))) (TVariable (Id "1")))
    it "(\\x. x) 0" do
      fmap snd (infer mempty [quotedExpr|(\x. x) 0|])
        `shouldBe` Right (TConstructor (Id "Int"))

    it "(\\x. x) (\\x. x)" do
      fmap snd (infer mempty [quotedExpr|(\x. x) (\x. x)|])
        `shouldBe` Right (TArrow (TVariable (Id "2")) (TVariable (Id "2")))

    -- ==================================================
    it "let x = 1 in x" do
      let expr =
            [quotedExpr|
let
  x : Int
  x =
    1
in
  x|]
      fmap snd (infer mempty expr)
        `shouldBe` Right
          (TConstructor (Id "Int"))

    -- ==================================================
    it "let x = 1 y = x in y" do
      let expr =
            [quotedExpr|
let
  x : Int
  x =
    1

  y : Int
  y =
    x
in
  y|]
      fmap snd (infer mempty expr)
        `shouldBe` Right (TConstructor (Id "Int"))

    -- ==================================================
    it "let id = \\x. x in id 1" do
      let expr =
            [quotedExpr|
let
  id : Int -> Int
  id =
    \x. x

in
  id 1|]
      fmap snd (infer mempty expr)
        `shouldBe` Right (TConstructor (Id "Int"))

    -- ==================================================
    it "case Unit of Unit -> 0" do
      let expr =
            [quotedExpr|
case Unit of
  Unit -> 0|]
      fmap snd (infer unitEnv expr)
        `shouldBe` Right (TConstructor (Id "Int"))

    --------------------------------------------------
    -- Switching from Expr to Program
    --------------------------------------------------

    -- ==================================================
    it "types a particular situation correctly" do
      let -- This is a close to minimal test for a particular bug--
          -- Reducing it more by even changing type variable names prevents
          -- it from triggering.
          --
          -- See NOTE_ZK9
          program =
            [quotedAST|
type Maybe b = Just b | Nothing

type List a = Cons a (List a) | Nil

type Unit = Unit

-- the error depends on the first letter here, if a there's an error, if z no error
aa : List Unit
aa =
  Cons Unit Nil

bb : List Int
bb =
  Cons 1 Nil

head : List a -> Maybe a
head =
  \xs. case xs of
         Cons x rest -> Just x
         Nil -> Nothing

result : Int
result =
  0
|]
      inferProgram program
        `shouldBe` Right (TConstructor (Id "Int"))

--     it "doesn't allow overly polymorphic type signatures" do
--       let
--         program = [quotedAST|type Maybe a = Just a | Nothing
--
-- result : Maybe a
-- result =
--   Just 0

-- | ]
--        inferProgram program
--          `shouldBe`
--            Right (Substitution (HashMap.fromList [(Id "a",TConstructor (Id "Int"))]),TConstructor (Id "Int"))
