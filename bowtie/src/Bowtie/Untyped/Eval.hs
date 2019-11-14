module Bowtie.Untyped.Eval where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.Prelude
import Bowtie.Untyped.Expr
import Data.Function (fix)
import Safe.Exact (zipExactMay)

import qualified Bowtie.Lib.Builtin as Builtin
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

data Error
  = AppNonLambda
  | NotFound Id
  | CaseNotConstruct Expr
  | CaseNoMatch Id (HashMap Id Match)
  | CaseWrongNumberVarsMatched Id [Id]
  | ErrorPanic Text
  | ExpectedAnInt Expr
  deriving (Eq, Show, Generic, NFData)

eval :: TermEnv -> Expr -> Either Error Expr
eval topEnv topExpr =
  case topExpr of
    Var id ->
      lookup id topEnv

    Lam mEnv id expr ->
      evalLam topEnv mEnv id expr

    App e1 e2 ->
      evalApp topEnv e1 e2

    Let decls e -> do
      evalLet topEnv decls e

    Construct tag exps ->
      pure (Construct tag exps)

    Case expr alternatives -> do
      evalCase topEnv expr alternatives

    PrimInt n ->
      pure (PrimInt n)

    PrimOp op ->
      evalOp topEnv op

evalLam :: TermEnv -> Maybe TermEnv -> Id -> Expr -> Either Error Expr
evalLam topEnv mEnv id expr =
  case mEnv of
    Nothing -> do
      let
        free :: HashMap Id ()
        free =
          HashMap.fromList (fmap (\a -> (a, ())) (Set.toList (freeVars expr)))

        newEnv :: TermEnv
        newEnv =
          TermEnv
            (HashMap.intersectionWith
              const
              (unTermEnv topEnv)
              free)

      pure (Lam (Just newEnv) id expr)

    Just _ ->
      pure (Lam mEnv id expr)

evalApp :: TermEnv -> Expr -> Expr -> Either Error Expr
evalApp topEnv e1 e2 = do
  f <- eval topEnv e1
  arg <- eval topEnv e2
  case f of
    Lam mEnv id lamExp -> do
      case mEnv of
        Nothing ->
          panic "unexpected unscoped lambda"

        Just env ->
          eval (addToEnv id arg env) lamExp

    Construct tag exps -> do
      pure (Construct tag (exps <> [arg])) -- PERFORMANCE

    _ ->
      Left AppNonLambda

-- | Risks looping forever!
evalLet :: TermEnv -> HashMap Id Expr -> Expr -> Either Error Expr
evalLet topEnv decls body = do
  evaledDecls <- traverse (eval topEnv) decls
  let
    update :: (Expr -> Expr) -> Expr -> Expr
    update f expr =
      case expr of
        Lam (Just env) id e ->
          let
            fEnv :: TermEnv
            fEnv =
              TermEnv (fmap f evaledDecls <> unTermEnv env)
          in
            Lam (Just fEnv) id e

        _ -> expr

    newEnv :: TermEnv
    newEnv =
      TermEnv (fmap (fix update) evaledDecls <> unTermEnv topEnv)

  eval newEnv body

evalCase :: TermEnv -> Expr -> HashMap Id Match -> Either Error Expr
evalCase topEnv expr alternatives = do
  res <- eval topEnv expr
  case res of
    -- args are 0 Nil in case Cons 0 Nil
    Construct conId args ->
      case HashMap.lookup conId alternatives of
        Nothing ->
          Left (CaseNoMatch conId alternatives)

        Just (Match boundVars newExp) -> do
          xs :: [(Id, Expr)] <-
            case zipExactMay boundVars args of
              Nothing ->
                Left (CaseWrongNumberVarsMatched conId boundVars)

              Just a ->
                Right a

          eval (TermEnv (HashMap.fromList xs) <> topEnv) newExp

    _ ->
      Left (CaseNotConstruct res)

evalOp :: TermEnv -> Operation -> Either Error Expr
evalOp topEnv op =
  case op of
    Compare e1 e2 -> do
      n1 <- evalInt topEnv e1
      n2 <- evalInt topEnv e2
      case compare n1 n2 of -- TODO: Just using compare here is a bad idea
        LT ->
          pure (Construct Builtin.lessThan mempty)

        EQ ->
          pure (Construct Builtin.equal mempty)

        GT ->
          pure (Construct Builtin.greaterThan mempty)

    Plus e1 e2 -> do
      n1 <- evalInt topEnv e1
      n2 <- evalInt topEnv e2
      pure (PrimInt (n1 + n2))

    Multiply e1 e2 -> do
      n1 <- evalInt topEnv e1
      n2 <- evalInt topEnv e2
      pure (PrimInt (n1 * n2))

    ShowInt expr -> do
      n <- evalInt topEnv expr
      pure (showIntBuiltin n)

    Panic expr -> do
      n <- eval topEnv expr
      Left (ErrorPanic (show n)) -- TODO: show is not right

-- | See 'desugarText' for a similar function.
showIntBuiltin :: Integer -> Expr
showIntBuiltin n =
  Construct Builtin.unicode [exprList (show n)]
  where
    exprList :: Text -> Expr
    exprList =
      Text.foldr consCodepoint (Construct Builtin.nil mempty)

    consCodepoint :: Char -> Expr -> Expr
    consCodepoint c expr =
      Construct
        Builtin.cons
        [ PrimInt (fromIntegral (charToCodepoint c))
        , expr
        ]

evalInt :: TermEnv -> Expr -> Either Error Integer
evalInt env expr = do
  res <- eval env expr
  case res of
    PrimInt n ->
      pure n

    _ ->
      Left (ExpectedAnInt res)

lookup :: Id -> TermEnv -> Either Error Expr
lookup id env =
  case HashMap.lookup id (unTermEnv env) of
    Just expr ->
      pure expr

    Nothing ->
      Left (NotFound id)

addToEnv :: Id -> Expr -> TermEnv -> TermEnv
addToEnv id expr env =
  TermEnv (HashMap.insert id expr (unTermEnv env))
