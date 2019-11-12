module Bowtie.Untyped.Eval where

import Bowtie.Lib.FreeVars
import Bowtie.Lib.Prelude
import Bowtie.Untyped.Expr
import Safe.Exact (zipExactMay)

import qualified Bowtie.Lib.Builtin as Builtin
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text as Text

data Error
  = AppNonLambda
  | NotFound Id
  | CaseWrongNumberVarsMatched Id [Id]
  | ErrorPanic Text
  deriving (Eq, Show, Generic, NFData)

eval :: TermEnv -> Expr -> Either Error Expr
eval topEnv topExpr =
  case topExpr of
    Var id ->
      lookup id topEnv

    Lam env id expr ->
      evalLam topEnv env id expr

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

evalLam :: TermEnv -> TermEnv -> Id -> Expr -> Either Error Expr
evalLam topEnv env id expr = do
  let
    free :: HashMap Id ()
    free =
      HashMap.fromList (fmap (\a -> (a, ())) (Set.toList (freeVars expr)))

    newEnv :: TermEnv
    newEnv =
      TermEnv
        (HashMap.intersectionWith
          const
          (unTermEnv (env <> topEnv))
          free)

  pure (Lam newEnv id expr)

evalApp :: TermEnv -> Expr -> Expr -> Either Error Expr
evalApp topEnv e1 e2 = do
  res <- eval topEnv e1
  case res of
    Lam env id lamExp -> do
      res2 <- eval topEnv e2
      eval (addToEnv env id res2 topEnv) lamExp

    Construct tag exps -> do
      res2 <- eval topEnv e2
      pure (Construct tag (exps <> [res2])) -- PERFORMANCE

    _ ->
      Left AppNonLambda

evalLet :: TermEnv -> HashMap Id Expr -> Expr -> Either Error Expr
evalLet topEnv decls e = do
  evaledDecls <- traverse (eval topEnv) decls

  let
    f :: Id -> Expr -> TermEnv -> TermEnv
    f i e' env =
      TermEnv (HashMap.insert i e' (unTermEnv env))

    newEnv :: TermEnv
    newEnv = HashMap.foldrWithKey f topEnv evaledDecls

  eval newEnv e

evalCase :: TermEnv -> Expr -> HashMap Id Match -> Either Error Expr
evalCase topEnv expr alternatives = do
  res <- eval topEnv expr
  case res of
    -- args are 0 Nil in case Cons 0 Nil
    Construct conId args ->
      case HashMap.lookup conId alternatives of
        Nothing ->
          panic
            (  "Case statement fell through. Constructor being cased apart: " <> show conId
            <> " alternatives tried: " <> Text.unlines (fmap (\a -> "==============" <> show a) (HashMap.toList alternatives))
            )

        Just (Match boundVars newExp) -> do
          xs :: [(Id, Expr)] <-
            case zipExactMay boundVars args of
              Nothing ->
                Left (CaseWrongNumberVarsMatched conId boundVars)

              Just a ->
                Right a

          eval (TermEnv (HashMap.fromList xs) <> topEnv) newExp

    _ ->
      panic ("Case not Construct: " <> show res)

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
      panic "not an int"

lookup :: Id -> TermEnv -> Either Error Expr
lookup id env =
  case HashMap.lookup id (unTermEnv env) of
    Just expr ->
      pure expr

    Nothing ->
      Left (NotFound id)

addToEnv :: TermEnv -> Id -> Expr -> TermEnv -> TermEnv
addToEnv bound id expr env =
  TermEnv (HashMap.insert id expr (unTermEnv (bound <> env)))
