{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Quoted.Expr
  ( quotedExpr

  -- * For Test.Quoted.Program (which also uses these orphan instances)
  , liftDataWithText
  , trimLeadingNewline
  ) where

import Bowtie.Lib.Prelude
import Bowtie.Surface.AST
import Bowtie.Surface.Parse
import Data.Data
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)

import qualified Bowtie.Type.AST as TYP
import qualified Data.Text as Text

deriving stock instance Data Expr
deriving stock instance Data TYP.Type
deriving stock instance Data Alt
deriving stock instance Data Id

quotedExpr :: QuasiQuoter
quotedExpr =
  QuasiQuoter
    { quoteExp =
        liftDataWithText . dirtyParseExpr . Text.pack . trimLeadingNewline

    , quotePat = \_ -> panic "quotedExpr: quotePat not defined"
    , quoteType = \_ -> panic "quotedExpr: quoteType not defined"
    , quoteDec  = \_ -> panic "quotedExpr: quoteDec not defined"
    }

-- This and liftText from: https://stackoverflow.com/a/38182444
--
-- Required to prevent the error:
--
--    Can't find interface-file declaration for variable Data.Text.Internal.pack
--
liftDataWithText :: forall a. Data a => a -> Q Exp
liftDataWithText =
  dataToExpQ f
  where
    f :: forall b. Typeable b => b -> Maybe (Q Exp)
    f =
      fmap liftText . (cast :: b -> Maybe Text)

    liftText :: Text -> Q Exp
    liftText txt =
      AppE (VarE 'Text.pack) <$> lift (Text.unpack txt)

trimLeadingNewline :: [Char] -> [Char]
trimLeadingNewline cs =
  case cs of
    '\n':rest -> rest
    _ -> cs
