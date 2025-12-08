{-# LANGUAGE CPP #-}
module Development.IDE.Plugin.Plugins.FillTypeWildcard
  ( suggestFillTypeWildcard
  ) where

import           Control.Lens
import           Data.Maybe                        (isJust)
import qualified Data.Text                         as T
import           Development.IDE                   (FileDiagnostic (..),
                                                    fdStructuredMessageL,
                                                    printOutputable)
import           Development.IDE.GHC.Compat        hiding (vcat)
import           Development.IDE.GHC.Compat.Error
import           Development.IDE.Types.Diagnostics (_SomeStructuredMessage)
import           GHC.Tc.Errors.Types               (ErrInfo (..))
import           Language.LSP.Protocol.Types       (Diagnostic (..),
                                                    TextEdit (TextEdit))
#if MIN_VERSION_ghc(9,13,0)
import           GHC.Tc.Errors.Ppr                 (pprErrCtxtMsg)
import           GHC.Utils.Outputable              (vcat)
#endif

suggestFillTypeWildcard :: FileDiagnostic -> [(T.Text, TextEdit)]
suggestFillTypeWildcard diag@FileDiagnostic{fdLspDiagnostic = Diagnostic {..}}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'
    | isWildcardDiagnostic diag
    , typeSignature <- extractWildCardTypeSignature diag =
        [("Use type signature: ‘" <> typeSignature <> "’", TextEdit _range typeSignature)]
    | otherwise = []

isWildcardDiagnostic :: FileDiagnostic -> Bool
isWildcardDiagnostic =
    maybe False (isJust . (^? _TypeHole) . hole_sort) . diagReportHoleError

-- | Extract the 'Hole' out of a 'FileDiagnostic'
diagReportHoleError :: FileDiagnostic -> Maybe Hole
diagReportHoleError diag = do
    solverReport <-
        diag
            ^? fdStructuredMessageL
                . _SomeStructuredMessage
                . msgEnvelopeErrorL
                . _TcRnMessage
                . _TcRnSolverReport
                . _1
    (hole, _) <- solverReport ^? reportContentL . _ReportHoleError

    Just hole

-- | Extract the type and surround it in parentheses except in obviously safe cases.
--
-- Inferring when parentheses are actually needed around the type signature would
-- require understanding both the precedence of the context of the hole and of
-- the signature itself. Inserting them (almost) unconditionally is ugly but safe.
extractWildCardTypeSignature :: FileDiagnostic -> T.Text
extractWildCardTypeSignature diag =
    case hole_ty <$> diagReportHoleError diag of
        Just ty
            | isTopLevel || not (isApp ty) || enclosed ty -> printOutputable ty
            | otherwise -> "(" <> printOutputable ty <> ")"
        Nothing -> error "GHC provided invalid type"
    where
        isTopLevel :: Bool
        isTopLevel =
            maybe False errorMessageRefersToToplevelHole (diagErrInfoContext diag)

        isApp :: Type -> Bool
        isApp (AppTy _ _)          = True
        isApp (TyConApp _ (_ : _)) = True
        isApp (FunTy{})            = True
        isApp _                    = False

        enclosed :: Type -> Bool
        enclosed (TyConApp con _)
            | con == listTyCon || isTupleTyCon con = True
        enclosed _ = False

-- | Extract the 'ErrInfo' context out of a 'FileDiagnostic' and render it to
-- 'Text'
diagErrInfoContext :: FileDiagnostic -> Maybe T.Text
diagErrInfoContext diag = do
    (_, detailedMsg) <-
        diag
            ^? fdStructuredMessageL
                . _SomeStructuredMessage
                . msgEnvelopeErrorL
                . _TcRnMessageWithCtx
                . _TcRnMessageWithInfo
    let TcRnMessageDetailed err _ = detailedMsg
#if MIN_VERSION_ghc(9,13,0)
        ErrInfo errInfoCtx _ _ = err
    Just (printOutputable (vcat $ map pprErrCtxtMsg errInfoCtx))
#else
        ErrInfo errInfoCtx _ = err
    Just (printOutputable errInfoCtx)
#endif

-- | Detect whether user wrote something like @foo :: _@ or @foo :: Maybe _@.
-- The former is considered toplevel case for which the function returns 'True',
-- the latter is not toplevel and the returned value is 'False'.
--
-- When type hole is at toplevel then the ErrInfo context starts with
-- "In the type signature" which ends with " :: _" like in the
-- following snippet:
--
--     Just "In the type signature: decl :: _"
--
-- When type hole is not at toplevel there’s a stack of where
-- the hole was located ending with "In the type signature":
--
--     Just "In the first argument of ‘HsDecl’\nIn the type signature: decl :: HsDecl _"
errorMessageRefersToToplevelHole :: T.Text -> Bool
errorMessageRefersToToplevelHole msg =
    "In the type signature:" `T.isPrefixOf` msg
        && " :: _" `T.isSuffixOf` T.takeWhile (/= '\n') msg
