{-# LANGUAGE CPP #-}

module Development.IDE.Plugin.Plugins.Diagnostic (
  matchVariableNotInScope,
  matchRegexUnifySpaces,
  unifySpaces,
  matchFoundHole,
  matchFoundHoleIncludeUnderscore,
  diagReportHoleError
  )
  where

import           Control.Lens
import           Data.Bifunctor                    (Bifunctor (..))
import qualified Data.Text                         as T
import           Development.IDE                   (printOutputable)
import           Development.IDE.GHC.Compat        (RdrName)
import           Development.IDE.GHC.Compat.Error  (Hole, _ReportHoleError,
                                                    _TcRnMessage,
                                                    _TcRnNotInScope,
                                                    _TcRnSolverReport, hole_occ,
                                                    hole_ty, msgEnvelopeErrorL,
                                                    reportContentL)
import           Development.IDE.Types.Diagnostics (FileDiagnostic,
                                                    _SomeStructuredMessage,
                                                    fdStructuredMessageL)
import           GHC.Tc.Errors.Types               (NotInScopeError)
import           Text.Regex.TDFA                   ((=~~))

unifySpaces :: T.Text -> T.Text
unifySpaces    = T.unwords . T.words

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case message =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing                                                -> Nothing

-- | 'matchRegex' combined with 'unifySpaces'
--
-- >>> matchRegexUnifySpaces  "hello I'm a cow" "he(ll)o"
-- Just ["ll"]
matchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexUnifySpaces message = matchRegex (unifySpaces message)

matchFoundHole :: FileDiagnostic -> Maybe (T.Text, T.Text)
matchFoundHole fd = do
    hole <- diagReportHoleError fd
    Just (printOutputable (hole_occ hole), printOutputable (hole_ty hole))

matchFoundHoleIncludeUnderscore :: FileDiagnostic -> Maybe (T.Text, T.Text)
matchFoundHoleIncludeUnderscore fd = first ("_" <>) <$> matchFoundHole fd

matchVariableNotInScope :: FileDiagnostic -> Maybe (T.Text, Maybe T.Text)
matchVariableNotInScope fd = do
    (rdrName, _) <- diagReportNotInScope fd
    Just (printOutputable rdrName, Nothing)

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

-- | Extract the 'NotInScopeError' and the corresponding 'RdrName' from a 'FileDiagnostic'
-- if it represents a not-in-scope error.
diagReportNotInScope :: FileDiagnostic -> Maybe (RdrName, NotInScopeError)
diagReportNotInScope diag = do
#if MIN_VERSION_ghc(9,13,0)
    (err, rdrName) <-
        diag
            ^? fdStructuredMessageL
                . _SomeStructuredMessage
                . msgEnvelopeErrorL
                . _TcRnMessage
                . _TcRnNotInScope
#else
    (err, rdrName, _, _) <-
        diag
            ^? fdStructuredMessageL
                . _SomeStructuredMessage
                . msgEnvelopeErrorL
                . _TcRnMessage
                . _TcRnNotInScope
#endif
    Just (rdrName, err)
