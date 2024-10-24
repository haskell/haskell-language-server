{-# LANGUAGE CPP #-}
module Development.IDE.Test.Diagnostic where

import           Control.Lens                ((^.))
import qualified Data.Text                   as T
import           GHC.Stack                   (HasCallStack)
import           Language.LSP.Protocol.Lens
import           Language.LSP.Protocol.Types
import           Development.IDE.GHC.Compat (ghcVersion, GhcVersion (..))

-- | (0-based line number, 0-based column number)
type Cursor = (UInt, UInt)

cursorPosition :: Cursor -> Position
cursorPosition (line,  col) = Position line col

type ErrorMsg = String

requireDiagnostic
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> (DiagnosticSeverity, Cursor, T.Text, Maybe T.Text, Maybe DiagnosticTag)
    -> Maybe ErrorMsg
requireDiagnostic actuals expected@(severity, cursor, expectedMsg, mbExpectedCode, expectedTag)
    | any match actuals = Nothing
    | otherwise = Just $
            "Could not find " <> show expected <>
            " in " <> show actuals
  where
    match :: Diagnostic -> Bool
    match d =
        Just severity == _severity d
        && cursorPosition cursor == d ^. range . start
        && standardizeQuotes (T.toLower expectedMsg) `T.isInfixOf`
           standardizeQuotes (T.toLower $ d ^. message)
        && hasTag expectedTag (d ^. tags)
        && codeMatches d

    codeMatches d
      | ghcVersion >= GHC96 =
        case (mbExpectedCode, _code d) of
          (Nothing, _)                         -> True
          (Just expectedCode, Nothing)         -> False
          (Just expectedCode, Just actualCode) -> InR expectedCode == actualCode
      | otherwise =  True

    hasTag :: Maybe DiagnosticTag -> Maybe [DiagnosticTag] -> Bool
    hasTag Nothing  _                   = True
    hasTag (Just _) Nothing             = False
    hasTag (Just actualTag) (Just tags) = actualTag `elem` tags

standardizeQuotes :: T.Text -> T.Text
standardizeQuotes msg = let
        repl '‘' = '\''
        repl '’' = '\''
        repl '`' = '\''
        repl  c  = c
    in  T.map repl msg
