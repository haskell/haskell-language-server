module Development.IDE.Test.Diagnostic where

import           Control.Lens            ((^.))
import qualified Data.Text               as T
import           GHC.Stack               (HasCallStack)
import           Language.LSP.Types
import           Language.LSP.Types.Lens as Lsp

-- | (0-based line number, 0-based column number)
type Cursor = (Int, Int)

cursorPosition :: Cursor -> Position
cursorPosition (line,  col) = Position line col

type ErrorMsg = String

requireDiagnostic
    :: (Foldable f, Show (f Diagnostic), HasCallStack)
    => f Diagnostic
    -> (DiagnosticSeverity, Cursor, T.Text, Maybe DiagnosticTag)
    -> Maybe ErrorMsg
requireDiagnostic actuals expected@(severity, cursor, expectedMsg, expectedTag)
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

    hasTag :: Maybe DiagnosticTag -> Maybe (List DiagnosticTag) -> Bool
    hasTag Nothing  _                          = True
    hasTag (Just _) Nothing                    = False
    hasTag (Just actualTag) (Just (List tags)) = actualTag `elem` tags

standardizeQuotes :: T.Text -> T.Text
standardizeQuotes msg = let
        repl '‘' = '\''
        repl '’' = '\''
        repl '`' = '\''
        repl  c  = c
    in  T.map repl msg
