module Hover where

import           Config
import           Control.Monad
import           Data.Foldable
import qualified Data.Text            as T
import           Development.IDE.Test
import           Test.Hls
import           Text.Regex.TDFA

assertFoundIn :: T.Text -> T.Text -> Assertion
assertFoundIn part whole =
    assertBool
        (T.unpack $ "failed to find: `" <> part <> "` in hover message:\n" <> whole)
        (part `T.isInfixOf` whole)

assertNotFoundIn :: T.Text -> T.Text -> Assertion
assertNotFoundIn part whole =
    assertBool
        (T.unpack $ "found unexpected: `" <> part <> "` in hover message:\n" <> whole)
        (not . T.isInfixOf part $ whole)

checkHover :: (HasCallStack) => Maybe Hover -> [Expect] -> Session ()
checkHover hover expectations = traverse_ check expectations
  where
    check :: (HasCallStack) => Expect -> Session ()
    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (InL MarkupContent{_value = standardizeQuotes -> msg})
                  ,_range    = _rangeInHover } ->
          case expected of
            ExpectRange  _expectedRange -> liftIO $ assertFailure $ "ExpectRange assertion not implemented, yet."
            ExpectHoverRange _expectedRange -> liftIO $ assertFailure $ "ExpectHoverRange assertion not implemented, yet."
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectHoverExcludeText snippets -> liftIO $ traverse_ (`assertNotFoundIn` msg) snippets
            ExpectHoverTextRegex re -> liftIO $ assertBool ("Regex not found in " <> T.unpack msg) (msg =~ re :: Bool)
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover
