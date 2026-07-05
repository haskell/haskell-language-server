module Hover (
  assertFoundIn,
  assertNotFoundIn,
  checkHover,
  checkHoverM,
) where

import           Config
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Maybe                 (mapMaybe)
import qualified Data.Text                  as T
import           Development.IDE.Test
import qualified Language.LSP.Protocol.Lens as L
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
checkHover hover expectations = checkHoverM hover (pure expectations)

checkHoverM :: (HasCallStack) => Maybe Hover -> Session [Expect] -> Session ()
checkHoverM hover expectations =
  traverse_ check =<< expectations
  where
    check :: (HasCallStack) => Expect -> Session ()
    check expected =
      case hover of
        Nothing -> unless (expected == ExpectNoHover) $ liftIO $ assertFailure "no hover found"
        Just Hover{_contents = (InL MarkupContent{_value = standardizeQuotes -> msg})
                  ,_range    = rangeInHover } ->
          case expected of
            ExpectRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverRange expectedRange -> checkHoverRange expectedRange rangeInHover msg
            ExpectHoverText snippets -> liftIO $ traverse_ (`assertFoundIn` msg) snippets
            ExpectHoverExcludeText snippets -> liftIO $ traverse_ (`assertNotFoundIn` msg) snippets
            ExpectHoverTextRegex re -> liftIO $ assertBool ("Regex not found in " <> T.unpack msg) (msg =~ re :: Bool)
            ExpectNoHover -> liftIO $ assertFailure $ "Expected no hover but got " <> show hover
            _ -> pure () -- all other expectations not relevant to hover
        _ -> liftIO $ assertFailure $ "test not expecting this kind of hover info" <> show hover

    checkHoverRange :: Range -> Maybe Range -> T.Text -> Session ()
    checkHoverRange expectedRange rangeInHover msg =
      let
        lineCol = extractLineColFromHoverMsg msg
        -- looks like hovers use 1-based numbering while definitions use 0-based
        -- turns out that they are stored 1-based in RealSrcLoc by GHC itself.
        adjust Position{_line = l, _character = c} =
          Position{_line = l + 1, _character = c + 1}
      in
      case map (read . T.unpack) lineCol of
        [l,c] -> liftIO $ adjust (expectedRange ^. L.start) @=? Position l c
        _     -> liftIO $ assertFailure $
          "expected: " <> show ("[...]<FILE_NAME>:<LINE>:<COL>**[...]", Just expectedRange) <>
          "\n but got: " <> show (msg, rangeInHover)

-- | Extract the source position from a message such as
--
-- @
--   "*Defined at C://file-name.hs:22:3*"
-- @
--
-- >>> extractLineColFromHoverMsg "*Defined at C://tmp/GotoHover.hs:22:3*"
-- ["22","3"]
--
-- >>> extractLineColFromHoverMsg "*Defined at /tmp/GotoHover.hs:22:3*"
-- ["22","3"]
extractLineColFromHoverMsg :: T.Text -> [T.Text]
extractLineColFromHoverMsg =
  -- Windows: "*Defined at C://tmp/GotoHover.hs:22:3*"
  -- Linux:   "*Defined at /tmp/GotoHover.hs:22:3*"
  T.lines
  -- Windows: ["*Defined at C://tmp/GotoHover.hs:22:3*"]
  -- Linux:   ["*Defined at /tmp/GotoHover.hs:22:3*"]
  >>> mapMaybe (T.stripPrefix "*Defined at ")
  -- Windows: ["C://tmp/GotoHover.hs:22:3*"]
  -- Linux:   ["/tmp/GotoHover.hs:22:3*"]
  >>> last
  -- Windows: "C://tmp/GotoHover.hs:22:3*"
  -- Linux:   "/tmp/GotoHover.hs:22:3*"
  >>> T.dropEnd 1
  -- Windows: "C://tmp/GotoHover.hs:22:3"
  -- Linux:   "/tmp/GotoHover.hs:22:3"
  >>> T.splitOn ":"
  -- Windows: ["C", "//tmp/GotoHover.hs", "22", "3"]
  -- Linux:   ["/tmp/GotoHover.hs", "22", "3"]
  >>> reverse
  -- Windows: ["3", "22", "//tmp/GotoHover.hs", "C"]
  -- Linux:   ["3", "22", "/tmp/GotoHover.hs"]
  >>> take 2
  -- Windows: ["3", "22"]
  -- Linux:   ["3", "22"]
  >>> reverse
  -- Windows: ["22", "3"]
  -- Linux:   ["22", "3"]
