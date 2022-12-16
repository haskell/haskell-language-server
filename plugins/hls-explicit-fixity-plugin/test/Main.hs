{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                 as T
import           Ide.Plugin.ExplicitFixity (Log, descriptor)
import           System.FilePath
import           Test.Hls

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "explicit-fixity"

main :: IO ()
main = defaultTestRunner tests

tests :: TestTree
tests = testGroup "Explicit fixity"
    [ hoverTest "(++)" (Position 5 7) "infixr 5 `++`"
    , hoverTest "($)" (Position 6 7) "infixr 0 `$`"
    , hoverTest "(.)" (Position 7 7) "infixr 9 `.`"
    , hoverTest "(+)" (Position 8 7) "infixl 6 `+`"
    , hoverTest "(-)" (Position 9 8) "infixl 6 `-`"
    , hoverTest "(<>)" (Position 10 7) "infixr 6 `<>`"
    , hoverTest "(>>=)" (Position 11 7) "infixl 1 `>>=`"
    , hoverTest "(>=>)" (Position 12 7) "infixr 1 `>=>`"
    , hoverTest "elem" (Position 13 7) "infix 4 `elem`"
    , hoverTest "on" (Position 14 7) "infixl 0 `on`"
    , hoverTest "(||)" (Position 15 8) "infixr 2 `||`"
    , hoverTest "mod" (Position 16 8) "infixl 7 `mod`"
    , hoverTest "(**)" (Position 17 8) "infixr 8 `**`"
    , hoverTest "(^)" (Position 18 8) "infixr 8 `^`"
    , hoverTest "(<$)" (Position 19 8) "infixl 4 `<$`"
    , hoverTest "seq" (Position 20 9) "infixr 0 `seq`"
    , hoverTest "(<|>)" (Position 21 8) "infixl 3 `<|>`"
    , hoverTest "fixity define" (Position 23 11) "infixr 7 `>>:`"
    , hoverTest "record" (Position 28 10) "infix 9 `>>::`"
    , hoverTest "wildcards" (Position 30 5) "infixr 7 `>>:`  \n  \ninfix 9 `>>::`"
    , hoverTest "function" (Position 32 11) "infixl 1 `f`"
    , hoverTest "signature" (Position 35 2) "infixr 9 `>>>:`"
    , hoverTest "operator" (Position 36 2) "infixr 9 `>>>:`"
    , hoverTest "escape" (Position 39 2) "infixl 3 `~\\:`"
    -- Ensure that there is no one extra new line in import statement
    , expectFail $ hoverTest "import" (Position 2 18) "Control.Monad***"
    -- Known issue, See https://github.com/haskell/haskell-language-server/pull/2973/files#r916535742
    , expectFail $ hoverTestImport "import" (Position 4 7) "infixr 9 `>>>:`"
    ]

hoverTest :: TestName -> Position -> T.Text -> TestTree
hoverTest = hoverTest' "Hover.hs"
hoverTestImport :: TestName -> Position -> T.Text -> TestTree
hoverTestImport = hoverTest' "HoverImport.hs"

hoverTest' :: String -> TestName -> Position -> T.Text -> TestTree
hoverTest' docName title pos expected = testCase title $ runSessionWithServer plugin testDataDir $ do
    doc <- openDoc docName "haskell"
    waitForKickDone
    h <- getHover doc pos
    let expected' = "\n" <> sectionSeparator <> expected
    case h of
        Nothing -> liftIO $ assertFailure "No hover"
        Just (Hover contents _) -> case contents of
          HoverContentsMS _ -> liftIO $ assertFailure "Unexpected content type"
          HoverContents (MarkupContent mk txt) -> do
              liftIO
                $ assertBool ("Failed to find `" <> T.unpack expected <> "` in hover message: " <> T.unpack txt)
                $ expected `T.isInfixOf` txt
    closeDoc doc

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
