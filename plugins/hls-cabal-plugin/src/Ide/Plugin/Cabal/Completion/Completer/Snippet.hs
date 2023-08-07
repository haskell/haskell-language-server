{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Completion.Completer.Snippet where

import           Control.Lens                                 ((?~))
import           Control.Monad.Extra                          (mapMaybeM)
import           Data.Function                                ((&))
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import qualified Data.Text                                    as T
import           Ide.Logger                                   (Priority (..),
                                                               logWith)
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types
import           Ide.Plugin.Cabal.Completion.Types
import qualified Language.LSP.Protocol.Lens                   as JL
import qualified Language.LSP.Protocol.Types                  as LSP
import qualified Text.Fuzzy.Parallel                          as Fuzzy

-- | Maps snippet triggerwords with their completers
snippetCompleter :: Completer
snippetCompleter recorder cData = do
  let scored = Fuzzy.simpleFilter Fuzzy.defChunkSize Fuzzy.defMaxResults (completionPrefix prefInfo) $ Map.keys snippets
  mapMaybeM
    ( \compl -> do
        let matched = Fuzzy.original compl
        let completion' = Map.lookup matched snippets
        case completion' of
          Nothing -> do
            logWith recorder Warning $ LogMapLookUpOfKnownKeyFailed matched
            pure Nothing
          Just completion ->
            pure $ Just $ mkSnippetCompletion completion matched
    )
    scored
  where
    snippets = snippetMap prefInfo
    prefInfo = cabalPrefixInfo cData
    mkSnippetCompletion :: T.Text -> T.Text -> LSP.CompletionItem
    mkSnippetCompletion insertText toDisplay =
      mkDefaultCompletionItem toDisplay
        & JL.kind ?~ LSP.CompletionItemKind_Snippet
        & JL.insertText ?~ insertText
        & JL.insertTextFormat ?~ LSP.InsertTextFormat_Snippet

type TriggerWord = T.Text

snippetMap :: CabalPrefixInfo -> Map TriggerWord T.Text
snippetMap prefInfo =
  fmap T.unlines $
    Map.fromList
      [ ( "library-snippet",
          [ "library",
            "  hs-source-dirs: $1",
            "  exposed-modules: $2",
            "  build-depends: base",
            "  default-language: Haskell2010"
          ]
        ),
        ( "recommended-fields",
          [ "cabal-version: $1",
            "name: " <> completionFileName prefInfo,
            "version: 0.1.0.0",
            "maintainer: $4",
            "category: $5",
            "synopsis: $6",
            "license: $7",
            "build-type: Simple"
          ]
        ),
        ( "executable-snippet",
          [ "executable $1",
            "  main-is: ${2:Main.hs}",
            "  build-depends: base"
          ]
        ),
        ( "benchmark-snippet",
          [ "benchmark $1",
            "  type: exitcode-stdio-1.0",
            "  main-is: ${3:Main.hs}",
            "  build-depends: base"
          ]
        ),
        ( "testsuite-snippet",
          [ "test-suite $1",
            "  type: exitcode-stdio-1.0",
            "  main-is: ${3:Main.hs}",
            "  build-depends: base"
          ]
        ),
        ( "common-warnings",
          [ "common warnings",
            "  ghc-options: -Wall"
          ]
        ),
        ( "source-repo-github-snippet",
          [ "source-repository head",
            "  type: git",
            "  location: git://github.com/$2"
          ]
        ),
        ( "source-repo-git-snippet",
          [ "source-repository head",
            "  type: git",
            "  location: $1"
          ]
        )
      ]
