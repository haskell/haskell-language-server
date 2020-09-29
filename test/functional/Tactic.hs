{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Tactic
  ( tests
  )
where

import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Language.Haskell.LSP.Test
import           Language.Haskell.LSP.Types     ( Position(..)
                                                , Range(..)
                                                , CAResult(..)
                                                , CodeAction(..)
                                                )
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.HUnit
import Data.Maybe (mapMaybe)
import Ide.Plugin.Tactic.Types (tacticTitle, TacticCommand (..))


------------------------------------------------------------------------------
-- | Get a range at the given line and column corresponding to having nothing
-- selected.
--
-- NB: These coordinates are in "file space", ie, 1-indexed.
pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 1)


------------------------------------------------------------------------------
-- | Get the title of a code action.
codeActionTitle :: CAResult -> Maybe Text
codeActionTitle CACommand{} = Nothing
codeActionTitle (CACodeAction(CodeAction title _ _ _ _)) = Just title


tests :: TestTree
tests = testGroup
  "tactic"
  [ mkTest
      "Produces intros code action"
      "T1.hs" 2 14
      [ (id, Intros, "")
      ]
  , mkTest
      "Produces destruct and homomorphism code actions"
      "T2.hs" 2 21
      [ (id, Destruct, "eab")
      , (id, Homomorphism, "eab")
      ]
  , mkTest
      "Won't suggest homomorphism on the wrong type"
      "T2.hs" 8 8
      [ (not, Homomorphism, "global")
      ]
  , mkTest
      "Won't suggest intros on the wrong type"
      "T2.hs" 8 8
      [ (not, Intros, "")
      ]
  ]


------------------------------------------------------------------------------
-- | Make a tactic unit test.
mkTest
    :: Foldable t
    => String  -- ^ The test name
    -> FilePath  -- ^ The file to load
    -> Int  -- ^ Cursor line
    -> Int  -- ^ Cursor columnn
    -> t ( Bool -> Bool   -- Use 'not' for actions that shouldnt be present
         , TacticCommand  -- An expected command ...
         , Text           -- ... for this variable
         ) -- ^ A collection of (un)expected code actions.
    -> TestTree
mkTest name fp line col ts =
  testCase name $ do
  runSession hieCommand fullCaps tacticPath $ do
    doc <- openDoc fp "haskell"
    actions <- getCodeActions doc $ pointRange line col
    let titles = mapMaybe codeActionTitle actions
    for_ ts $ \(f, tc, var) -> do
      let title = tacticTitle tc var
      liftIO $
        f (elem title titles)
          @? ("Expected a code action with title " <> T.unpack title)

tacticPath :: FilePath
tacticPath = "test/testdata/tactic"

