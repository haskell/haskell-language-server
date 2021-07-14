{-# LANGUAGE OverloadedStrings #-}
module Command (tests) where

import           Control.Lens            hiding (List)
import           Data.Char
import qualified Data.Text               as T
import           Language.LSP.Types      as LSP
import           Language.LSP.Types.Lens as LSP
import           Test.Hls
import           Test.Hls.Command
import           Test.Hls.Flags          (requiresEvalPlugin)

tests :: TestTree
tests = testGroup "commands" [
    testCase "are prefixed" $
        runSession hlsCommand fullCaps "test/testdata/" $ do
            ResponseMessage _ _ (Right res) <- initializeResponse
            let List cmds = res ^. LSP.capabilities . executeCommandProvider . _Just . commands
                f x = (T.length (T.takeWhile isNumber x) >= 1) && (T.count ":" x >= 2)
            liftIO $ do
                all f cmds @? "All prefixed"
                not (null cmds) @? "Commands aren't empty"
    , requiresEvalPlugin $ testCase "get de-prefixed" $
        runSession hlsCommand fullCaps "test/testdata/" $ do
            ResponseMessage _ _ (Left err) <- request
                SWorkspaceExecuteCommand
                (ExecuteCommandParams Nothing "34133:eval:evalCommand" (Just (List [])))
            let ResponseError _ msg _ = err
            -- We expect an error message about the dud arguments, but we can
            -- check that we found the right plugin.
            liftIO $ "while parsing args for evalCommand in plugin eval" `T.isInfixOf` msg @? "Has error message"
    ]
