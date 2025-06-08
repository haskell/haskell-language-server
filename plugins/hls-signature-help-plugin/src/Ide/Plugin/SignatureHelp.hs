{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Ide.Plugin.SignatureHelp (descriptor) where

import           Control.Monad.Trans              (lift)
import qualified Data.List.NonEmpty               as NL
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils (runIdeActionE,
                                                   useWithStaleFastE)
import           Development.IDE.Spans.AtPoint    (getNamesAtPoint)
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Text.Regex.TDFA                  ((=~))

data Log = LogDummy

instance Pretty Log where
    pretty = \case
        LogDummy -> "TODO(@linj) remove this dummy log"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _recorder pluginId =
    (defaultPluginDescriptor pluginId "Provides signature help of something callable")
        { pluginHandlers = mkPluginHandler SMethod_TextDocumentSignatureHelp signatureHelpProvider
        }

-- get src info
--   function
--   which arg is under the cursor
-- get function type (and arg doc)
-- assemble result
-- TODO(@linj)
signatureHelpProvider :: PluginMethodHandler IdeState Method_TextDocumentSignatureHelp
signatureHelpProvider ideState _pluginId (SignatureHelpParams (TextDocumentIdentifier uri) position _mProgreeToken _mContext) = do
    nfp <- getNormalizedFilePathE uri
    names <- runIdeActionE "signatureHelp" (shakeExtras ideState) $ do
        (HAR {hieAst}, positionMapping) <- useWithStaleFastE GetHieAst nfp
        let ns = getNamesAtPoint hieAst position positionMapping
        pure ns
    mRangeAndDoc <-
        runIdeActionE
            "signatureHelp.getDoc"
            (shakeExtras ideState)
            (lift (getAtPoint nfp position))
    let (_mRange, contents) = case mRangeAndDoc of
            Just (mRange, contents) -> (mRange, contents)
            Nothing                 -> (Nothing, [])

    pure $
        InL $
            SignatureHelp
                ( case mkSignatureHelpLabel names contents of
                    Just label ->
                        [ SignatureInformation
                              label
                              Nothing
                              (Just [ParameterInformation (InR (5, 8)) Nothing])
                              Nothing
                        ]
                    Nothing -> []
                )
                (Just 0)
                (Just $ InL 0)
    where
        mkSignatureHelpLabel names types =
            case (chooseName $ printName <$> names, chooseType types >>= showType) of
                (Just name, Just typ) -> Just $ T.pack name <> " :: " <> typ
                _                     -> Nothing
        chooseName names = case names of
            []            -> Nothing
            name : names' -> Just $ NL.last (name NL.:| names')
        chooseType types = case types of
            []  -> Nothing
            [t] -> Just t
            _   -> Just $ types !! (length types - 2)
        showType typ = getMatchedType $ typ =~ ("\n```haskell\n(.*) :: (.*)\n```\n" :: T.Text)
        getMatchedType :: (T.Text, T.Text, T.Text, [T.Text]) -> Maybe T.Text
        getMatchedType (_, _, _, [_, t]) = Just t
        getMatchedType _                 = Nothing
