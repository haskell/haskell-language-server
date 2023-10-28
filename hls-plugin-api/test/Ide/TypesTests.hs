{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}
module Ide.TypesTests
    ( tests
    ) where
import           Control.Lens                  ((?~))
import           Data.Default                  (Default (def))
import           Data.Function                 ((&))
import           Data.List.NonEmpty            (NonEmpty ((:|)), nonEmpty)
import qualified Data.Text                     as Text
import           Ide.Types                     (Config (Config),
                                                PluginRequestMethod (combineResponses))
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message (Method (Method_TextDocumentDefinition),
                                                SMethod (..))
import           Language.LSP.Protocol.Types   (ClientCapabilities,
                                                Definition (Definition),
                                                DefinitionClientCapabilities (DefinitionClientCapabilities, _dynamicRegistration, _linkSupport),
                                                DefinitionLink (DefinitionLink),
                                                DefinitionParams (DefinitionParams, _partialResultToken, _position, _textDocument, _workDoneToken),
                                                Location (Location),
                                                LocationLink (LocationLink),
                                                Null (Null),
                                                Position (Position),
                                                Range (Range),
                                                TextDocumentClientCapabilities (TextDocumentClientCapabilities, _definition),
                                                TextDocumentIdentifier (TextDocumentIdentifier),
                                                Uri (Uri), filePathToUri,
                                                type (|?) (..))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (assertBool, testCase, (@=?))

tests :: TestTree
tests = testGroup "PluginTypes"
    [ combineResponsesTests ]

combineResponsesTests :: TestTree
combineResponsesTests = testGroup "combineResponses"
    [ combineResponsesTextDocumentDefinitionTests
    ]

combineResponsesTextDocumentDefinitionTests :: TestTree
combineResponsesTextDocumentDefinitionTests = testGroup "TextDocumentDefinition"
    [ testCase "merges all single location responses into one response with all locations and upgrades them into links (with link support)" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InL . Definition . InL . Location testFileUri $ range1) :|
                [ InL . Definition . InL . Location testFileUri $ range2
                , InL . Definition . InL . Location testFileUri $ range3
                ]

            result = combineResponses SMethod_TextDocumentDefinition def supportsLinkInDefinitionCaps definitionParams pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InL $
                [ DefinitionLink $ LocationLink Nothing testFileUri range1 range1
                , DefinitionLink $ LocationLink Nothing testFileUri range2 range2
                , DefinitionLink $ LocationLink Nothing testFileUri range3 range3
                ]
        expectedResult @=? result

    , testCase "merges all location link responses into one with all links (with link support)" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InR . InL $ [DefinitionLink $ LocationLink Nothing testFileUri range1 range1]) :|
                [ InR . InL $
                    [ DefinitionLink $ LocationLink Nothing testFileUri range2 range2
                    , DefinitionLink $ LocationLink Nothing testFileUri range3 range3
                    ]
                ]

            result = combineResponses SMethod_TextDocumentDefinition def supportsLinkInDefinitionCaps definitionParams pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InL $
                [ DefinitionLink $ LocationLink Nothing testFileUri range1 range1
                , DefinitionLink $ LocationLink Nothing testFileUri range2 range2
                , DefinitionLink $ LocationLink Nothing testFileUri range3 range3
                ]
        expectedResult @=? result

    , testCase "merges location responses with link responses into link responses (with link support)" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InL . Definition . InL . Location testFileUri $ range1) :|
                [ InR . InL $ [ DefinitionLink $ LocationLink Nothing testFileUri range2 range2 ]
                , InL . Definition . InR $ [Location testFileUri range3]
                ]

            result = combineResponses SMethod_TextDocumentDefinition def supportsLinkInDefinitionCaps definitionParams pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InL $
                [ DefinitionLink $ LocationLink Nothing testFileUri range1 range1
                , DefinitionLink $ LocationLink Nothing testFileUri range2 range2
                , DefinitionLink $ LocationLink Nothing testFileUri range3 range3
                ]
        expectedResult @=? result

    , testCase "ignores Null responses when other responses are available" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InL . Definition . InL . Location testFileUri $ range1) :|
                [ InR . InR $ Null
                , InL . Definition . InR $ [Location testFileUri range3]
                ]

            result = combineResponses SMethod_TextDocumentDefinition def supportsLinkInDefinitionCaps definitionParams pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InL $
                [ DefinitionLink $ LocationLink Nothing testFileUri range1 range1
                , DefinitionLink $ LocationLink Nothing testFileUri range3 range3
                ]
        expectedResult @=? result

    , testCase "returns Null when all responses are Null" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InR . InR $ Null) :|
                [ InR . InR $ Null
                , InR . InR $ Null
                ]

            result = combineResponses SMethod_TextDocumentDefinition def supportsLinkInDefinitionCaps definitionParams pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InR $ Null
        expectedResult @=? result
    ]

(range1, range2, range3) = (Range (Position 3 0) $ Position 3 5, Range (Position 5 7) $ Position 5 13, Range (Position 24 30) $ Position 24 40)

supportsLinkInDefinitionCaps :: ClientCapabilities
supportsLinkInDefinitionCaps = def & L.textDocument ?~ textDocumentCaps
    where
        textDocumentCaps :: TextDocumentClientCapabilities
        textDocumentCaps = def { _definition = Just DefinitionClientCapabilities { _linkSupport = Just True, _dynamicRegistration = Nothing }}

definitionParams :: DefinitionParams
definitionParams = DefinitionParams
    { _textDocument = TextDocumentIdentifier testFileUri
    , _position = Position 5 4
    , _workDoneToken = Nothing
    , _partialResultToken = Nothing
    }

testFileUri :: Uri
testFileUri = filePathToUri "file://tester/Test.hs"
