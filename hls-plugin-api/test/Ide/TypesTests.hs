{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.TypesTests
    ( tests
    ) where
import           Control.Lens                  ((?~), (^?))
import           Data.Default                  (Default (def))
import           Data.Function                 ((&))
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import           Data.Maybe                    (isJust)
import qualified Data.Text                     as Text
import           Ide.Types                     (PluginRequestMethod (combineResponses))
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message (MessageParams, MessageResult,
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
                                                TextDocumentClientCapabilities,
                                                TextDocumentIdentifier (TextDocumentIdentifier),
                                                TypeDefinitionClientCapabilities (TypeDefinitionClientCapabilities, _dynamicRegistration, _linkSupport),
                                                TypeDefinitionParams (..),
                                                Uri (Uri), _L, _R, _definition,
                                                _typeDefinition, filePathToUri,
                                                type (|?) (..))
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (testCase, (@=?))
import           Test.Tasty.QuickCheck         (ASCIIString (ASCIIString),
                                                Arbitrary (arbitrary), Gen,
                                                arbitraryBoundedEnum, cover,
                                                listOf1, oneof, testProperty,
                                                (===))

tests :: TestTree
tests = testGroup "PluginTypes"
    [ combineResponsesTests ]

combineResponsesTests :: TestTree
combineResponsesTests = testGroup "combineResponses"
    [ combineResponsesTextDocumentDefinitionTests
    , combineResponsesTextDocumentTypeDefinitionTests
    ]

combineResponsesTextDocumentDefinitionTests :: TestTree
combineResponsesTextDocumentDefinitionTests = testGroup "TextDocumentDefinition" $
    defAndTypeDefSharedTests SMethod_TextDocumentDefinition definitionParams

combineResponsesTextDocumentTypeDefinitionTests :: TestTree
combineResponsesTextDocumentTypeDefinitionTests = testGroup "TextDocumentTypeDefinition" $
    defAndTypeDefSharedTests SMethod_TextDocumentTypeDefinition typeDefinitionParams

defAndTypeDefSharedTests ::
    ( MessageResult m  ~ (Definition |? ([DefinitionLink] |? Null))
    , PluginRequestMethod m
    )
    => SMethod m -> MessageParams m -> [TestTree]
defAndTypeDefSharedTests message params =
    [ testCase "merges all single location responses into one response with all locations (without upgrading to links)" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InL . Definition . InL . Location testFileUri $ range1) :|
                [ InL . Definition . InL . Location testFileUri $ range2
                , InL . Definition . InL . Location testFileUri $ range3
                ]

            result = combineResponses message def supportsLinkInAllDefinitionCaps params pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InL . Definition . InR $
                [ Location testFileUri range1
                , Location testFileUri range2
                , Location testFileUri range3
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

            result = combineResponses message def supportsLinkInAllDefinitionCaps params pluginResponses

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

            result = combineResponses message def supportsLinkInAllDefinitionCaps params pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InL $
                [ DefinitionLink $ LocationLink Nothing testFileUri range1 range1
                , DefinitionLink $ LocationLink Nothing testFileUri range2 range2
                , DefinitionLink $ LocationLink Nothing testFileUri range3 range3
                ]
        expectedResult @=? result

    , testCase "preserves link-specific data when merging link and location responses (with link support)" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InL . Definition . InL . Location testFileUri $ range1) :|
                [ InR . InL $ [ DefinitionLink $ LocationLink (Just range1) testFileUri range2 range3 ] ]

            result = combineResponses message def supportsLinkInAllDefinitionCaps params pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InL $
                [ DefinitionLink $ LocationLink Nothing testFileUri range1 range1
                , DefinitionLink $ LocationLink (Just range1) testFileUri range2 range3
                ]
        expectedResult @=? result

    , testCase "ignores Null responses when other responses are available" $ do
        let pluginResponses :: NonEmpty (Definition |? ([DefinitionLink] |? Null))
            pluginResponses =
                (InL . Definition . InL . Location testFileUri $ range1) :|
                [ InR . InR $ Null
                , InR . InL $ [DefinitionLink $ LocationLink Nothing testFileUri range3 range3]
                ]

            result = combineResponses message def supportsLinkInAllDefinitionCaps params pluginResponses

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

            result = combineResponses message def supportsLinkInAllDefinitionCaps params pluginResponses

            expectedResult :: Definition |? ([DefinitionLink] |? Null)
            expectedResult = InR . InR $ Null
        expectedResult @=? result

    , testProperty "downgrades all locationLinks to locations when missing link support in capabilities" $ \(MkGeneratedNonEmpty responses) -> do
        let pluginResponses = fmap (\(MkGeneratedDefinition definition) -> definition) responses

            result = combineResponses message def def params pluginResponses

        cover 70 (any (isJust . (>>= (^? _L)) . (^? _R)) pluginResponses) "Has at least one response with links" $
            cover 10 (any (isJust . (^? _L)) pluginResponses) "Has at least one response with locations" $
            cover 10 (any (isJust . (>>= (^? _R)) . (^? _R)) pluginResponses) "Has at least one response with Null" $
            (isJust (result ^? _L) || isJust (result ^? _R >>= (^? _R))) === True
    ]


range1, range2, range3 :: Range
range1 = Range (Position 3 0) $ Position 3 5
range2 = Range (Position 5 7) $ Position 5 13
range3 = Range (Position 24 30) $ Position 24 40

supportsLinkInAllDefinitionCaps :: ClientCapabilities
supportsLinkInAllDefinitionCaps = def & L.textDocument ?~ textDocumentCaps
    where
        textDocumentCaps :: TextDocumentClientCapabilities
        textDocumentCaps = def
            { _definition = Just DefinitionClientCapabilities { _linkSupport = Just True, _dynamicRegistration = Nothing }
            , _typeDefinition = Just TypeDefinitionClientCapabilities { _linkSupport = Just True, _dynamicRegistration = Nothing }
            }

definitionParams :: DefinitionParams
definitionParams = DefinitionParams
    { _textDocument = TextDocumentIdentifier testFileUri
    , _position = Position 5 4
    , _workDoneToken = Nothing
    , _partialResultToken = Nothing
    }

typeDefinitionParams :: TypeDefinitionParams
typeDefinitionParams = TypeDefinitionParams
    { _textDocument = TextDocumentIdentifier testFileUri
    , _position = Position 5 4
    , _workDoneToken = Nothing
    , _partialResultToken = Nothing
    }

testFileUri :: Uri
testFileUri = filePathToUri "file://tester/Test.hs"

newtype GeneratedDefinition = MkGeneratedDefinition (Definition |? ([DefinitionLink] |? Null)) deriving newtype (Show)

instance Arbitrary GeneratedDefinition where
    arbitrary = MkGeneratedDefinition <$> oneof
        [ InL . Definition . InL <$> generateLocation
        , InL . Definition . InR <$> listOf1 generateLocation
        , InR . InL . map DefinitionLink <$> listOf1 generateLocationLink
        , pure . InR . InR $ Null
        ]
        where
            generateLocation :: Gen Location
            generateLocation = do
                (LocationLink _ uri range _) <- generateLocationLink
                pure $ Location uri range

            generateLocationLink :: Gen LocationLink
            generateLocationLink = LocationLink <$> generateMaybe generateRange <*> generateUri <*> generateRange <*> generateRange

            generateMaybe :: Gen a -> Gen (Maybe a)
            generateMaybe gen = oneof [Just <$> gen, pure Nothing]

            generateUri :: Gen Uri
            generateUri = do
                (ASCIIString str) <- arbitrary
                pure . Uri . Text.pack $ str

            generateRange :: Gen Range
            generateRange = Range <$> generatePosition <*> generatePosition

            generatePosition :: Gen Position
            generatePosition = Position <$> arbitraryBoundedEnum <*> arbitraryBoundedEnum

newtype GeneratedNonEmpty a = MkGeneratedNonEmpty (NonEmpty a) deriving newtype (Show)

instance Arbitrary a => Arbitrary (GeneratedNonEmpty a) where
    arbitrary = MkGeneratedNonEmpty <$> ((:|) <$> arbitrary <*> arbitrary)
