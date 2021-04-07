{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- See Note [Constraints]
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ide.Plugin.Properties
  ( PropertyType (..),
    ToHsType,
    MetaData (..),
    PropertyKey (..),
    SPropertyKey (..),
    KeyNameProxy (..),
    Properties,
    HasProperty,
    emptyProperties,
    defineNumberProperty,
    defineIntegerProperty,
    defineStringProperty,
    defineBooleanProperty,
    defineObjectProperty,
    defineArrayProperty,
    defineEnumProperty,
    toDefaultJSON,
    toVSCodeExtensionSchema,
    usePropertyEither,
    useProperty,
    (&),
  )
where

import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import           Data.Either          (fromRight)
import           Data.Function        ((&))
import           Data.Kind            (Constraint, Type)
import qualified Data.Map.Strict      as Map
import           Data.Proxy (Proxy (..))
import qualified Data.Text            as T
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits
import           Unsafe.Coerce        (unsafeCoerce)

-- | Types properties may have
data PropertyType
  = TNumber
  | TInteger
  | TString
  | TBoolean
  | TObject Type
  | TArray Type
  | TEnum Type

type family ToHsType (t :: PropertyType) where
  ToHsType 'TNumber = Double -- in js, there are no distinct types for integers and floating-point values
  ToHsType 'TInteger = Int   -- so here we use Double for Number, Int for Integer
  ToHsType 'TString = T.Text
  ToHsType 'TBoolean = Bool
  ToHsType ('TObject a) = a
  ToHsType ('TArray a) = [a]
  ToHsType ('TEnum a) = a

-- ---------------------------------------------------------------------

-- | Metadata of a property
data MetaData (t :: PropertyType) where
  MetaData ::
    (IsTEnum t ~ 'False) =>
    { defaultValue :: ToHsType t,
      description :: T.Text
    } ->
    MetaData t
  EnumMetaData ::
    (IsTEnum t ~ 'True) =>
    { defaultValue :: ToHsType t,
      description :: T.Text,
      enumValues :: [ToHsType t],
      enumDescriptions :: [T.Text]
    } ->
    MetaData t

-- | Used at type level for name-type mapping in 'Properties'
data PropertyKey = PropertyKey Symbol PropertyType

-- | Singleton type of 'PropertyKey'
data SPropertyKey (k :: PropertyKey) where
  SNumber :: SPropertyKey ('PropertyKey s 'TNumber)
  SInteger :: SPropertyKey ('PropertyKey s 'TInteger)
  SString :: SPropertyKey ('PropertyKey s 'TString)
  SBoolean :: SPropertyKey ('PropertyKey s 'TBoolean)
  SObject :: (A.ToJSON a, A.FromJSON a) => Proxy a -> SPropertyKey ('PropertyKey s ('TObject a))
  SArray :: (A.ToJSON a, A.FromJSON a) => Proxy a -> SPropertyKey ('PropertyKey s ('TArray a))
  SEnum :: (A.ToJSON a, A.FromJSON a, Eq a, Show a) => Proxy a -> SPropertyKey ('PropertyKey s ('TEnum a))

-- | Existential wrapper of 'SPropertyKey', with an extra 'MetaData'
data SomePropertyKeyWithMetaData
  = forall k s t.
    (k ~ 'PropertyKey s t) =>
    SomePropertyKeyWithMetaData (SPropertyKey k) (MetaData t)

-- | 'Properties' is a partial implementation of json schema, without supporting union types and validation.
-- In hls, it defines a set of properties which used in dedicated configuration of a plugin.
-- A property is an immediate child of the json object in each plugin's "config" section.
-- It was designed to be compatible with vscode's settings UI.
-- Use 'emptyProperties' and 'useProperty' to create and consume 'Properties'.
newtype Properties (r :: [PropertyKey]) = Properties (Map.Map String SomePropertyKeyWithMetaData)

-- | A proxy type in order to allow overloaded labels as properties' names at the call site
data KeyNameProxy (s :: Symbol) = KnownSymbol s => KeyNameProxy

instance (KnownSymbol s', s ~ s') => IsLabel s (KeyNameProxy s') where
  fromLabel = KeyNameProxy

-- ---------------------------------------------------------------------

type family IsTEnum (t :: PropertyType) :: Bool where
  IsTEnum ('TEnum _) = 'True
  IsTEnum _ = 'False

type family FindByKeyName (s :: Symbol) (r :: [PropertyKey]) :: PropertyType where
  FindByKeyName s ('PropertyKey s t ': _) = t
  FindByKeyName s (_ ': xs) = FindByKeyName s xs

type family Elem (s :: Symbol) (r :: [PropertyKey]) :: Constraint where
  Elem s ('PropertyKey s _ ': _) = ()
  Elem s (_ ': xs) = Elem s xs
  Elem s '[] = TypeError ('Text "The key ‘" ':<>: 'Text s ':<>: 'Text "’ is missing")

type family NotElem (s :: Symbol) (r :: [PropertyKey]) :: Constraint where
  NotElem s ('PropertyKey s _ ': _) = TypeError ('Text "The key ‘" ':<>: 'Text s ':<>: 'Text "’ is already defined")
  NotElem s (_ ': xs) = NotElem s xs
  NotElem s '[] = ()

-- | In row @r@, there is a 'PropertyKey' @k@, which has name @s@ and carries haskell type @t@
type HasProperty s k t r = (k ~ 'PropertyKey s t, Elem s r, FindByKeyName s r ~ t, KnownSymbol s)

-- ---------------------------------------------------------------------

-- | Creates a 'Properties' that defines no property
--
-- Useful to start a definitions chain, for example:
-- @
-- properties =
--  emptyProperties
--    & defineStringProperty
--      #exampleString
--      "Description of exampleString"
--      "Foo"
--    & defineNumberProperty
--      #exampleNumber
--      "Description of exampleNumber"
--      233
-- @
emptyProperties :: Properties '[]
emptyProperties = Properties Map.empty

insert ::
  (k ~ 'PropertyKey s t, NotElem s r, KnownSymbol s) =>
  KeyNameProxy s ->
  SPropertyKey k ->
  MetaData t ->
  Properties r ->
  Properties (k ': r)
insert kn key metadata (Properties old) =
  Properties
    ( Map.insert
        (symbolVal kn)
        (SomePropertyKeyWithMetaData key metadata)
        old
    )

find ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  Properties r ->
  (SPropertyKey k, MetaData t)
find kn (Properties p) = case p Map.! symbolVal kn of
  (SomePropertyKeyWithMetaData sing metadata) ->
    -- Note [Constraints]
    -- It's safe to use unsafeCoerce here:
    --   Since each property name is unique that the redefinition will be prevented by predication on the type level list,
    --   the value we get from the name-indexed map must be exactly the singleton and metadata corresponding to the type.
    -- We drop this information at type level: some of the above type families return '() :: Constraint',
    -- so GHC will consider them as redundant.
    -- But we encode it using semantically identical 'Map' at term level,
    -- which avoids inducting on the list by defining a new type class.
    unsafeCoerce (sing, metadata)

-- ---------------------------------------------------------------------

-- | Given the name of a defined property, generates a JSON parser of 'plcConfig'
usePropertyEither ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  Properties r ->
  A.Object ->
  Either String (ToHsType t)
usePropertyEither kn p = parseProperty kn (find kn p)

-- | Like 'usePropertyEither' but returns 'defaultValue' on parse error
useProperty ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  Properties r ->
  A.Object ->
  ToHsType t
useProperty kn p = fromRight (defaultValue metadata) . usePropertyEither kn p
  where
    (_, metadata) = find kn p

parseProperty ::
  (k ~ 'PropertyKey s t, KnownSymbol s) =>
  KeyNameProxy s ->
  (SPropertyKey k, MetaData t) ->
  A.Object ->
  Either String (ToHsType t)
parseProperty kn k x = case k of
  (SNumber, _) -> parseEither
  (SInteger, _) -> parseEither
  (SString, _) -> parseEither
  (SBoolean, _) -> parseEither
  (SObject _, _) -> parseEither
  (SArray _, _) -> parseEither
  (SEnum _, EnumMetaData {..}) ->
    A.parseEither
      ( \o -> do
          txt <- o A..: keyName
          if txt `elem` enumValues
            then pure txt
            else
              fail $
                "invalid enum member: "
                  <> show txt
                  <> ". Expected one of "
                  <> show enumValues
      )
      x
  where
    keyName = T.pack $ symbolVal kn
    parseEither :: forall a. A.FromJSON a => Either String a
    parseEither = A.parseEither (A..: keyName) x

-- ---------------------------------------------------------------------

-- | Defines a number property
defineNumberProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  Double ->
  Properties r ->
  Properties ('PropertyKey s 'TNumber : r)
defineNumberProperty kn description defaultValue =
  insert kn SNumber MetaData {..}

-- | Defines an integer property
defineIntegerProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  Int ->
  Properties r ->
  Properties ('PropertyKey s 'TInteger : r)
defineIntegerProperty kn description defaultValue =
  insert kn SInteger MetaData {..}

-- | Defines a string property
defineStringProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  T.Text ->
  Properties r ->
  Properties ('PropertyKey s 'TString : r)
defineStringProperty kn description defaultValue =
  insert kn SString MetaData {..}

-- | Defines a boolean property
defineBooleanProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  Bool ->
  Properties r ->
  Properties ('PropertyKey s 'TBoolean : r)
defineBooleanProperty kn description defaultValue =
  insert kn SBoolean MetaData {..}

-- | Defines an object property
defineObjectProperty ::
  (KnownSymbol s, NotElem s r, A.ToJSON a, A.FromJSON a) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  a ->
  Properties r ->
  Properties ('PropertyKey s ('TObject a) : r)
defineObjectProperty kn description defaultValue =
  insert kn (SObject Proxy) MetaData {..}

-- | Defines an array property
defineArrayProperty ::
  (KnownSymbol s, NotElem s r, A.ToJSON a, A.FromJSON a) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  [a] ->
  Properties r ->
  Properties ('PropertyKey s ('TArray a) : r)
defineArrayProperty kn description defaultValue =
  insert kn (SArray Proxy) MetaData {..}

-- | Defines an enum property
defineEnumProperty ::
  (KnownSymbol s, NotElem s r, A.ToJSON a, A.FromJSON a, Eq a, Show a) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | valid enum members with each of description
  [(a, T.Text)] ->
  a ->
  Properties r ->
  Properties ('PropertyKey s ('TEnum a) : r)
defineEnumProperty kn description enums defaultValue =
  insert kn (SEnum Proxy) $ EnumMetaData defaultValue description (fst <$> enums) (snd <$> enums)

-- ---------------------------------------------------------------------

-- | Converts a properties definition into kv pairs with default values from 'MetaData'
toDefaultJSON :: Properties r -> [A.Pair]
toDefaultJSON (Properties p) = [toEntry s v | (s, v) <- Map.toList p]
  where
    toEntry :: String -> SomePropertyKeyWithMetaData -> A.Pair
    toEntry (T.pack -> s) = \case
      (SomePropertyKeyWithMetaData SNumber MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SInteger MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SString MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SBoolean MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData (SObject _) MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData (SArray _) MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData (SEnum _) EnumMetaData {..}) ->
        s A..= defaultValue

-- | Converts a properties definition into kv pairs as vscode schema
toVSCodeExtensionSchema :: T.Text -> Properties r -> [A.Pair]
toVSCodeExtensionSchema prefix (Properties p) =
  [(prefix <> T.pack k) A..= toEntry v | (k, v) <- Map.toList p]
  where
    toEntry :: SomePropertyKeyWithMetaData -> A.Value
    toEntry = \case
      (SomePropertyKeyWithMetaData SNumber MetaData {..}) ->
        A.object
          [ "type" A..= A.String "number",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SInteger MetaData {..}) ->
        A.object
          [ "type" A..= A.String "integer",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SString MetaData {..}) ->
        A.object
          [ "type" A..= A.String "string",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SBoolean MetaData {..}) ->
        A.object
          [ "type" A..= A.String "boolean",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData (SObject _) MetaData {..}) ->
        A.object
          [ "type" A..= A.String "object",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData (SArray _) MetaData {..}) ->
        A.object
          [ "type" A..= A.String "array",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData (SEnum _) EnumMetaData {..}) ->
        A.object
          [ "type" A..= A.String "string",
            "description" A..= description,
            "enum" A..= enumValues,
            "enumDescriptions" A..= enumDescriptions,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
