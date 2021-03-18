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
import           Data.Kind            (Constraint)
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits
import           Unsafe.Coerce        (unsafeCoerce)

-- | Types properties may have
data PropertyType
  = TNumber
  | TString
  | TBoolean
  | TObject
  | TArray
  | TEnum

type family ToHsType (t :: PropertyType) where
  ToHsType 'TNumber = Int
  ToHsType 'TString = T.Text
  ToHsType 'TBoolean = Bool
  ToHsType 'TObject = A.Object
  ToHsType 'TArray = A.Array
  ToHsType 'TEnum = T.Text -- supports only text enum now

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
  SString :: SPropertyKey ('PropertyKey s 'TString)
  SBoolean :: SPropertyKey ('PropertyKey s 'TBoolean)
  SObject :: SPropertyKey ('PropertyKey s 'TObject)
  SArray :: SPropertyKey ('PropertyKey s 'TArray)
  SEnum :: SPropertyKey ('PropertyKey s 'TEnum)

-- | Existential wrapper of 'SPropertyKey', with an extra 'MetaData'
data SomePropertyKeyWithMetaData
  = forall k s t.
    (k ~ 'PropertyKey s t) =>
    SomePropertyKeyWithMetaData (SPropertyKey k) (MetaData t)

-- | 'Properties' defines a set of properties which used in dedicated configuration of a plugin.
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
  IsTEnum 'TEnum = 'True
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
usePropertyEither k p = parseProperty k (find k p)

-- | Like 'usePropertyEither' but returns 'defaultValue' on parse error
useProperty ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  Properties r ->
  Maybe A.Object ->
  ToHsType t
useProperty k p =
  maybe
    (defaultValue metadata)
    (fromRight (defaultValue metadata) . usePropertyEither k p)
  where
    (_, metadata) = find k p

parseProperty ::
  (k ~ 'PropertyKey s t, KnownSymbol s) =>
  KeyNameProxy s ->
  (SPropertyKey k, MetaData t) ->
  A.Object ->
  Either String (ToHsType t)
parseProperty k km x = case km of
  (SNumber, _) -> parseEither
  (SString, _) -> parseEither
  (SBoolean, _) -> parseEither
  (SObject, _) -> parseEither
  (SArray, _) -> parseEither
  (SEnum, EnumMetaData {..}) ->
    A.parseEither
      ( \o -> do
          txt <- o A..: keyName
          if txt `elem` enumValues
            then pure txt
            else
              fail $
                "invalid enum member: "
                  <> T.unpack txt
                  <> ". Expected one of "
                  <> show enumValues
      )
      x
  where
    keyName = T.pack $ symbolVal k
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
  Int ->
  Properties r ->
  Properties ('PropertyKey s 'TNumber : r)
defineNumberProperty kn description defaultValue =
  insert kn SNumber MetaData {..}

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
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  A.Object ->
  Properties r ->
  Properties ('PropertyKey s 'TObject : r)
defineObjectProperty kn description defaultValue =
  insert kn SObject MetaData {..}

-- | Defines an array property
defineArrayProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | default value
  A.Array ->
  Properties r ->
  Properties ('PropertyKey s 'TArray : r)
defineArrayProperty kn description defaultValue =
  insert kn SArray MetaData {..}

-- | Defines an enum property
defineEnumProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  -- | description
  T.Text ->
  -- | valid enum members with each of description
  [(T.Text, T.Text)] ->
  T.Text ->
  Properties r ->
  Properties ('PropertyKey s 'TEnum : r)
defineEnumProperty kn description enums defaultValue =
  insert kn SEnum $ EnumMetaData defaultValue description (fst <$> enums) (snd <$> enums)

-- ---------------------------------------------------------------------

-- | Converts a properties definition into kv pairs with default values from 'MetaData'
toDefaultJSON :: Properties r -> [A.Pair]
toDefaultJSON (Properties p) = [toEntry s v | (s, v) <- Map.toList p]
  where
    toEntry :: String -> SomePropertyKeyWithMetaData -> A.Pair
    toEntry (T.pack -> s) = \case
      (SomePropertyKeyWithMetaData SNumber MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SString MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SBoolean MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SObject MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SArray MetaData {..}) ->
        s A..= defaultValue
      (SomePropertyKeyWithMetaData SEnum EnumMetaData {..}) ->
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
      (SomePropertyKeyWithMetaData SObject MetaData {..}) ->
        A.object
          [ "type" A..= A.String "object",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SArray MetaData {..}) ->
        A.object
          [ "type" A..= A.String "array",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SEnum EnumMetaData {..}) ->
        A.object
          [ "type" A..= A.String "string",
            "description" A..= description,
            "enum" A..= enumValues,
            "enumDescriptions" A..= enumDescriptions,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
