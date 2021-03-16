{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- See Note [Constraints]
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ide.Plugin.Properties
  ( PropertyType (..),
    ToHsType,
    MetaData (..),
    PropertyKey,
    SPropertyKey (..),
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

import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Either      (fromRight)
import           Data.Function    ((&))
import           Data.Kind        (Constraint)
import qualified Data.Map.Strict  as Map
import           Data.Proxy       (Proxy (..))
import qualified Data.Text        as T
import           GHC.TypeLits
import           Unsafe.Coerce    (unsafeCoerce)

-- ---------------------------------------------------------------------

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

data MetaData (t :: PropertyType) where
  MetaData ::
    (IsNotTEnum t) =>
    {defaultValue :: ToHsType t, description :: T.Text} ->
    MetaData t
  EnumMetaData ::
    (IsTEnum t) =>
    { defaultValue :: ToHsType t,
      description :: T.Text,
      enumValues :: [T.Text],
      enumDescriptions :: [T.Text]
    } ->
    MetaData t

data PropertyKey (s :: Symbol) (t :: PropertyType)

data SPropertyKey k where
  SNumber :: SPropertyKey (PropertyKey s 'TNumber)
  SString :: SPropertyKey (PropertyKey s 'TString)
  SBoolean :: SPropertyKey (PropertyKey s 'TBoolean)
  SObject :: SPropertyKey (PropertyKey s 'TObject)
  SArray :: SPropertyKey (PropertyKey s 'TArray)
  SEnum :: SPropertyKey (PropertyKey s 'TEnum)

data SomePropertyKeyWithMetaData
  = forall k s t.
    (k ~ PropertyKey s t) =>
    SomePropertyKeyWithMetaData (SPropertyKey k, MetaData t)

-- | Describes dedicated configuration of a plugin.
--
-- It was designed to be compatible with vscode's poor settings UI
newtype Properties (r :: [*]) = Properties (Map.Map String SomePropertyKeyWithMetaData)

-- ---------------------------------------------------------------------

type family IsTEnum (t :: PropertyType) :: Constraint where
  IsTEnum 'TEnum = ()
  IsTEnum x = TypeError ('Text "Expected ‘" ':<>: 'ShowType 'TEnum ':<>: 'Text "’, but got ‘" ':<>: 'ShowType x ':<>: 'Text "’")

type family IsNotTEnum (t :: PropertyType) :: Constraint where
  IsNotTEnum 'TEnum = TypeError ('Text "Unexpected " ':<>: 'ShowType 'TEnum)
  IsNotTEnum x = ()

type family FindKey (s :: Symbol) r where
  FindKey s (PropertyKey s t ': _) = t
  FindKey s (_ ': xs) = FindKey s xs

type family Elem (s :: Symbol) r :: Constraint where
  Elem s (PropertyKey s _ ': _) = ()
  Elem s (_ ': xs) = Elem s xs
  Elem s '[] = TypeError ('Text "The key ‘" ':<>: 'Text s ':<>: 'Text "’ is undefined")

type family NotElem (s :: Symbol) r :: Constraint where
  NotElem s (PropertyKey s _ ': _) = TypeError ('Text "The key ‘" ':<>: 'Text s ':<>: 'Text "’ is already defined")
  NotElem s (_ ': xs) = NotElem s xs
  NotElem s '[] = ()

type HasProperty s k t r = (k ~ PropertyKey s t, Elem s r, FindKey s r ~ t, KnownSymbol s)

-- ---------------------------------------------------------------------

emptyProperties :: Properties '[]
emptyProperties = Properties Map.empty

insert ::
  forall s k r t.
  (k ~ PropertyKey s t, NotElem s r, KnownSymbol s) =>
  SPropertyKey k ->
  MetaData t ->
  Properties r ->
  Properties (k ': r)
insert key metadata (Properties old) = Properties (Map.insert (symbolVal (Proxy @s)) (SomePropertyKeyWithMetaData (key, metadata)) old)

find ::
  forall s k r t.
  (HasProperty s k t r) =>
  Properties r ->
  (SPropertyKey k, MetaData t)
find (Properties p) = case p Map.! symbolVal (Proxy @s) of
  (SomePropertyKeyWithMetaData x) ->
    -- Note [Constraints]
    -- It's safe to use unsafeCoerce here:
    --   Since each property name is unique that the redefinition will be prevented by predication on the type level list,
    --   the value we get from the name-indexed map must be exactly the singleton and metadata corresponding to the type.
    -- We drop this information at type level: some of the above type families return '() :: Constraint',
    -- so GHC will consider them as redundant.
    -- But we encode it using semantically identical 'Map' at term level,
    -- which avoids inducting on the list by defining a new type class.
    unsafeCoerce x

-- ---------------------------------------------------------------------

-- | Given the name of a defined property, generates a JSON parser of 'plcConfig'
usePropertyEither ::
  forall s k t r.
  (HasProperty s k t r) =>
  Properties r ->
  A.Object ->
  Either String (ToHsType t)
usePropertyEither p = parseProperty @s (find p)

-- | Like 'usePropertyEither' but returns 'defaultValue' on parse error
useProperty ::
  forall s k t r.
  (HasProperty s k t r) =>
  Properties r ->
  Maybe A.Object ->
  ToHsType t
useProperty p = maybe (defaultValue metadata) (fromRight (defaultValue metadata) . usePropertyEither @s p)
  where
    (_, metadata) = find @s p

parseProperty ::
  forall s k t.
  (k ~ PropertyKey s t, KnownSymbol s) =>
  (SPropertyKey k, MetaData t) ->
  A.Object ->
  Either String (ToHsType t)
parseProperty km x = case km of
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
            else fail $ "unknown enum option: " <> T.unpack txt
      )
      x
  _ -> error "impossible"
  where
    keyName = T.pack $ symbolVal (Proxy @s)
    parseEither :: forall a. A.FromJSON a => Either String a
    parseEither = A.parseEither (A..: keyName) x

-- ---------------------------------------------------------------------

defineNumberProperty ::
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  T.Text ->
  Int ->
  Properties r ->
  Properties (PropertyKey s 'TNumber : r)
defineNumberProperty description defaultValue = insert SNumber MetaData {..}

defineStringProperty ::
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  T.Text ->
  T.Text ->
  Properties r ->
  Properties (PropertyKey s 'TString : r)
defineStringProperty description defaultValue = insert SString MetaData {..}

defineBooleanProperty ::
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  T.Text ->
  Bool ->
  Properties r ->
  Properties (PropertyKey s 'TBoolean : r)
defineBooleanProperty description defaultValue = insert SBoolean MetaData {..}

defineObjectProperty ::
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  T.Text ->
  A.Object ->
  Properties r ->
  Properties (PropertyKey s 'TObject : r)
defineObjectProperty description defaultValue = insert SObject MetaData {..}

defineArrayProperty ::
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  T.Text ->
  A.Array ->
  Properties r ->
  Properties (PropertyKey s 'TArray : r)
defineArrayProperty description defaultValue = insert SArray MetaData {..}

defineEnumProperty ::
  forall s r.
  (KnownSymbol s, NotElem s r) =>
  T.Text ->
  [(T.Text, T.Text)] ->
  T.Text ->
  Properties r ->
  Properties (PropertyKey s 'TEnum : r)
defineEnumProperty description enums defaultValue = insert SEnum $ EnumMetaData defaultValue description (fst <$> enums) (snd <$> enums)

-- ---------------------------------------------------------------------

toDefaultJSON :: Properties r -> [A.Pair]
toDefaultJSON (Properties p) =
  [toEntry k v | (k, v) <- Map.toList p]
  where
    toEntry :: String -> SomePropertyKeyWithMetaData -> A.Pair
    toEntry s (SomePropertyKeyWithMetaData k) = case k of
      (SNumber, MetaData {..}) ->
        T.pack s A..= defaultValue
      (SString, MetaData {..}) ->
        T.pack s A..= defaultValue
      (SBoolean, MetaData {..}) ->
        T.pack s A..= defaultValue
      (SObject, MetaData {..}) ->
        T.pack s A..= defaultValue
      (SArray, MetaData {..}) ->
        T.pack s A..= defaultValue
      (SEnum, EnumMetaData {..}) ->
        T.pack s A..= defaultValue
      _ -> error "impossible"

toVSCodeExtensionSchema :: T.Text -> Properties r -> [A.Pair]
toVSCodeExtensionSchema prefix (Properties p) =
  [(prefix <> T.pack k) A..= toEntry v | (k, v) <- Map.toList p]
  where
    toEntry :: SomePropertyKeyWithMetaData -> A.Value
    toEntry (SomePropertyKeyWithMetaData k) = case k of
      (SNumber, MetaData {..}) ->
        A.object
          [ "type" A..= A.String "number",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SString, MetaData {..}) ->
        A.object
          [ "type" A..= A.String "string",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SBoolean, MetaData {..}) ->
        A.object
          [ "type" A..= A.String "boolean",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SObject, MetaData {..}) ->
        A.object
          [ "type" A..= A.String "object",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SArray, MetaData {..}) ->
        A.object
          [ "type" A..= A.String "array",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SEnum, EnumMetaData {..}) ->
        A.object
          [ "type" A..= A.String "string",
            "description" A..= description,
            "enum" A..= enumValues,
            "enumDescriptions" A..= enumDescriptions,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      _ -> error "impossible"
