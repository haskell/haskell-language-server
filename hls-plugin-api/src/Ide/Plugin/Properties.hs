{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


module Ide.Plugin.Properties
  ( PropertyType (..),
    ToHsType,
    NotElem,
    MetaData (..),
    PropertyKey (..),
    SPropertyKey (..),
    KeyNameProxy (..),
    KeyNamePath (..),
    Properties,
    HasProperty,
    HasPropertyByPath,
    emptyProperties,
    defineNumberProperty,
    defineIntegerProperty,
    defineStringProperty,
    defineBooleanProperty,
    defineObjectProperty,
    defineArrayProperty,
    defineEnumProperty,
    definePropertiesProperty,
    toDefaultJSON,
    toVSCodeExtensionSchema,
    usePropertyEither,
    useProperty,
    usePropertyByPathEither,
    usePropertyByPath,
    (&),
  )
where

import           Control.Arrow        (first)
import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import           Data.Either          (fromRight)
import           Data.Function        ((&))
import           Data.Kind            (Constraint, Type)
import           Data.Proxy           (Proxy (..))
import           Data.String          (IsString (fromString))
import qualified Data.Text            as T
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits


-- | Types properties may have
data PropertyType
  = TNumber
  | TInteger
  | TString
  | TBoolean
  | TObject Type
  | TArray Type
  | TEnum Type
  | TProperties [PropertyKey] -- ^ A typed TObject, defined in a recursive manner

type family ToHsType (t :: PropertyType) where
  ToHsType 'TNumber = Double -- in js, there are no distinct types for integers and floating-point values
  ToHsType 'TInteger = Int   -- so here we use Double for Number, Int for Integer
  ToHsType 'TString = T.Text
  ToHsType 'TBoolean = Bool
  ToHsType ('TObject a) = a
  ToHsType ('TArray a) = [a]
  ToHsType ('TEnum a) = a
  ToHsType ('TProperties _) = A.Object

-- ---------------------------------------------------------------------

-- | Metadata of a property
data MetaData (t :: PropertyType) where
  MetaData ::
    (IsTEnum t ~ 'False, IsProperties t ~ 'False) =>
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
  PropertiesMetaData ::
    (t ~ TProperties rs) =>
    {
      defaultValue :: ToHsType t
      , description :: T.Text
      , childrenProperties :: Properties rs
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
  SProperties :: SPropertyKey ('PropertyKey s ('TProperties pp))

-- | Existential wrapper of 'SPropertyKey', with an extra 'MetaData'
data SomePropertyKeyWithMetaData
  = forall k s t.
    (k ~ 'PropertyKey s t) =>
    SomePropertyKeyWithMetaData (SPropertyKey k) (MetaData t)

-- | 'Properties' is a partial implementation of json schema, without supporting union types and validation.
-- In hls, it defines a set of properties used in dedicated configuration of a plugin.
-- A property is an immediate child of the json object in each plugin's "config" section.
-- It was designed to be compatible with vscode's settings UI.
-- Use 'emptyProperties' and 'useProperty' to create and consume 'Properties'.
data Properties (r :: [PropertyKey]) where
    ConsProperties :: (k ~ 'PropertyKey s t, KnownSymbol s, NotElem s ks)
        => KeyNameProxy s -> (SPropertyKey k) -> (MetaData t) -> Properties ks -> Properties (k : ks)
    EmptyProperties :: Properties '[]

-- | A proxy type in order to allow overloaded labels as properties' names at the call site
data KeyNameProxy (s :: Symbol) = KnownSymbol s => KeyNameProxy

instance (KnownSymbol s', s ~ s') => IsLabel s (KeyNameProxy s') where
  fromLabel = KeyNameProxy

data NonEmptyList a =
    a :| NonEmptyList a | NE a

-- | a path to a property in a json object
data KeyNamePath (r :: NonEmptyList Symbol) where
   SingleKey :: KeyNameProxy s -> KeyNamePath (NE s)
   ConsKeysPath :: KeyNameProxy s1 -> KeyNamePath ss -> KeyNamePath (s1 :| ss)

class ParsePropertyPath (rs :: [PropertyKey]) (r :: NonEmptyList Symbol) where
    usePropertyByPathEither :: KeyNamePath r -> Properties rs -> A.Object -> Either String (ToHsType (FindByKeyPath r rs))
    useDefault :: KeyNamePath r -> Properties rs -> ToHsType (FindByKeyPath r rs)
    usePropertyByPath :: KeyNamePath r -> Properties rs -> A.Object -> ToHsType (FindByKeyPath r rs)
    usePropertyByPath p ps x = fromRight (useDefault p ps) $ usePropertyByPathEither p ps x

instance (HasProperty s k t r) => ParsePropertyPath r (NE s) where
    usePropertyByPathEither (SingleKey kn) sm x = parseProperty kn (find kn sm) x
    useDefault (SingleKey kn) sm = defaultValue metadata
        where (_, metadata) = find kn sm

instance ( ToHsType (FindByKeyPath ss r2) ~ ToHsType (FindByKeyPath (s :| ss) r)
          ,HasProperty s ('PropertyKey s ('TProperties r2)) t2 r
          , ParsePropertyPath r2 ss)
          => ParsePropertyPath r (s :| ss) where
    usePropertyByPathEither (ConsKeysPath kn p) sm x = do
        let (key, meta) = find kn sm
        interMedia <- parseProperty kn (key, meta) x
        case meta of
            PropertiesMetaData {..}
                -> usePropertyByPathEither p childrenProperties interMedia
    useDefault (ConsKeysPath kn p) sm = case find kn sm of
            (_, PropertiesMetaData {..}) -> useDefault p childrenProperties

-- ---------------------------------------------------------------------

type family IsProperties (t :: PropertyType) :: Bool where
  IsProperties ('TProperties pp) = 'True
  IsProperties _ = 'False

type family IsTEnum (t :: PropertyType) :: Bool where
  IsTEnum ('TEnum _) = 'True
  IsTEnum _ = 'False

type family FindByKeyPath (ne :: NonEmptyList Symbol) (r :: [PropertyKey]) :: PropertyType where
  FindByKeyPath (s :| xs) ('PropertyKey s ('TProperties rs) ': _) = FindByKeyPath xs rs
  FindByKeyPath (s :| xs) (_ ': ys) = FindByKeyPath (s :| xs) ys
  FindByKeyPath (NE s) ys = FindByKeyName s ys

type family FindByKeyName (s :: Symbol) (r :: [PropertyKey]) :: PropertyType where
  FindByKeyName s ('PropertyKey s t ': _) = t
  FindByKeyName s (_ ': xs) = FindByKeyName s xs

type family IsPropertySymbol (s :: Symbol) (r :: PropertyKey) :: Bool where
    IsPropertySymbol s ('PropertyKey s _) = 'True
    IsPropertySymbol s _ = 'False

type family Elem (s :: Symbol) (r :: [PropertyKey]) :: Constraint where
  Elem s ('PropertyKey s _ ': _) = ()
  Elem s (_ ': xs) = Elem s xs
  Elem s '[] = TypeError ('Text "The key ‘" ':<>: 'Text s ':<>: 'Text "’ is missing")

type family NotElem (s :: Symbol) (r :: [PropertyKey]) :: Constraint where
  NotElem s ('PropertyKey s _ ': _) = TypeError ('Text "The key ‘" ':<>: 'Text s ':<>: 'Text "’ is already defined")
  NotElem s (_ ': xs) = NotElem s xs
  NotElem s '[] = ()


-- | In row @r@, there is a 'PropertyKey' @k@, which has name @s@ and carries haskell type @t@
type HasProperty s k t r = (k ~ 'PropertyKey s t, Elem s r, FindByKeyPath (NE s) r ~ t, FindByKeyName s r ~ t, KnownSymbol s, FindPropertyMeta s r t)
-- similar to HasProperty, but the path is given as a type-level list of symbols
type HasPropertyByPath props path t = (t ~ FindByKeyPath path props, ParsePropertyPath props path)
class FindPropertyMeta (s :: Symbol) (r :: [PropertyKey]) t where
   findSomePropertyKeyWithMetaData :: KeyNameProxy s -> Properties r -> (SPropertyKey ('PropertyKey s t), MetaData t)
instance (FindPropertyMetaIf (IsPropertySymbol symbol k) symbol k ks t) => FindPropertyMeta symbol (k : ks) t where
  findSomePropertyKeyWithMetaData = findSomePropertyKeyWithMetaDataIf
class (bool ~ IsPropertySymbol symbol k) => FindPropertyMetaIf bool symbol k ks t where
  findSomePropertyKeyWithMetaDataIf :: KeyNameProxy symbol -> Properties (k : ks) -> (SPropertyKey ('PropertyKey symbol t), MetaData t)
instance (k ~ 'PropertyKey s t) => FindPropertyMetaIf 'True s k ks t where
  findSomePropertyKeyWithMetaDataIf _ (ConsProperties _ k m _) = (k, m)
instance ('False ~ IsPropertySymbol s k, FindPropertyMeta s ks t) => FindPropertyMetaIf 'False s k ks t where
  findSomePropertyKeyWithMetaDataIf s (ConsProperties _ _ _ ks) = findSomePropertyKeyWithMetaData s ks

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
emptyProperties = EmptyProperties

insert ::
  (k ~ 'PropertyKey s t, NotElem s r, KnownSymbol s) =>
  KeyNameProxy s ->
  SPropertyKey k ->
  MetaData t ->
  Properties r ->
  Properties (k ': r)
insert = ConsProperties

find ::
  (HasProperty s k t r) =>
  KeyNameProxy s ->
  Properties r ->
  (SPropertyKey k, MetaData t)
find = findSomePropertyKeyWithMetaData

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
  (SProperties, _) -> parseEither
  (SNumber, _) -> parseEither
  (SInteger, _) -> parseEither
  (SString, _) -> parseEither
  (SBoolean, _) -> parseEither
  (SObject _, _) -> parseEither
  (SArray _, _) -> parseEither
  (SEnum _, EnumMetaData {..}) ->
    A.parseEither
      ( \o -> do
          txt <- o A..: key
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
    key = fromString $ symbolVal kn
    parseEither :: forall a. A.FromJSON a => Either String a
    parseEither = A.parseEither (A..: key) x

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

definePropertiesProperty ::
  (KnownSymbol s, NotElem s r) =>
  KeyNameProxy s ->
  T.Text ->
  Properties childrenProps ->
  Properties r ->
  Properties ('PropertyKey s ('TProperties childrenProps) : r)
definePropertiesProperty kn description ps rs =
    insert kn SProperties (PropertiesMetaData mempty description ps) rs

-- ---------------------------------------------------------------------

-- | Converts a properties definition into kv pairs with default values from 'MetaData'
toDefaultJSON :: Properties r -> [A.Pair]
toDefaultJSON pr = case pr of
    EmptyProperties -> []
    ConsProperties keyNameProxy k m xs ->
        toEntry (symbolVal keyNameProxy) (SomePropertyKeyWithMetaData k m) : toDefaultJSON xs
  where
    toEntry :: String -> SomePropertyKeyWithMetaData -> A.Pair
    toEntry s = \case
      (SomePropertyKeyWithMetaData SNumber MetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData SInteger MetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData SString MetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData SBoolean MetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData (SObject _) MetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData (SArray _) MetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData (SEnum _) EnumMetaData {..}) ->
        fromString s A..= defaultValue
      (SomePropertyKeyWithMetaData SProperties  PropertiesMetaData {..}) ->
        fromString s A..= A.object (toDefaultJSON childrenProperties)

-- | Converts a properties definition into kv pairs as vscode schema
toVSCodeExtensionSchema :: T.Text -> Properties r -> [A.Pair]
toVSCodeExtensionSchema prefix p = [fromString (T.unpack prefix <> fromString k) A..= v | (k, v) <- toVSCodeExtensionSchema' p]
toVSCodeExtensionSchema' :: Properties r -> [(String, A.Value)]
toVSCodeExtensionSchema' ps = case ps of
    EmptyProperties -> []
    ConsProperties (keyNameProxy :: KeyNameProxy s) (k :: SPropertyKey k) (m :: MetaData t) xs ->
          [(symbolVal keyNameProxy <> maybe "" ((<>) ".") k1, v)
            | (k1, v) <- toEntry (SomePropertyKeyWithMetaData k m) ]
          ++ toVSCodeExtensionSchema' xs
  where
    wrapEmpty :: A.Value -> [(Maybe String, A.Value)]
    wrapEmpty v = [(Nothing, v)]
    toEntry :: SomePropertyKeyWithMetaData -> [(Maybe String, A.Value)]
    toEntry = \case
      (SomePropertyKeyWithMetaData SNumber MetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "number",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SInteger MetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "integer",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SString MetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "string",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SBoolean MetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "boolean",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData (SObject _) MetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "object",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData (SArray _) MetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "array",
            "markdownDescription" A..= description,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData (SEnum _) EnumMetaData {..}) ->
        wrapEmpty $ A.object
          [ "type" A..= A.String "string",
            "description" A..= description,
            "enum" A..= enumValues,
            "enumDescriptions" A..= enumDescriptions,
            "default" A..= defaultValue,
            "scope" A..= A.String "resource"
          ]
      (SomePropertyKeyWithMetaData SProperties PropertiesMetaData {..}) ->
        map (first Just) $ toVSCodeExtensionSchema' childrenProperties
