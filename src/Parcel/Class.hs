{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Parcel.Class where

import Control.Monad.Trans.Writer
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownSymbol, TypeError, symbolVal)
import Language.Python.Common qualified as Py
import Parcel.Python.Repr
import Parcel.Python.Gen
import Parcel.Python.Utils
import Prelude hiding (return)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Servant.API (JSON, Header, Headers)

class Parcel a where
  repr :: forall a. ParcelRepr
  -- default repr :: (Generic a, ToJSON a, GParcel (Rep a)) => ParcelRepr
  -- repr = gRepr @(Rep a)
  decl :: Maybe (PyM [Py.Statement ()])
  -- default decl :: (Generic a, ToJSON a, GParcel (Rep a)) => Maybe (PyM [Py.Statement ()])
  -- decl = mkDecl (gRepr @(Rep a))

ty :: forall a. Parcel a => Ty
ty = parcelToTy $ repr @a

instance {-# OVERLAPPABLE #-} (Generic a, ToJSON a, GParcel (Rep a)) => Parcel a where
  repr = gRepr @(Rep a)
  decl = mkDecl $ repr @a

instance Parcel Int where
  repr = KnownParcel TInt
  decl = Nothing

instance Parcel Bool where
  repr = KnownParcel TBool
  decl = Nothing

instance Parcel Double where
  repr = KnownParcel TDouble
  decl = Nothing

instance (Parcel k, Parcel v) => Parcel (Map k v) where
  repr = KnownParcel $ TMap (ty @k) (ty @v)
  decl = Nothing

instance Parcel Text where
  repr = KnownParcel TStr
  decl = Nothing

instance Parcel BSL.ByteString where
  repr = KnownParcel TStr
  decl = Nothing

instance Parcel BS.ByteString where
  repr = KnownParcel TStr
  decl = Nothing

instance Parcel JSON where
  repr = KnownParcel TStr
  decl = Nothing

instance Parcel a => Parcel [a] where
  repr = KnownParcel $ TList $ ty @a
  decl = Nothing

instance Parcel a => Parcel (Maybe a) where
  repr = KnownParcel $ TOptional $ ty @a
  decl = Nothing

instance Parcel ty => Parcel (Header a ty) where
  repr = KnownParcel $ TOptional $ ty @ty
  decl = Nothing

-- instance Parcel ty => Parcel (Headers '[Header a ty] c) where
--   repr = KnownParcel $ TOptional $ ty @ty
--   decl = Nothing

instance Parcel ty => Parcel (Headers a ty) where
  repr = KnownParcel $ TOptional $ ty @ty
  decl = Nothing

class GParcel a where
  gRepr :: forall a. ParcelRepr

instance (Datatype d, HasFields fs) => GParcel (D1 d (C1 c fs)) where
  gRepr = RecordParcel RecordRepr {name = T.pack $ datatypeName (undefined :: D1 d (C1 c fs) p), fields = gFields @(C1 c fs)}

instance (GSumParcel (D1 d (a :+: b)) allNullary, AllNullary (D1 d (a :+: b)) allNullary) => GParcel (D1 d (a :+: b)) where
  gRepr = SumParcel (sumRepr @(D1 d (a :+: b)) @allNullary)

class GSumParcel a allNullary where
  sumRepr :: forall a allNullary. SumRepr

instance (Datatype d, GetSumTyCases alts) => GSumParcel (D1 d alts) False where
  sumRepr = SumRepr (T.pack $ datatypeName (undefined :: D1 d alts p)) (sumTyCases @alts)

instance (Datatype d, GetSumTyCases alts) => GSumParcel (D1 d alts) True where
  sumRepr = SumRepr (T.pack $ datatypeName (undefined :: D1 d alts p)) (sumTyCases @alts)

class GRecParcel a where
  gFields :: forall a. [(Text, Ty)]

instance (HasFields fs) => GRecParcel (C1 c fs) where
  gFields = getFields (Proxy @fs)

class HasFields a where
  getFields :: Proxy a -> [(Text, Ty)]

instance (HasFields f) => HasFields (C1 m f) where
  getFields _ = getFields (Proxy @f)

instance (KnownSymbol selName, Parcel fieldTy) => HasFields (S1 (MetaSel (Just selName) a b c) (K1 R fieldTy)) where
  getFields _ = [(T.pack $ symbolVal (Proxy @selName), ty @fieldTy)]

instance (IsField a, HasFields b) => HasFields (a :*: b) where
  getFields _ = getFieldEntry @a : getFields (Proxy :: Proxy b)

getFieldEntry :: forall a. IsField a => (Text, Ty)
getFieldEntry = (fieldName @a, fieldTy @a)

class IsField a where
  fieldName :: Text
  fieldTy :: Ty

instance {-# OVERLAPPABLE #-} TypeError a => IsField a

instance (Parcel fieldTy, Selector (MetaSel m a b c)) => IsField (S1 (MetaSel m a b c) (K1 R fieldTy)) where
  fieldName = T.pack $ selName (undefined :: S1 (MetaSel m a b c) (K1 R fieldTy) p)
  fieldTy = ty @fieldTy

class ToRecord a where
  className :: forall a. Text
  fieldNames :: forall a. [(Text, Ty)]

instance
  (HasFields (f p), KnownSymbol tyName) =>
  ToRecord (D1 ('MetaData tyName modNam packName isNewtype) f p)
  where
  className = T.pack $ symbolVal (Proxy @tyName)
  fieldNames = getFields (Proxy @(f p))

-- instance (KnownSymbol selName, Parcel fieldTy) => ToEnum (S1 (MetaSel (Just selName) a b c) (K1 R fieldTy) p) where
--   getFields _ = [(T.pack $ symbolVal (Proxy @selName), ty @fieldTy)]

-- instance (GetEnumName a, GetEnumName b) => HasFields (a :+: b) where
--   getFields _ = getFieldEntry @a : getFieldEntry @b : []

data MyEnum = Enum1 | Enum2
  deriving stock (Generic)

class ToEnum a where
  enumRepr :: EnumRepr

instance (Datatype d, GetEnumNames alts) => ToEnum (D1 d alts) where
  enumRepr = EnumRepr (T.pack $ datatypeName (undefined :: D1 d alts p)) (enumNames @alts)

class GetEnumNames a where
  enumNames :: [Text]

instance (Constructor c, GetEnumNames cs) => GetEnumNames (C1 c U1 :+: cs) where
  enumNames = T.pack (conName (undefined :: C1 c U1 p)) : enumNames @cs

instance (Constructor c) => GetEnumNames (C1 c U1) where
  enumNames = [T.pack (conName (undefined :: C1 c U1 p))]

-- >>> enumRepr @(Rep MyEnum)
-- EnumRepr {tyName = "MyEnum", alts = ["Enum1","Enum2"]}

class GetSumTyCases a where
  sumTyCases :: [SumCase]

class IsSumTyCase a where
  sumTyCase :: SumCase

instance (IsSumTyCase c, GetSumTyCases cs) => GetSumTyCases (c :+: cs) where
  sumTyCases = sumTyCase @c : sumTyCases @cs

instance (IsSumTyCase (C1 c k)) => GetSumTyCases (C1 c k) where
  sumTyCases = [sumTyCase @(C1 c k)]

instance (Constructor c) => IsSumTyCase (C1 c U1) where
  sumTyCase = SumCase (T.pack (conName (undefined :: C1 c U1 p))) []

instance (Constructor c, Selector s, Parcel fieldTy) => IsSumTyCase (C1 c (S1 s (Rec0 fieldTy))) where
  sumTyCase =
    SumCase
      (T.pack (conName (undefined :: C1 c (S1 m (Rec0 fieldTy)) p)))
      [(T.pack fieldName, ty @fieldTy)]
    where
      fieldName = case selName (undefined :: S1 s f p) of
        "" -> error "requires field name"
        name -> name

-- deriving (ToSumTy) via (Rep MaybeInt a)

-- >>> repr @(Rep MaybeInt)
-- SumParcel (SumRepr {name = "MaybeInt", alts = [SumCase {name = "NothingInt", args = []},SumCase {name = "JustInt", args = [("myInt",TInt)]}]})
