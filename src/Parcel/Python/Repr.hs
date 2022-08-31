module Parcel.Python.Repr where
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

data KnownTy
  = TInt
  | TBool
  | TStr
  | TDict
  | TList Ty
  | TOptional Ty
  | TDictArg Ty Ty
  | TMap Ty Ty
  | TDouble
  deriving stock (Generic, Show, Eq, Ord)

data Ty
  = TKnown KnownTy
    | TyRef Text
  deriving stock (Generic, Show, Eq, Ord)

data ParcelRepr
  = SumParcel SumRepr
  | RecordParcel RecordRepr
  | KnownParcel KnownTy
  deriving stock (Show, Generic, Eq, Ord)

data RecordRepr = RecordRepr {name :: Text, fields :: [(Text, Ty)]}
  deriving stock (Show, Generic, Eq, Ord)

data SumRepr = SumRepr {name :: Text, alts :: [SumCase]}
  deriving stock (Show, Generic, Eq, Ord)

data SumCase = SumCase {name :: Text, args :: [(Text, Ty)]}
  deriving stock (Generic, Show, Eq, Ord)

data EnumRepr = EnumRepr {tyName :: Text, alts :: [Text]}
  deriving stock (Generic, Show)

parcelToTy :: ParcelRepr -> Ty
parcelToTy = \case
  SumParcel SumRepr {..} -> TyRef name
  RecordParcel RecordRepr {..} -> TyRef name
  KnownParcel ty -> TKnown ty
