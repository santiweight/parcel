module Parcel.Utils where

import Data.Aeson qualified as Asn
import Data.Aeson.Encoding qualified as Asn
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Prelude

encode :: (Generic a, Asn.GToJSON' Asn.Encoding Asn.Zero (Rep a)) => a -> Text
encode a = TL.toStrict $ TL.decodeUtf8 $ Asn.encodingToLazyByteString $ Asn.genericToEncoding Asn.defaultOptions {Asn.tagSingleConstructors = True} a

decode :: FromJSON a => BL.ByteString -> Maybe a
decode = Asn.decode
