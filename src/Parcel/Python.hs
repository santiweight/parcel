{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Parcel.Python where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Functor
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Generics
import Language.Python.Common qualified as Pretty
import Language.Python.Common qualified as Py
import Numeric.Interval
import Parcel.Class
import Parcel.Python.Repr
import Parcel.Python.Utils
import Parcel.Utils qualified as Utils
import System.Directory.Extra
import System.FilePath
import Prelude hiding (return)
import Parcel.Python.Gen
import Data.Maybe (fromMaybe)

prettyModule :: Py.Module () -> Text
prettyModule = T.pack . Pretty.prettyText
-- -- >>> aesonEnum
-- -- "\"One\""
-- aesonEnum :: BSL.ByteString
-- aesonEnum = Aeson.encode One

-- >>> aesonNothing
-- "null"
aesonNothing :: BSL.ByteString
aesonNothing = Aeson.encode $ Nothing @Int

-- >>> aesonJust
-- "1"
aesonJust :: BSL.ByteString
aesonJust = Aeson.encode $ Just (1 :: Int)

-- >>> aesonLeft
-- "{\"Left\":1}"
aesonLeft :: BSL.ByteString
aesonLeft = Aeson.encode (Left 1 :: Either Int Int)

-- >>> aesonRight
-- "{\"Right\":1}"
aesonRight :: BSL.ByteString
aesonRight = Aeson.encode (Right 1 :: Either Int Int)

-- -- >>> aesonSomething
-- -- "{\"field1\":{\"myInt\":1,\"tag\":\"JustInt\"},\"field2\":{\"Right\":{\"field1\":{\"tag\":\"NothingInt\"},\"field2\":{\"Left\":2},\"field3\":\"One\"}},\"field3\":\"Two\"}"
-- aesonSomething :: BSL.ByteString
-- aesonSomething = Aeson.encode (Something (JustInt 1) (Right $ Something NothingInt (Left 2) One) Two)

-- >>> Pretty.prettyText $ mkLoad @SimpleData
-- "@classmethod\ndef load(cls):\n    currPath = Path(__file__)\n    dataDirPath = currPath.parent\n    jsonPath = dataDirPath / \"data/SimpleData.json\"\n    return cls(simpleData=obj[\"simpleData\"])"
