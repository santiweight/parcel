{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Parcel (tests) where

import Data.Proxy
import Data.Text qualified as T
import GHC.Generics
import Language.Python.Common.Pretty qualified as Pretty
import Parcel
import Parcel.Class
import Parcel.Python hiding (Newtype, SingleConstructorData)
import Parcel.Utils
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Prelude
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson (ToJSON, FromJSON)
import Parcel.Python.Gen
import Data.Maybe (fromMaybe)

tests :: TestTree
tests =
  testGroup
    "parcel"
    [ testGroup
        "encode"
        [        ],
        goldenVsStringDiff
          "MaybeInt"
          differ
          "test/golden/MaybeInt.py.expected"
          (pure $ BSL.pack $ prettyDecl @MaybeInt),
        goldenVsStringDiff
          "Something"
          differ
          "test/golden/Something.py.expected"
          (pure $ BSL.pack $ prettyDecl @Something)
    ]

prettyDecl :: forall a. Parcel a => String
prettyDecl = Pretty.prettyText $ pyMToModule $ fromMaybe (pure []) $ decl @a

differ :: FilePath -> FilePath -> [String]
differ ref new = ["diff", "-u", ref, new]

data MaybeInt = NothingInt | JustInt {myInt :: Int}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Parcel)

data EitherIntSomething = MyLeft {unLeft :: Int} | MyRight {unRight :: Something}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Parcel)

data Something = Something {field1 :: MaybeInt, field2 :: EitherIntSomething, field3 :: SimpleEnum}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Parcel)

data SimpleEnum = One | Two
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Parcel)
