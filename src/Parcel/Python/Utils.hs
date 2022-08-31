{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parcel.Python.Utils where

import Data.Text qualified as T
import Language.Python.Common qualified as Py
import Data.Text (Text)
import Language.Python.Common
import Prelude
import Data.Kind (Type)
import GHC.Generics

v :: Text -> Py.Expr ()
v name = Py.Var (ident name) ()

arg :: Py.Expr () -> Py.Argument ()
arg e = Py.ArgExpr e ()

return :: Py.Expr () -> Py.Statement ()
return e = Py.Return (Just e) ()

infix 2 =:

infix 3 @@

infix 4 @.

(=:) :: Py.Expr () -> Py.Expr () -> Py.Statement ()
name =: expr = Py.Assign [name] expr ()

(@@) :: Py.Expr () -> [Py.Argument ()] -> Py.Expr ()
fun @@ args = Py.Call fun args ()

(@.) :: Py.Expr () -> Py.Ident () -> Py.Expr ()
obj @. field = Py.Dot obj field ()

string :: Text -> Py.Expr ()
string txt = Py.Strings [show txt] ()

unquotedString :: Text -> Py.Expr ()
unquotedString txt = Py.Strings [T.unpack txt] ()

ident :: Text -> Py.Ident ()
ident name = Py.Ident {ident_string = T.unpack name, ident_annot = ()}

param :: Text -> Py.Parameter ()
param name =
  Py.Param
    { param_name = ident name,
      param_py_annotation = Nothing,
      param_default = Nothing,
      param_annot = ()
    }

dataclass :: Py.Statement () -> Py.Statement ()
dataclass cls = Py.Decorated [Py.Decorator [ident "dataclass"] [] ()] cls ()

data True

data False

class And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True True True

instance And False False False

instance And False True False

instance And True False False


class AllNullary (f :: Type -> Type) allNullary | f -> allNullary

instance
  ( AllNullary a allNullaryL,
    AllNullary b allNullaryR,
    And allNullaryL allNullaryR allNullary
  ) =>
  AllNullary (a :+: b) allNullary

instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary

instance AllNullary (a :*: b) False

instance AllNullary (a :.: b) False

instance AllNullary (K1 i c) False

instance AllNullary Par1 False

instance AllNullary (Rec1 f) False

instance AllNullary U1 True

