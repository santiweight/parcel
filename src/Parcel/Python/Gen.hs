{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Parcel.Python.Gen where

import Control.Applicative (liftA2)
import Control.Monad.Writer hiding (return)
import Data.Bifunctor (second)
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Tuple (swap)
import Language.Python.Common qualified as Py
import Parcel.Python.Repr
import Parcel.Python.Utils
import Prelude hiding (return)

-- TODO use semantic imports to collect them and group all pretty-like
newtype PyM a = PyM {runPyM :: Writer (Set (Py.Statement ())) a}
  deriving newtype (Functor, Applicative, Monad)

instance Monoid a => Monoid (PyM a) where
  mempty = pure mempty

instance Semigroup a => Semigroup (PyM a) where
  (<>) = liftA2 (<>)

addImport :: Py.Statement () -> PyM ()
addImport = PyM . tell . Set.singleton

addImports :: [Py.Statement ()] -> PyM ()
addImports = PyM . tell . Set.fromList

pyMToModule :: PyM [Py.Statement ()] -> Py.Module ()
pyMToModule m =
  let (defs, imports) = runWriter $ runPyM m
   in Py.Module (Set.toList imports <> defs)

pyMToImportsAndDecls :: PyM [Py.Statement ()] -> ([Py.Statement ()], [Py.Statement ()])
pyMToImportsAndDecls m = swap $ second Set.toList $ runWriter $ runPyM m

mkDecl :: ParcelRepr -> Maybe (PyM [Py.Statement ()])
mkDecl repr =
  ( addImport
      (Py.FromImport (Py.ImportRelative 0 (Just [ident "dataclasses"]) ()) (Py.FromItems [Py.FromItem (ident "dataclass") Nothing ()] ()) ())
      *>
  )
    <$> case repr of
      RecordParcel recParcel -> Just $ mkRecordDecl recParcel
      SumParcel sumParcel -> Just $ mkSumTyModule sumParcel
      KnownParcel _ -> Nothing

-- >>> Pretty.prettyText $ mkEnumDecl @(Rep SimpleEnum)
-- "from enum import Enum\nclass SimpleEnum(str, Enum):\n    One = \"One\"\n    Two = \"Two\"\n    def encode(self):\n        return self"
mkEnumDecl :: EnumRepr -> PyM [Py.Statement ()]
mkEnumDecl EnumRepr {..} = do
  addImport $ Py.FromImport (Py.ImportRelative 0 (Just [ident "enum"]) ()) (Py.FromItems [Py.FromItem (ident "Enum") Nothing ()] ()) ()
  pure [enumClass]
  where
    enumClass =
      Py.Class
        (ident tyName)
        [Py.ArgExpr (unquotedString "str") (), Py.ArgExpr (unquotedString "Enum") ()]
        ( fields
            <> [encodeFun]
        )
        ()
    encodeFun = Py.Fun (ident "encode") [param "self"] Nothing [return (v "self")] ()

    fields =
      zip [1 ..] alts <&> \(_altNum, altName) ->
        Py.Assign [unquotedString altName] (string altName) ()

mkRecordDecl :: RecordRepr -> PyM [Py.Statement ()]
mkRecordDecl recordRepr = do
  addImports
    [ Py.Import [Py.ImportItem [ident "json"] (Just (ident "json")) ()] (),
      Py.FromImport (Py.ImportRelative 0 (Just [ident "typing"]) ()) (Py.ImportEverything ()) (),
      Py.Import [Py.ImportItem [ident "utils"] (Just (ident "utils")) ()] (),
      Py.FromImport (Py.ImportRelative 0 (Just [ident "pathlib"]) ()) (Py.FromItems [Py.FromItem (ident "Path") Nothing ()] ()) ()
    ]
  pure [mkRecordClass recordRepr]

mkRecordClass :: RecordRepr -> Py.Statement ()
mkRecordClass recordRepr@RecordRepr {..} =
  dataclass $
    Py.Class
      (ident name)
      []
      (fieldDecls <> [encodeDef, mkDecode recordRepr, mkLoad recordRepr])
      ()
  where
    fieldDecls = case fields of
      [] -> [Py.Pass ()]
      cases ->
        cases <&> \(caseName, ty) ->
          Py.AnnotatedAssign (unquotedString $ tyToPyTy ty) (unquotedString caseName) Nothing ()
    encodeDef = mkEncode recordRepr

-- >>> Pretty.prettyText $ mkLoad @SimpleData
-- "@classmethod\ndef load(cls):\n    currPath = Path(__file__)\n    dataDirPath = currPath.parent\n    jsonPath = dataDirPath / \"data/SimpleData.json\"\n    with jsonPath.open() as f :\n        res = json.load(f)\n        return cls(f)"
mkLoad :: RecordRepr -> Py.Statement ()
mkLoad RecordRepr {..} =
  Py.Decorated
    [Py.Decorator [ident "classmethod"] [] ()]
    ( Py.Fun
        (ident "load")
        [param "cls"]
        Nothing
        [ v "currPath" =: v "Path" @@ [arg (v "__file__")],
          v "dataDirPath" =: v "currPath" @. ident "parent",
          v "jsonPath" =: Py.BinaryOp (Py.Divide ()) (v "dataDirPath") (string $ "data/" <> name <> ".json") (),
          Py.With
            [(v "jsonPath" @. ident "open" @@ [], Just (v "f"))]
            [ v "res" =: v "json" @. ident "load" @@ [arg (v "f")],
              return $ v "cls" @. ident "decode" @@ [arg (v "res")]
            ]
            ()
        ]
        ()
    )
    ()

-- >>> Pretty.prettyText $ mkDecode @SimpleData
-- "@classmethod\ndef decode(cls, obj):\n    return cls(simpleData=obj[\"simpleData\"])"
-- >>> Pretty.prettyText $ mkDecode @CapsulePerformance
-- "@classmethod\ndef decode(cls, obj):\n    return cls(bar=DeltaTime.decode(obj[\"bar\"]))"
mkDecode :: RecordRepr -> Py.Statement ()
mkDecode RecordRepr {..} =
  Py.Decorated
    [Py.Decorator [ident "classmethod"] [] ()]
    ( Py.Fun
        (ident "decode")
        [param "cls", param "obj"]
        Nothing
        [ return
            ( v "cls"
                @@ ( fields <&> \(fieldName, fieldTy) ->
                       Py.ArgKeyword (ident fieldName) (decodeExpr fieldTy (Py.Subscript (v "obj") (string fieldName) ())) ()
                   )
            )
        ]
        ()
    )
    ()

mkEncode :: RecordRepr -> Py.Statement ()
mkEncode RecordRepr {..} =
  Py.Fun
    (ident "encode")
    [param "self"]
    Nothing
    [return objEncoding]
    ()
  where
    objEncoding = Py.Dictionary (tagEntry : fieldEntries) ()
    tagEntry = Py.DictMappingPair (string "tag") (string name)
    fieldEntries = uncurry mkDictPair <$> fields
    mkDictPair name ty =
      Py.DictMappingPair
        (string name)
        (encodeExpr ty $ v "self" @. ident name)

-- >>> mkSumTyModule @MaybeInt
-- Module [Import {import_items = [ImportItem {import_item_name = [Ident {ident_string = "json", ident_annot = ()}], import_as_name = Just (Ident {ident_string = "json", ident_annot = ()}), import_item_annot = ()}], stmt_annot = ()},FromImport {from_module = ImportRelative {import_relative_dots = 0, import_relative_module = Just [Ident {ident_string = "typing", ident_annot = ()}], import_relative_annot = ()}, from_items = ImportEverything {from_items_annot = ()}, stmt_annot = ()},Import {import_items = [ImportItem {import_item_name = [Ident {ident_string = "utils", ident_annot = ()}], import_as_name = Just (Ident {ident_string = "utils", ident_annot = ()}), import_item_annot = ()}], stmt_annot = ()},FromImport {from_module = ImportRelative {import_relative_dots = 0, import_relative_module = Just [Ident {ident_string = "pathlib", ident_annot = ()}], import_relative_annot = ()}, from_items = FromItems {from_items_items = [FromItem {from_item_name = Ident {ident_string = "Path", ident_annot = ()}, from_as_name = Nothing, from_item_annot = ()}], from_items_annot = ()}, stmt_annot = ()},Class {class_name = Ident {ident_string = "MaybeInt", ident_annot = ()}, class_args = [], class_body = [], stmt_annot = ()},Class {class_name = Ident {ident_string = "NothingInt", ident_annot = ()}, class_args = [ArgExpr {arg_expr = Strings {strings_strings = ["\"MaybeInt\""], expr_annot = ()}, arg_annot = ()}], class_body = [], stmt_annot = ()},Class {class_name = Ident {ident_string = "JustInt", ident_annot = ()}, class_args = [ArgExpr {arg_expr = Strings {strings_strings = ["\"MaybeInt\""], expr_annot = ()}, arg_annot = ()}], class_body = [], stmt_annot = ()}]
mkSumTyModule :: SumRepr -> PyM [Py.Statement ()]
mkSumTyModule SumRepr {..} = do
  addImports
    [ Py.Import [Py.ImportItem [ident "json"] (Just (ident "json")) ()] (),
      Py.FromImport (Py.ImportRelative 0 (Just [ident "typing"]) ()) (Py.ImportEverything ()) (),
      Py.Import [Py.ImportItem [ident "utils"] (Just (ident "utils")) ()] (),
      Py.FromImport (Py.ImportRelative 0 (Just [ident "pathlib"]) ()) (Py.FromItems [Py.FromItem (ident "Path") Nothing ()] ()) (),
      Py.FromImport (Py.ImportRelative 0 (Just [ident "dataclasses"]) ()) (Py.FromItems [Py.FromItem (ident "dataclass") Nothing ()] ()) ()
    ]
  pure $
    Py.Class
      (ident name)
      []
      [ Py.Fun
          (ident "__init__")
          [param "self"]
          Nothing
          [ Py.Raise (Py.RaiseV3 Nothing) ()
          ]
          ()
      ]
      () :
    subClasses
  where
    subClasses =
      alts <&> \(SumCase caseName args) ->
        let encode =
              Py.Fun
                (ident "encode")
                [param "self"]
                Nothing
                [return objEncoding]
                ()
              where
                objEncoding = Py.Dictionary (tagEntry : fieldEntries) ()
                tagEntry = Py.DictMappingPair (string "tag") (string caseName)
                fieldEntries = uncurry mkDictPair <$> args
                mkDictPair name ty =
                  Py.DictMappingPair
                    (string name)
                    (encodeExpr ty $ v "self" @. ident name)
            fields = case args of
              [] -> [Py.Pass ()]
              cases ->
                cases <&> \(caseName, ty) ->
                  Py.AnnotatedAssign (unquotedString $ tyToPyTy ty) (unquotedString caseName) Nothing ()
         in dataclass $ Py.Class (ident caseName) [Py.ArgExpr (unquotedString name) ()] (fields <> [encode]) ()

tyToPyTy :: Ty -> Text
tyToPyTy (TyRef name) = name
tyToPyTy (TKnown knownTy) = knownTyToPyTy knownTy

knownTyToPyTy :: KnownTy -> Text
knownTyToPyTy TInt = "int"
knownTyToPyTy TBool = "bool"
knownTyToPyTy TStr = "str"
knownTyToPyTy TDouble = "float"
knownTyToPyTy TDict = "dict"
knownTyToPyTy (TMap tyL tyR) = "dict[" <> tyToPyTy tyL <> "," <> tyToPyTy tyR <> "]"
knownTyToPyTy (TList arg) = "list[" <> tyToPyTy arg <> "]"
knownTyToPyTy (TOptional arg) = "Optional[" <> tyToPyTy arg <> "]"
knownTyToPyTy (TDictArg _ _) = undefined

decodeExpr :: Ty -> Py.Expr () -> Py.Expr ()
decodeExpr ty e = case ty of
  TKnown knownTy -> e
  TyRef name -> v name @. ident "decode" @@ [arg e]

encodeExpr :: Ty -> Py.Expr () -> Py.Expr ()
encodeExpr ty e = case ty of
  TKnown knownTy -> e
  TyRef name -> e @. ident "encode" @@ []
