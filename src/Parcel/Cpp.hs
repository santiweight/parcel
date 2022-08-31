{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Parcel.Cpp where

import Control.Monad
import Data.Text qualified as T
import GHC.Generics
import GHC.Generics qualified as Gen
-- import Language.C hiding (Pretty, pretty)
-- import Language.C qualified as C hiding (Pretty, pretty)
-- import Language.C.Analysis hiding (Type, VarName)
-- import Language.C.Analysis.Export
-- import Language.C.Analysis.SemRep qualified as C
-- import Language.C.Analysis.TypeUtils
-- import Language.C.System.GCC
import Parcel.Class
import Prettyprinter qualified as PP
import System.Directory.Extra (createDirectoryIfMissing)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.PrettyPrint.HughesPJ qualified as PPPJ
import Prelude
import Data.Kind (Type)
import Data.String
import Prettyprinter (Pretty (..), Doc)
import Data.Text (Text)
import Data.Functor

data SimpleEnum = One | Two
  deriving stock (Generic)
  -- deriving (ToRecord) via (Rep SingleConstructorData a)

-- #include <iostream>
-- #include <iomanip>
-- #include <nlohmann/json.hpp>
-- // for convenience
-- using json = nlohmann::json;

-- struct SimpleData {
--   int simpleData;
--   std::string toJson(SimpleData& self) {
--     json res;
--     res["simpleData"] = self.simpleData;
--     return res.dump();
--   }
-- };

-- int main() {
--   SimpleData res = SimpleData({1});
--   std::cout << res.toJson(res);
--   return 0;
-- }

-- >>> prettyS $ toCompUnit @SimpleData
-- "#include <iostream>\n#include <iomanip>\n#include <nlohmann/json.hpp>\nusing jsonnlohmann::json;\nstruct SimpleData {\n  int simpleData;\n  int toJson(int self) {\n    json res;\n    json[\"simpleData\"] = self.simpleData;\n  }\n};"
toCompUnit :: forall a. ToRecord a => CppCompUnit
toCompUnit =
  let imports = CppInclude <$> ["<iostream>", "<iomanip>", "<nlohmann/json.hpp>"]
      usings = [CppUsing "json" $ CppModulePath "nlohmann::json"]
      compUnit = CppCompUnit (imports <> usings) [mkCppStruct @a]
   in compUnit

-- >>> outputCpp @SimpleData
outputCpp :: forall a. ToRecord a => IO ()
outputCpp = do
  let name = className @a
  let outputDir = "parcel/cpp"
  createDirectoryIfMissing True outputDir
  writeFile (outputDir </> T.unpack name <.> ".hpp") (show $ pretty (toCompUnit @a))
  pure ()

usageMsg :: String -> String
usageMsg prg =
  PPPJ.render $
    PPPJ.text "Usage:" PPPJ.<+> PPPJ.text prg PPPJ.<+> PPPJ.hsep (map PPPJ.text ["CPP_OPTIONS", "input_file.c"])

class ToCpp a where
  toDecl :: forall a. CppTyDecl

instance (HasFields (f p)) => ToCpp (C1 m f p) where
  toDecl = error ""

toEnumDecl :: forall a. (Generic a, GIsCppEnum (Rep a)) => CppEnum
toEnumDecl = gToEnumDecl @(Rep a)

class GIsCppEnum a where
  gToEnumDecl :: forall a. CppEnum

instance (Datatype m, GEnumConstrs cs) => GIsCppEnum (D1 m cs) where
  gToEnumDecl = CppEnum (CppName $ T.pack $ datatypeName (undefined :: t m f k)) $ fmap EnumAltName $ gEnumConstrs @cs

class GUndatatype (a :: k -> Type) where
  type Undatatype a :: k -> Type

instance forall m f. GUndatatype (D1 m f) where
  type Undatatype (D1 m f) = f

-- >>> gEnumConstrs @(Undatatype (Rep SimpleEnum))
-- ["One","Two"]
class GEnumConstrs a where
  gEnumConstrs :: forall a. [Text]

instance (Constructor c, GEnumConstrs b) => GEnumConstrs (C1 c U1 :+: b) where
  gEnumConstrs = T.pack (Gen.conName @c undefined) : gEnumConstrs @b

instance (Constructor c) => GEnumConstrs (C1 c U1) where
  gEnumConstrs = [T.pack (Gen.conName @c undefined)]

-- instance TypeError (ShowType (Rep a)) => GIsCppEnum a where
--   gToEnumDecl = undefined
--     -- where m = (undefined :: t c f a)

-- >>> foo
-- CppEnum (CppName "SimpleEnum") [EnumAltName "One",EnumAltName "Two"]
foo :: CppEnum
foo = toEnumDecl @SimpleEnum

class GIsEnumConstr a where
  gEnumConstr :: forall a. Text

bar :: CppEnum
bar =
  gToEnumDecl
    @( D1
         ('MetaData "SimpleEnum" "Parcel.Cpp" "main" 'False)
         ( C1 ('MetaCons "One" 'PrefixI 'False) U1
             :+: C1 ('MetaCons "Two" 'PrefixI 'False) U1
         )
     )

newtype CppName = CppName Text
  deriving stock (Show)
  deriving newtype (IsString, Pretty)

newtype CppTyName = CppTyName Text
  deriving stock (Show)
  deriving newtype (IsString, Pretty)

newtype CppTyRef = CppTyRef CppTyName
  deriving stock (Show)
  deriving newtype (IsString, Pretty)

intTyRef :: CppTyRef
intTyRef = "int"

doubleTyRef :: CppTyRef
doubleTyRef = "double"

data CppTyDecl
  = CppStructDecl CppStruct
  | CppEnumDecl CppEnum

-- TODO invariants
newtype EnumAltName = EnumAltName Text
  deriving stock (Show)
  deriving newtype (IsString, Pretty)

data CppEnum = CppEnum CppName [EnumAltName]
  deriving stock (Show)

-- >>> show $ pretty $ CppEnum "arg_state" [ "get_option", "get_input" ]
-- "enum class arg_state {\n  get_option,\n  get_input,\n};"
instance Pretty CppEnum where
  pretty (CppEnum name altNames) =
    enumHerald <> PP.line <> PP.indent 2 (PP.vsep alts) <> PP.line <> "};"
    where
      alts = (<> ",") . PP.pretty <$> altNames
      enumHerald = "enum class " <> pretty name <> " {"

-- >>> show $ pretty $ CppStruct "slam_entry" [CppField intTyRef "foo", CppField intTyRef "bar"]
-- "struct slam_entry {\n  int foo;\n  int bar;\n};"
data CppStruct = CppStruct CppName [CppField] [CppFun]

instance Pretty CppStruct where
  pretty = prettyCppStruct

prettyCppStruct :: CppStruct -> Doc ann
prettyCppStruct (CppStruct name fields funs) =
  PP.vsep
    [ PP.nest 2 (PP.vsep (structHerald : (PP.pretty <$> fields) <> (PP.pretty <$> funs))),
      "};"
    ]
  where
    structHerald = "struct " <> pretty name <> " {"

data CppFun = CppFun CppTyRef CppName [CppArg] [CppStmt]
  deriving stock (Show)

data CppStmt
  = CppVarDecl CppTyRef VarName (Maybe CppExpr)
  | CppAssignDecl CppAssignIx (Maybe CppExpr)
  deriving stock (Show)

-- >>> prettyS $ CppVarDecl intTyRef "foo" Nothing
-- "int foo;"
instance Pretty CppStmt where
  -- TODO unify equations with case statement...
  pretty (CppVarDecl ty varName maybeExpr) = pretty ty <> " " <> pretty varName <> prettiedExpr <> ";"
    where
      prettiedExpr = case maybeExpr of
        Nothing -> ""
        Just expr -> " = " <> pretty expr
  pretty (CppAssignDecl (CppAssignIx varName ix) maybeExpr) = pretty varName <> "[" <> prettyIx ix <> "]" <> prettiedExpr <> ";"
    where
      prettyIx (CppStringIx str) = "\"" <> pretty str <> "\""
      prettyIx (CppNumIx num) = pretty num
      prettiedExpr = case maybeExpr of
        Nothing -> ""
        Just expr -> " = " <> pretty expr

data CppIx = CppStringIx Text | CppNumIx Int
  deriving stock (Show)

data CppAssignIx = CppAssignIx VarName CppIx
  deriving stock (Show)

newtype CppModulePath = CppModulePath Text
  deriving newtype (Pretty)

data CppImport
  = CppInclude Text
  | CppUsing Text CppModulePath

instance Pretty CppImport where
  pretty (CppInclude name) = "#include " <> pretty name
  pretty (CppUsing name modulePath) = "using " <> pretty name <> pretty modulePath <> ";"

data CppCompUnit = CppCompUnit [CppImport] [CppStruct]

instance Pretty CppCompUnit where
  pretty (CppCompUnit imports structs) = PP.vsep (PP.pretty <$> imports) <> PP.line <> PP.vsep (PP.pretty <$> structs)

data CppExpr
  = ENum Double
  | ESel CppExpr VarName
  | EVar VarName
  deriving stock (Show)

instance Pretty CppExpr where
  pretty = \case
    ENum num -> pretty num
    ESel lhs selector -> pretty lhs <> "." <> pretty selector
    EVar varName -> pretty varName

-- >>> prettyS $ toJsonFun @SimpleData
-- "toJson(int self) {\n  json res;\n  json[\"simpleData\"] = self.simpleData;\n}"
toJsonFun :: forall a. ToRecord a => CppFun
toJsonFun = CppFun intTyRef "toJson" [CppArg intTyRef "self"] (CppVarDecl "json" "res" Nothing : assignFields)
  where
    assignFields = fieldNames @a <&> (\(fieldName, _fieldTy) -> CppAssignDecl (CppAssignIx "json" $ CppStringIx fieldName) $ Just $ ESel (EVar "self") (VarName fieldName))

-- >>> prettyS $ CppFun "foo" [CppArg intTyRef "bar", CppArg doubleTyRef "baz"] [CppVarDecl intTyRef "foo" Nothing]
-- "foo(int bar, double baz) {\n  int foo;\n}"
instance Pretty CppFun where
  pretty (CppFun returnTy name args body) =
    funHerald <> PP.line <> PP.indent 2 (PP.vsep $ pretty <$> body) <> PP.line <> "}"
    where
      funHerald = pretty returnTy <> " " <> pretty name <> "(" <> PP.hsep (PP.punctuate PP.comma (pretty <$> args)) <> ") {"

data CppArg = CppArg CppTyRef VarName
  deriving stock (Show)

-- >>> prettyS $ CppArg intTyRef "foo"
-- "int foo"
instance Pretty CppArg where
  pretty (CppArg ty argName) = pretty ty <> " " <> pretty argName

newtype VarName = VarName Text
  deriving stock (Show)
  deriving newtype (IsString, Pretty)

data CppField = CppField CppTyRef CppName

instance Pretty CppField where
  pretty (CppField ty name) = pretty ty <> " " <> pretty name <> ";"

-- >>> show $ pretty $ mkCppStruct @SingleConstructorData
-- "struct SingleConstructorData {\n  int singleConsData;\n  toJson(int self) {\n    json res;\n    json[\"singleConsData\"] = self.singleConsData;\n  }\n};"
mkCppStruct :: forall a. ToRecord a => CppStruct
mkCppStruct =
  let mkField ty fieldName = CppField ty fieldName
   in CppStruct (CppName $ className @a) (mkField intTyRef . CppName . fst <$> fieldNames @a) [toJsonFun @a]

-- // create an empty structure (null)
-- json j;

-- // add a number that is stored as double (note the implicit conversion of j to an object)
-- j["pi"] = 3.141;

-- // add a Boolean that is stored as bool
-- j["happy"] = true;

-- // add a string that is stored as std::string
-- j["name"] = "Niels";

-- // add another null object by passing nullptr
-- j["nothing"] = nullptr;

-- // add an object inside the object
-- j["answer"]["everything"] = 42;

-- // add an array that is stored as std::vector (using an initializer list)
-- j["list"] = { 1, 0, 2 };

-- // add another object (using an initializer list of pairs)
-- j["object"] = { {"currency", "USD"}, {"value", 42.99} };

-- // instead, you could also write (which looks very similar to the JSON above)
-- json j2 = {
--   {"pi", 3.141},
--   {"happy", true},
--   {"name", "Niels"},
--   {"nothing", nullptr},
--   {"answer", {
--     {"everything", 42}
--   }},
--   {"list", {1, 0, 2}},
--   {"object", {
--     {"currency", "USD"},
--     {"value", 42.99}
--   }}
-- };

-- -- >>> show $ prettyUsingInclude (mkModule @SingleConstructorData)
-- -- "struct SingleConstructorData {\n    unsigned short singleConsData;\n};"
-- -- >>> show $ prettyUsingInclude (mkModule @SingleConstructorData)
-- -- "struct SingleConstructorData {\n    unsigned short singleConsData;\n};"
-- -- >>> show $ prettyUsingInclude foo
-- -- Variable not in scope: foo :: CTranslUnit
-- -- Perhaps you meant `for' (imported from Prelume)
-- mkModule :: forall a. ToRecord a => C.CTranslUnit
-- mkModule =
--   let foo = fmap CTypeSpec . fmap (() <$) . exportCompType $ mkCompTy @a
--    in undefNode <$ (CTranslUnit [CDeclExt (CDecl foo [] ())] ())

-- -- >>> mkCompTy @SingleConstructorData
-- -- CompType (NamedRef (Ident "SingleConstructorData" 366966389 (OnlyPos <builtin> (<no file>,-1)))) struct [MemberDecl (VarDecl (VarName (Ident "singleConsData" 387846849 (OnlyPos <builtin> (<no file>,-1))) Nothing) (DeclAttrs (FunctionAttrs {isInline = False, isNoreturn = False}) NoStorage []) (DirectType (TyIntegral unsigned short) (TypeQuals {constant = False, volatile = False, restrict = False, atomic = False, nullable = False, nonnull = False, clrdonly = False, clwronly = False}) [])) Nothing (OnlyPos <no file> (<no file>,-1))] [] (OnlyPos <no file> (<no file>,-1))
-- mkCompTy :: forall a. ToRecord a => CompType
-- mkCompTy =
--   let mkField fieldName =
--         MemberDecl
--           (VarDecl (C.VarName (mkIdent' fieldName) Nothing) (DeclAttrs noFunctionAttrs NoStorage []) uint16_tType)
--           Nothing
--           undefNode
--    in CompType (NamedRef $ mkClassIdent @a) StructTag (mkField <$> fieldNames @a) [] undefNode

-- errorOnLeft :: (Show a) => String -> (Either a b) -> IO b
-- errorOnLeft msg = either (error . ((msg ++ ": ") ++) . show) return

-- errorOnLeftM :: (Show a) => String -> IO (Either a b) -> IO b
-- errorOnLeftM msg action = action >>= errorOnLeft msg

-- -- >>> exportCompType compTy
-- -- [CSUType (CStruct CStructTag (Just (Ident "bar" 1880290 (OnlyPos <builtin> (<no file>,-1)))) (Just [CDecl [CTypeSpec (CUnsigType (OnlyPos <no file> (<no file>,-1))),CTypeSpec (CShortType (OnlyPos <no file> (<no file>,-1)))] [(Just (CDeclr (Just (Ident "baz" 2011362 (OnlyPos <builtin> (<no file>,-1)))) [] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] (OnlyPos <no file> (<no file>,-1))]) [] (OnlyPos <no file> (<no file>,-1))) (OnlyPos <no file> (<no file>,-1))]
-- compTy :: CompType
-- compTy =
--   CompType
--     (NamedRef $ builtinIdent "bar")
--     StructTag
--     [ MemberDecl
--         (VarDecl (C.VarName (builtinIdent "baz") Nothing) (DeclAttrs noFunctionAttrs NoStorage []) uint16_tType)
--         Nothing
--         undefNode
--     ]
--     []
--     undefNode

-- -- foo = exportDeclr [] (TypeDefType (TypeDefRef (builtinIdent "bad") _ _) noTypeQuals []) _ _

-- defineEnum :: EnumType -> C.CExtDecl
-- defineEnum ty = CDeclExt (CDecl (map CTypeSpec (exportEnumType $ ty)) [] undefNode)

-- defineComp :: CompType -> C.CExternalDeclaration C.NodeInfo
-- defineComp ty = CDeclExt (CDecl (map CTypeSpec (exportCompTypeRef ty)) [] undefNode)

-- mkModule :: forall a. ToRecord a => Py.Module ()
-- mkModule =
--   Py.Module
--     [ Py.Import
--         [Py.ImportItem [mkIdent "json"] (Just (mkIdent "json")) ()]
--         (),
--       mkClass @a
--     ]

-- mkClass :: forall a. ToRecord a => Py.Statement ()
-- mkClass =
--   Py.Class
--     (mkIdent (className (Proxy @a)))
--     []
--     -- [encodeDef, decodeDef]
--     [constructorDef, encodeDef]
--     ()
--   where
--     constructorDef =
--       Py.Fun
--         (mkIdent "__init__")
--         (mkParam "self" : (mkParam <$> fieldNames (Proxy @a)))
--         Nothing
--         ( fieldNames (Proxy @a) <&> \fieldName ->
--             Py.Assign [Py.Dot (mkVar "self") (mkIdent fieldName) ()] (mkVar fieldName) ()
--         )
--         ()
--     encodeDef =
--       Py.Fun
--         (mkIdent "encode")
--         [mkParam "self"]
--         Nothing
--         [ Py.Return
--             ( Just
--                 ( Py.Call
--                     ( Py.Dot
--                         (mkVar "json")
--                         (mkIdent "dumps")
--                         ()
--                     )
--                     [Py.ArgExpr (Py.Dictionary (Py.DictMappingPair (Py.Strings ["\"tag\""] ()) (Py.Strings [show $ className (Proxy @a)] ()) : (mkDictPair <$> fieldNames (Proxy @a))) ()) ()]
--                     ()
--                 )
--             )
--             ()
--         ]
--         ()
--     mkDictPair name =
--       Py.DictMappingPair
--         (Py.Strings [show name] ())
--         (Py.Dot (mkVar "self") (mkIdent name) ())

-- -- decodeDef = _

-- mkIdent :: Text -> Py.Ident ()
-- mkIdent name = Py.Ident {ident_string = T.unpack name, ident_annot = ()}

-- mkParam :: Text -> Py.Parameter ()
-- mkParam name =
--   Py.Param
--     { param_name = mkIdent name,
--       param_py_annotation = Nothing,
--       param_default = Nothing,
--       param_annot = ()
--     }
-- -- >>> fmap (() <$) bar
-- -- Right (CTranslUnit [CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just (Ident "foo" 1832934 (NodeInfo <no file> (<no file>,3) (Name {nameId = 0})))) (Just [CDecl [CTypeSpec (CDoubleType ())] [(Just (CDeclr (Just (Ident "foo" 1832934 (NodeInfo <no file> (<no file>,3) (Name {nameId = 1})))) [] Nothing [] ()),Nothing,Nothing)] ()]) [] ()) ())] [] ())] ())
-- bar :: Either ParseError CTranslUnit
-- bar = execParser_ translUnitP "struct foo { double foo; };" nopos

-- mkClassIdent :: forall a. ToRecord a => C.Ident
-- mkClassIdent = mkIdent' $ className @a

-- mkIdent' :: Text -> C.Ident
-- mkIdent' = builtinIdent . T.unpack
