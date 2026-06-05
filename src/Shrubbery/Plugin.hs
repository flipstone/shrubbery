{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

This module provides a GHC source plugin that generates 'Shrubbery.Classes.TaggedUnionable'
instances for user-defined ADTs.

To use the plugin, add @-fplugin Shrubbery.Plugin@ to your GHC options, then write a
standalone deriving declaration:

@
data Fruit = Apple Int | Banana String

deriving instance ShrubberyMagic => TaggedUnionable Fruit
@

The plugin generates the equivalent of:

@
instance ShrubberyMagic => TaggedUnionable Fruit where
  type TaggedBranchTypes Fruit = \'[\"Apple\" \@= Int, \"Banana\" \@= String]
  dissectTagged branches fruit =
    case usingShrubberyMagic fruit of
      Apple val -> selectBranchAtTag \@\"Apple\" branches val
      Banana val -> selectBranchAtTag \@\"Banana\" branches val
  unifyTaggedWithTag (_ :: proxy tag) =
    selectBranchAtTag \@tag
      ( taggedBranchBuild
      $ taggedBranch \@\"Apple\" Apple
      $ taggedBranch \@\"Banana\" Banana
      $ taggedBranchEnd
      )
@

Nullary constructors (with no fields) are mapped to @()@:

@
data Light = On | Off

deriving instance ShrubberyMagic => TaggedUnionable Light
@

generates @TaggedBranchTypes Light = \'[\"On\" \@= (), \"Off\" \@= ()]@, with dissection passing @()@
as the value and unification wrapping the constructor in @\\() -> Con@.

Rather than constructing the instance as GHC AST nodes directly, the plugin renders it as Haskell
source text and parses it with GHC's own parser (see 'Shrubbery.GHCCompat.parseInstanceDecls').
This keeps the plugin almost entirely free of the GHC-version-specific AST handling that would
otherwise be required.
-}
module Shrubbery.Plugin
  ( plugin
  , ShrubberyMagic (..)
  ) where

import Data.List (intercalate)
import GHC.Driver.Plugins qualified as Plugins
import GHC.Driver.Session qualified as Session
import GHC.Hs qualified as GHC
import GHC.Types.Name.Occurrence qualified as OccName
import GHC.Types.Name.Reader qualified as RdrName
import GHC.Types.SrcLoc qualified as SrcLoc
import Language.Haskell.Syntax qualified as Syntax
import Shrubbery.GHCCompat qualified as Compat

{- | Marker typeclass used in standalone deriving declarations. When the plugin is active,
  @deriving instance ShrubberyMagic => TaggedUnionable MyAdt@ is intercepted and replaced with a
  generated instance that keeps the 'ShrubberyMagic' constraint.

  The 'usingShrubberyMagic' method is an identity function used by generated code to make the
  constraint non-redundant.

@since 0.2.4.0
-}
class ShrubberyMagic where
  usingShrubberyMagic :: a -> a

instance ShrubberyMagic where
  {-# INLINE usingShrubberyMagic #-}
  usingShrubberyMagic = id

{- | The GHC plugin. Activate with @-fplugin Shrubbery.Plugin@.

@since 0.2.3.2
-}
plugin :: Plugins.Plugin
plugin =
  Plugins.defaultPlugin
    { Plugins.parsedResultAction =
        Compat.wrapParsedResultAction
          processDecls
    , Plugins.pluginRecompile = Plugins.purePlugin
    }

-- | The module qualifier used by plugin-generated code to reference shrubbery internals.
magicQualifier :: String
magicQualifier = "Shrubbery_Magic_"

-- | Qualify an internal name under the magic qualifier, e.g. @Shrubbery_Magic_.taggedBranch@.
magicQualified :: String -> String
magicQualified name = magicQualifier ++ "." ++ name

-- | Qualified imports added to any module where the plugin generates instances.
magicImports :: [GHC.LImportDecl GHC.GhcPs]
magicImports =
  [ Compat.mkQualifiedImportDecl "Shrubbery.Plugin" magicQualifier
  , Compat.mkQualifiedImportDecl "Shrubbery.TaggedBranches" magicQualifier
  , Compat.mkQualifiedImportDecl "Shrubbery.TypeList" magicQualifier
  ]

{- | Process all declarations, replacing standalone deriving declarations that match the
  @ShrubberyMagic => TaggedUnionable X@ pattern with generated instances.
  Returns any imports to add and the new declarations.
-}
processDecls ::
  Session.DynFlags ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Either (SrcLoc.SrcSpan, String) ([GHC.LImportDecl GHC.GhcPs], [GHC.LHsDecl GHC.GhcPs])
processDecls dflags decls = do
  let
    adtDefs = collectADTDefs decls
  results <- traverse (processDecl dflags adtDefs) decls
  let
    anyMagic = any processDeclWasMagic results
    newDecls = concatMap processDeclDecls results
    imports =
      if anyMagic
        then magicImports
        else []
  pure (imports, newDecls)

-- | Result of processing a single declaration.
data ProcessDeclResult = ProcessDeclResult
  { processDeclWasMagic :: Bool
  , processDeclDecls :: [GHC.LHsDecl GHC.GhcPs]
  }

-- | Result of matching a standalone deriving declaration.
data DerivingMatch = DerivingMatch
  { derivingMatchAdtName :: String
  , derivingMatchInstHead :: String
  }

{- | Process a single declaration. If it is a standalone deriving declaration matching
  @deriving instance ShrubberyMagic => TaggedUnionable X@, replace it with the generated
  instance. Otherwise, return it unchanged.
-}
processDecl ::
  Session.DynFlags ->
  [(String, ADTDef)] ->
  GHC.LHsDecl GHC.GhcPs ->
  Either (SrcLoc.SrcSpan, String) ProcessDeclResult
processDecl dflags adtDefs lDecl@(SrcLoc.L _ decl) =
  case decl of
    Syntax.DerivD _ derivDecl ->
      case matchShrubberyMagicDeriving derivDecl of
        Just match ->
          case lookup (derivingMatchAdtName match) adtDefs of
            Just adtDef ->
              let
                source = renderInstanceModule match (adtDefConstructors adtDef)
              in
                case Compat.parseInstanceDecls dflags source of
                  Right generatedDecls ->
                    Right
                      ProcessDeclResult
                        { processDeclWasMagic = True
                        , processDeclDecls = generatedDecls
                        }
                  Left err ->
                    Left (SrcLoc.noSrcSpan, err)
            Nothing ->
              Right (ProcessDeclResult False [lDecl])
        Nothing ->
          Right (ProcessDeclResult False [lDecl])
    _ ->
      Right (ProcessDeclResult False [lDecl])

-- | Collect all data type definitions from the module declarations into an association list.
collectADTDefs :: [GHC.LHsDecl GHC.GhcPs] -> [(String, ADTDef)]
collectADTDefs = concatMap collectADTDef

collectADTDef :: GHC.LHsDecl GHC.GhcPs -> [(String, ADTDef)]
collectADTDef (SrcLoc.L _ decl) =
  case decl of
    Syntax.TyClD _ tyClDecl ->
      case tyClDecl of
        Syntax.DataDecl {Syntax.tcdLName = SrcLoc.L _ name, Syntax.tcdDataDefn = dataDefn} ->
          let
            adtName = OccName.occNameString (RdrName.rdrNameOcc name)
          in
            [(adtName, extractDataDefn dataDefn)]
        _ ->
          []
    _ ->
      []

{- | Match a standalone deriving declaration of the form
  @deriving instance ShrubberyMagic => TaggedUnionable X@.

  Returns a 'DerivingMatch' containing the ADT name and the user's instance head rendered as
  source text (reused verbatim for the generated instance head), or 'Nothing' if the pattern
  doesn't match.
-}
matchShrubberyMagicDeriving :: Syntax.DerivDecl GHC.GhcPs -> Maybe DerivingMatch
matchShrubberyMagicDeriving derivDecl =
  case Syntax.deriv_type derivDecl of
    Syntax.HsWC _ (SrcLoc.L _ sigTy) ->
      case SrcLoc.unLoc (Syntax.sig_body sigTy) of
        Syntax.HsQualTy _ ctxt bodyTy ->
          case (hasShrubberyMagicCtxt (SrcLoc.unLoc ctxt), extractTaggedUnionableApp (SrcLoc.unLoc bodyTy)) of
            (True, Just adtName) ->
              Just
                DerivingMatch
                  { derivingMatchAdtName = adtName
                  , derivingMatchInstHead = Compat.showHsType (Syntax.sig_body sigTy)
                  }
            _ -> Nothing
        _ ->
          Nothing

hasShrubberyMagicCtxt :: [GHC.LHsType GHC.GhcPs] -> Bool
hasShrubberyMagicCtxt =
  any isShrubberyMagicTy

isShrubberyMagicTy :: GHC.LHsType GHC.GhcPs -> Bool
isShrubberyMagicTy (SrcLoc.L _ ty) =
  case ty of
    Syntax.HsTyVar _ _ (SrcLoc.L _ name) ->
      rdrNameString name == "ShrubberyMagic"
    _ ->
      False

extractTaggedUnionableApp :: GHC.HsType GHC.GhcPs -> Maybe String
extractTaggedUnionableApp ty =
  case ty of
    Syntax.HsAppTy _ (SrcLoc.L _ classTy) (SrcLoc.L _ adtTy) ->
      case (classTy, adtTy) of
        (Syntax.HsTyVar _ _ (SrcLoc.L _ className), Syntax.HsTyVar _ _ (SrcLoc.L _ adtName)) ->
          if rdrNameString className == "TaggedUnionable"
            then Just (rdrNameString adtName)
            else Nothing
        _ ->
          Nothing
    _ ->
      Nothing

rdrNameString :: RdrName.RdrName -> String
rdrNameString = OccName.occNameString . RdrName.rdrNameOcc

-- | An ADT definition extracted from the module.
data ADTDef = ADTDef
  { adtDefConstructors :: [ConstructorInfo]
  }

-- | Information about a single constructor.
data ConstructorInfo = ConstructorInfo
  { constructorName :: String
  , constructorFieldType :: String
  , constructorIsNullary :: Bool
  }

extractDataDefn :: Syntax.HsDataDefn GHC.GhcPs -> ADTDef
extractDataDefn dataDefn =
  let
    cons = Compat.getDataDefnCons dataDefn
  in
    ADTDef
      { adtDefConstructors = fmap (extractConstructor . SrcLoc.unLoc) cons
      }

extractConstructor :: Syntax.ConDecl GHC.GhcPs -> ConstructorInfo
extractConstructor conDecl =
  case conDecl of
    Syntax.ConDeclH98 {Syntax.con_name = SrcLoc.L _ name, Syntax.con_args = args} ->
      let
        (fieldTypeStr, isNullary) = extractConDeclH98Field args
      in
        ConstructorInfo
          { constructorName = rdrNameString name
          , constructorFieldType = fieldTypeStr
          , constructorIsNullary = isNullary
          }
    Syntax.ConDeclGADT {} ->
      ConstructorInfo
        { constructorName = Compat.getGADTConName conDecl
        , constructorFieldType = "()"
        , constructorIsNullary = True
        }

extractConDeclH98Field ::
  Syntax.HsConDeclH98Details GHC.GhcPs ->
  (String, Bool)
extractConDeclH98Field args =
  case args of
    Syntax.PrefixCon _ [GHC.HsScaled _ fieldTy] ->
      (Compat.showHsType fieldTy, False)
    Syntax.PrefixCon _ [] ->
      ("()", True)
    _ ->
      ("()", True)

-- | Instance generation

{- | Render the generated instance as a complete (header-bearing) Haskell module so that it can be
  fed to GHC's parser. Only the instance declaration is meaningful; the module wrapper exists only
  to satisfy 'parseModule'. For @data Fruit = Apple Int | Banana String@ this produces, modulo
  whitespace:

  @
    module ShrubberyPluginGenerated where
    instance ShrubberyMagic => TaggedUnionable Fruit where {
      type TaggedBranchTypes Fruit = \'[\"Apple\" Shrubbery_Magic_.\@= Int, \"Banana\" Shrubbery_Magic_.\@= String] ;
      dissectTagged shrubberyBranches shrubberyArg = case Shrubbery_Magic_.usingShrubberyMagic shrubberyArg of {
        Apple val -> Shrubbery_Magic_.selectBranchAtTag \@\"Apple\" shrubberyBranches val ;
        Banana val -> Shrubbery_Magic_.selectBranchAtTag \@\"Banana\" shrubberyBranches val
      } ;
      unifyTaggedWithTag (_ :: proxy tag) = Shrubbery_Magic_.selectBranchAtTag \@tag (...)
    }
  @

  Explicit braces and semicolons are used for the instance body and case expression so the output
  is insensitive to layout.
-}
renderInstanceModule :: DerivingMatch -> [ConstructorInfo] -> String
renderInstanceModule match constructors =
  unlines
    [ "module ShrubberyPluginGenerated where"
    , renderInstance match constructors
    ]

renderInstance :: DerivingMatch -> [ConstructorInfo] -> String
renderInstance match constructors =
  concat
    [ "instance ", derivingMatchInstHead match, " where {\n"
    , renderTypeFamilyInstance match constructors, " ;\n"
    , renderDissectionBind constructors, " ;\n"
    , renderUnificationBind constructors, "\n"
    , "}\n"
    ]

-- | @type TaggedBranchTypes ADT = '[\"Con1\" Shrubbery_Magic_.\@= Type1, ...]@
renderTypeFamilyInstance :: DerivingMatch -> [ConstructorInfo] -> String
renderTypeFamilyInstance match constructors =
  concat
    [ "  type TaggedBranchTypes ", derivingMatchAdtName match
    , " = '[", intercalate ", " (fmap renderTagEntry constructors), "]"
    ]

-- | @\"Con\" Shrubbery_Magic_.\@= FieldType@
renderTagEntry :: ConstructorInfo -> String
renderTagEntry conInfo =
  concat
    [ stringLiteral (constructorName conInfo)
    , " ", magicQualified "@=", " "
    , constructorFieldType conInfo
    ]

{- | @dissectTagged shrubberyBranches shrubberyArg =
       case Shrubbery_Magic_.usingShrubberyMagic shrubberyArg of { Con val -> ... ; ... }@
-}
renderDissectionBind :: [ConstructorInfo] -> String
renderDissectionBind constructors =
  concat
    [ "  dissectTagged shrubberyBranches shrubberyArg = case "
    , magicQualified "usingShrubberyMagic", " shrubberyArg of {\n"
    , intercalate " ;\n" (fmap renderDissectionAlt constructors)
    , "\n  }"
    ]

renderDissectionAlt :: ConstructorInfo -> String
renderDissectionAlt conInfo =
  let
    (pat, valExpr) =
      if constructorIsNullary conInfo
        then (constructorName conInfo, "()")
        else (constructorName conInfo ++ " val", "val")
  in
    concat
      [ "    ", pat, " -> "
      , magicQualified "selectBranchAtTag"
      , " @", stringLiteral (constructorName conInfo)
      , " shrubberyBranches ", valExpr
      ]

{- | @unifyTaggedWithTag (_ :: proxy tag) =
       Shrubbery_Magic_.selectBranchAtTag \@tag (taggedBranchBuild (taggedBranch \@\"Con\" Con (...)))@
-}
renderUnificationBind :: [ConstructorInfo] -> String
renderUnificationBind constructors =
  concat
    [ "  unifyTaggedWithTag (_ :: proxy tag) = "
    , magicQualified "selectBranchAtTag", " @tag "
    , renderBranchChain constructors
    ]

{- | @(taggedBranchBuild (taggedBranch \@\"Con1\" Con1 (taggedBranch \@\"Con2\" Con2 taggedBranchEnd)))@

  This is the right-nested application that @taggedBranchBuild $ taggedBranch ... $ ... $
  taggedBranchEnd@ would desugar to.
-}
renderBranchChain :: [ConstructorInfo] -> String
renderBranchChain constructors =
  let
    chain =
      foldr
        (\conInfo rest -> concat ["(", renderBranch conInfo, " ", rest, ")"])
        (magicQualified "taggedBranchEnd")
        constructors
  in
    concat ["(", magicQualified "taggedBranchBuild", " ", chain, ")"]

-- | @Shrubbery_Magic_.taggedBranch \@\"Con\" Con@, or @... \@\"Con\" (\\() -> Con)@ for nullary.
renderBranch :: ConstructorInfo -> String
renderBranch conInfo =
  let
    conExpr =
      if constructorIsNullary conInfo
        then "(\\() -> " ++ constructorName conInfo ++ ")"
        else constructorName conInfo
  in
    concat
      [ magicQualified "taggedBranch"
      , " @", stringLiteral (constructorName conInfo)
      , " ", conExpr
      ]

-- | Render a 'String' as a Haskell string-literal token, e.g. @Con@ becomes @\"Con\"@.
stringLiteral :: String -> String
stringLiteral str =
  "\"" ++ str ++ "\""
