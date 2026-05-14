{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

This module provides a GHC source plugin that generates conversion functions from user-defined ADTs
to 'Shrubbery.TaggedUnion.TaggedUnion' values.

To use the plugin, add @-fplugin Shrubbery.Plugin@ to your GHC options, then define a function
with a type signature matching the pattern @MyADT -> TaggedUnion \'[\"tag1\" \@= Type1, ...]@ and use
@magicToTaggedUnion@ as the body. The plugin will replace the body with the appropriate @case@
analysis at compile time.

Example:

@
data Fruit = Apple Int | Banana String

fruitToTaggedUnion :: Fruit -> TaggedUnion \'[\"apple\" \@= Int, \"banana\" \@= String]
fruitToTaggedUnion = magicToTaggedUnion
@

The plugin generates the equivalent of:

@
fruitToTaggedUnion fruit =
  case fruit of
    Apple val -> unifyTaggedUnion \@\"apple\" val
    Banana val -> unifyTaggedUnion \@\"banana\" val
@

Alternatively, use @magicToTaggedUnionInferType@ with a type application to have the plugin infer
the type signature automatically, using constructor names as tags:

@
fruitToTaggedUnion = magicToTaggedUnionInferType \@Fruit
@

This generates a type signature @Fruit -> TaggedUnion \'[\"Apple\" \@= Int, \"Banana\" \@= String]@
and the corresponding case expression.
-}
module Shrubbery.Plugin
  ( plugin
  , magicToTaggedUnion
  , magicToTaggedUnionInferType
  ) where

import Data.List qualified as List
import GHC.Data.FastString qualified as FS
import GHC.Driver.Plugins qualified as Plugins
import GHC.Hs qualified as GHC
import GHC.Parser.Annotation qualified as Ann
import GHC.Types.Fixity qualified as Fixity
import GHC.Types.Name.Occurrence qualified as OccName
import GHC.Types.Name.Reader qualified as RdrName
import GHC.Types.SrcLoc qualified as SrcLoc
import GHC.TypeLits (Symbol, TypeError, ErrorMessage (Text, (:<>:)))
import Language.Haskell.Syntax qualified as Syntax
import Shrubbery.GHCCompat qualified as Compat

{- | Open type family that is intentionally left without instances. When the plugin is active,
  it replaces the binding before the type-checker ever sees this constraint. When the plugin is
  not active, any use site triggers a 'TypeError' via the closed type family 'AssertPluginLoaded'.

@since 0.2.3.2
-}
type family AssertPluginLoaded (name :: Symbol) where
  AssertPluginLoaded name = TypeError (Text "Shrubbery." :<>: Text name :<>: Text " was not replaced by the plugin. Ensure -fplugin Shrubbery.Plugin is enabled.")

{- | The placeholder value that marks a function body for plugin replacement.

When the plugin is not active, any use of this function will produce a compile-time error
instructing the user to enable @-fplugin Shrubbery.Plugin@.

@since 0.2.3.2
-}
magicToTaggedUnion :: AssertPluginLoaded "magicToTaggedUnion"
magicToTaggedUnion = error "unreachable"

{- | The placeholder value that marks a function body for plugin replacement with inferred type.

Use with a type application specifying the ADT: @myFunc = magicToTaggedUnionInferType \@MyADT@.
The plugin generates both the type signature and the case expression, using constructor names as tags.

When the plugin is not active, any use of this function will produce a compile-time error
instructing the user to enable @-fplugin Shrubbery.Plugin@.

@since 0.2.3.2
-}
magicToTaggedUnionInferType :: AssertPluginLoaded "magicToTaggedUnionInferType"
magicToTaggedUnionInferType = error "unreachable"

{- | The GHC plugin. Activate with @-fplugin Shrubbery.Plugin@.

@since 0.2.3.2
-}
plugin :: Plugins.Plugin
plugin =
  Plugins.defaultPlugin
    { Plugins.parsedResultAction =
        Compat.wrapParsedResultAction
          ["magicToTaggedUnion", "magicToTaggedUnionInferType"]
          processDecls
    , Plugins.pluginRecompile = Plugins.purePlugin
    }

-- | Structured error type for plugin failures.
type PluginError = (SrcLoc.SrcSpan, String)

mkPluginError :: SrcLoc.SrcSpan -> String -> PluginError
mkPluginError = (,)

-- | Process all declarations, replacing @magicToTaggedUnion@ bodies with generated case expressions.
processDecls ::
  [GHC.LHsDecl GHC.GhcPs] ->
  Either (SrcLoc.SrcSpan, String) [GHC.LHsDecl GHC.GhcPs]
processDecls decls =
  let
    magicBindings = findMagicBindings decls
  in
    foldlAccumEither (processMagicBinding decls) decls magicBindings

-- | The kind of magic placeholder found in a binding.
data MagicKind
  = -- | @magicToTaggedUnion@ — requires a user-provided type signature.
    ExplicitSig
  | -- | @magicToTaggedUnionInferType \@ADT@ — infers the type signature from the ADT definition.
    InferType String

-- | A binding that uses a magic placeholder as its body.
data MagicBinding = MagicBinding
  { magicBindingName :: String
  , magicBindingSpan :: SrcLoc.SrcSpan
  , magicBindingDeclIndex :: Int
  , magicBindingKind :: MagicKind
  }

-- | Find all function bindings whose body is @magicToTaggedUnion@.
findMagicBindings :: [GHC.LHsDecl GHC.GhcPs] -> [MagicBinding]
findMagicBindings decls =
  concatMap findInIndexed (zip [0 ..] decls)

findInIndexed :: (Int, GHC.LHsDecl GHC.GhcPs) -> [MagicBinding]
findInIndexed (idx, SrcLoc.L _ decl) =
  case decl of
    Syntax.ValD _ bind ->
      findInBind idx bind
    _ ->
      []

findInBind :: Int -> GHC.HsBind GHC.GhcPs -> [MagicBinding]
findInBind idx bind =
  case bind of
    Syntax.FunBind {Syntax.fun_id = SrcLoc.L _ name, Syntax.fun_matches = mg} ->
      case detectMagicMatchGroup mg of
        Just kind ->
          [ MagicBinding
              { magicBindingName = OccName.occNameString (RdrName.rdrNameOcc name)
              , magicBindingSpan = Ann.getLocA (Syntax.fun_id bind)
              , magicBindingDeclIndex = idx
              , magicBindingKind = kind
              }
          ]
        Nothing -> []
    _ ->
      []

detectMagicMatchGroup :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Maybe MagicKind
detectMagicMatchGroup mg =
  case mg of
    Syntax.MG {Syntax.mg_alts = SrcLoc.L _ alts} ->
      case alts of
        [SrcLoc.L _ match] -> detectMagicMatch match
        _ -> Nothing

detectMagicMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Maybe MagicKind
detectMagicMatch match =
  case match of
    Syntax.Match {Syntax.m_pats = [], Syntax.m_grhss = grhss} ->
      detectMagicGRHSs grhss
    _ -> Nothing

detectMagicGRHSs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Maybe MagicKind
detectMagicGRHSs grhss =
  case grhss of
    Syntax.GRHSs {Syntax.grhssGRHSs = [SrcLoc.L _ grhs]} ->
      detectMagicGRHS grhs
    _ -> Nothing

detectMagicGRHS :: GHC.GRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Maybe MagicKind
detectMagicGRHS grhs =
  case grhs of
    Syntax.GRHS _ [] body -> detectMagicExpr (SrcLoc.unLoc body)
    _ -> Nothing

detectMagicExpr :: GHC.HsExpr GHC.GhcPs -> Maybe MagicKind
detectMagicExpr expr =
  case expr of
    Syntax.HsVar _ (SrcLoc.L _ name)
      | rdrNameString name == "magicToTaggedUnion" ->
          Just ExplicitSig
    _
      | Just (fun, tyArg) <- Compat.matchHsAppType expr
      , isRdrNameExpr "magicToTaggedUnionInferType" (SrcLoc.unLoc fun) ->
          case extractWcTypeName (GHC.hswc_body tyArg) of
            Just adtName -> Just (InferType adtName)
            Nothing -> Nothing
    _ -> Nothing

isRdrNameExpr :: String -> GHC.HsExpr GHC.GhcPs -> Bool
isRdrNameExpr target expr =
  case expr of
    Syntax.HsVar _ (SrcLoc.L _ name) -> rdrNameString name == target
    _ -> False

rdrNameString :: RdrName.RdrName -> String
rdrNameString = OccName.occNameString . RdrName.rdrNameOcc

extractWcTypeName :: GHC.LHsType GHC.GhcPs -> Maybe String
extractWcTypeName (SrcLoc.L _ hsType) =
  case hsType of
    Syntax.HsTyVar _ _ (SrcLoc.L _ name) ->
      Just (rdrNameString name)
    _ ->
      Nothing

{- | Process a single magic binding by finding its type signature and the ADT definition,
  then generating the replacement case expression.
-}
processMagicBinding ::
  [GHC.LHsDecl GHC.GhcPs] ->
  [GHC.LHsDecl GHC.GhcPs] ->
  MagicBinding ->
  Either PluginError [GHC.LHsDecl GHC.GhcPs]
processMagicBinding originalDecls currentDecls mb =
  case magicBindingKind mb of
    ExplicitSig ->
      processExplicitSigBinding originalDecls currentDecls mb
    InferType adtName ->
      processInferTypeBinding originalDecls currentDecls mb adtName

processExplicitSigBinding ::
  [GHC.LHsDecl GHC.GhcPs] ->
  [GHC.LHsDecl GHC.GhcPs] ->
  MagicBinding ->
  Either PluginError [GHC.LHsDecl GHC.GhcPs]
processExplicitSigBinding originalDecls currentDecls mb = do
  sigInfo <- findTypeSig originalDecls mb
  let
    adtName = sigInfoADTName sigInfo
  adtDef <- findADTDef originalDecls adtName (magicBindingSpan mb)
  let
    constructors = adtDefConstructors adtDef
  let
    tags = sigInfoTags sigInfo
  validateConstructorTagMatch (magicBindingSpan mb) (magicBindingName mb) constructors tags
  let
    newBind = generateCaseBinding mb constructors tags
  let
    replaceAtIndex i decl =
      if i == magicBindingDeclIndex mb
        then newBind
        else decl
  pure (zipWith replaceAtIndex [0 ..] currentDecls)

processInferTypeBinding ::
  [GHC.LHsDecl GHC.GhcPs] ->
  [GHC.LHsDecl GHC.GhcPs] ->
  MagicBinding ->
  String ->
  Either PluginError [GHC.LHsDecl GHC.GhcPs]
processInferTypeBinding originalDecls currentDecls mb adtName = do
  adtDef <- findADTDef originalDecls adtName (magicBindingSpan mb)
  let
    constructors = adtDefConstructors adtDef
  let
    tags = fmap constructorToTag constructors
  let
    newBind = generateCaseBinding mb constructors tags
  let
    sigDecl = generateTypeSigDecl mb adtName constructors
  let
    replaceAtIndex i decl =
      if i == magicBindingDeclIndex mb
        then newBind
        else decl
  pure (sigDecl : zipWith replaceAtIndex [0 ..] currentDecls)

constructorToTag :: ConstructorInfo -> TagInfo
constructorToTag conInfo =
  TagInfo
    { tagInfoName = constructorName conInfo
    , tagInfoType = constructorFieldType conInfo
    }

-- | Information extracted from a type signature.
data SigInfo = SigInfo
  { sigInfoADTName :: String
  , sigInfoTags :: [TagInfo]
  }

-- | A single tag entry from the type signature.
data TagInfo = TagInfo
  { tagInfoName :: String
  , tagInfoType :: String
  }

-- | Find the type signature for a magic binding.
findTypeSig :: [GHC.LHsDecl GHC.GhcPs] -> MagicBinding -> Either PluginError SigInfo
findTypeSig decls mb =
  let
    bindingName = magicBindingName mb
    sigs = concatMap (extractTypeSig bindingName) decls
  in
    case sigs of
      [] ->
        Left $
          mkPluginError (magicBindingSpan mb) $
            "shrubbery source plugin: No type signature found for '"
              <> bindingName
              <> "'. A type signature of the form '"
              <> bindingName
              <> " :: MyADT -> TaggedUnion '[\"tag\" @= Type, ...]' is required."
      (sig : _) ->
        parseSigType (magicBindingSpan mb) bindingName sig

extractTypeSig ::
  String ->
  GHC.LHsDecl GHC.GhcPs ->
  [GHC.LHsSigWcType GHC.GhcPs]
extractTypeSig name (SrcLoc.L _ decl) =
  case decl of
    Syntax.SigD _ sig ->
      case sig of
        Syntax.TypeSig _ ids sigType ->
          if any (matchesName name) ids
            then [sigType]
            else []
        _ -> []
    _ -> []

matchesName :: String -> GHC.LIdP GHC.GhcPs -> Bool
matchesName name (SrcLoc.L _ rdrName) =
  OccName.occNameString (RdrName.rdrNameOcc rdrName) == name

-- | Parse a type signature to extract the ADT name and tagged type list.
parseSigType ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.LHsSigWcType GHC.GhcPs ->
  Either PluginError SigInfo
parseSigType srcSpan bindingName sigWcType =
  let
    sigType = GHC.hswc_body sigWcType
    hsType = Syntax.sig_body (SrcLoc.unLoc sigType)
  in
    parseFunctionType srcSpan bindingName (SrcLoc.unLoc hsType)

-- | Parse a function type @ADT -> TaggedUnion '[...]@.
parseFunctionType ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.HsType GHC.GhcPs ->
  Either PluginError SigInfo
parseFunctionType srcSpan bindingName hsType =
  case hsType of
    Syntax.HsFunTy _ _arrow argType resultType ->
      do
        adtName <- extractTypeName srcSpan bindingName (SrcLoc.unLoc argType)
        tags <- extractTaggedUnionTags srcSpan bindingName (SrcLoc.unLoc resultType)
        pure
          SigInfo
            { sigInfoADTName = adtName
            , sigInfoTags = tags
            }
    _ ->
      Left $
        mkPluginError srcSpan $
          "shrubbery source plugin: The type signature for '"
            <> bindingName
            <> "' must be a function type of the form 'MyADT -> TaggedUnion [...]'."

-- | Extract a simple type name from a type expression.
extractTypeName ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.HsType GHC.GhcPs ->
  Either PluginError String
extractTypeName srcSpan bindingName hsType =
  case hsType of
    Syntax.HsTyVar _ _ (SrcLoc.L _ name) ->
      pure (OccName.occNameString (RdrName.rdrNameOcc name))
    _ ->
      Left $
        mkPluginError srcSpan $
          "shrubbery source plugin: The argument type in the signature for '"
            <> bindingName
            <> "' must be a simple type name (e.g., 'MyADT'), not a compound type."

-- | Extract the tagged types from @TaggedUnion '[...]@.
extractTaggedUnionTags ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.HsType GHC.GhcPs ->
  Either PluginError [TagInfo]
extractTaggedUnionTags srcSpan bindingName hsType =
  case hsType of
    -- TaggedUnion '[...]
    Syntax.HsAppTy _ funTy argTy ->
      case SrcLoc.unLoc funTy of
        Syntax.HsTyVar _ _ (SrcLoc.L _ name)
          | OccName.occNameString (RdrName.rdrNameOcc name) == "TaggedUnion" ->
              extractTagList srcSpan bindingName (SrcLoc.unLoc argTy)
        _ ->
          Left (notTaggedUnionError srcSpan bindingName)
    _ ->
      Left (notTaggedUnionError srcSpan bindingName)

notTaggedUnionError :: SrcLoc.SrcSpan -> String -> PluginError
notTaggedUnionError srcSpan bindingName =
  mkPluginError srcSpan $
    "shrubbery source plugin: The return type in the signature for '"
      <> bindingName
      <> "' must be 'TaggedUnion [\"tag\" @= Type, ...]'."

-- | Extract tag entries from a promoted list type @'[\"tag1\" \@= Type1, ...]@.
extractTagList ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.HsType GHC.GhcPs ->
  Either PluginError [TagInfo]
extractTagList srcSpan bindingName hsType =
  case hsType of
    Syntax.HsExplicitListTy _ _ items ->
      traverse (extractTagEntry srcSpan bindingName . SrcLoc.unLoc) items
    _ ->
      Left $
        mkPluginError srcSpan $
          "shrubbery source plugin: The TaggedUnion argument in the signature for '"
            <> bindingName
            <> "' must be a promoted list like '[\"tag\" @= Type, ...]'."

-- | Extract a single tag entry from @\"tag\" \@= Type@.
extractTagEntry ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.HsType GHC.GhcPs ->
  Either PluginError TagInfo
extractTagEntry srcSpan bindingName hsType =
  case Compat.matchHsOpTy hsType of
    Just (op, lhs, rhs) ->
      let
        opName = OccName.occNameString (RdrName.rdrNameOcc (SrcLoc.unLoc op))
      in
        if opName == "@="
          then do
            tagName <- extractStringLiteral srcSpan bindingName (SrcLoc.unLoc lhs)
            typeName <- extractTypeName srcSpan bindingName (SrcLoc.unLoc rhs)
            pure TagInfo {tagInfoName = tagName, tagInfoType = typeName}
          else Left (badTagEntryError srcSpan bindingName)
    Nothing ->
      Left (badTagEntryError srcSpan bindingName)

badTagEntryError :: SrcLoc.SrcSpan -> String -> PluginError
badTagEntryError srcSpan bindingName =
  mkPluginError srcSpan $
    "shrubbery source plugin: Each entry in the TaggedUnion list for '"
      <> bindingName
      <> "' must be of the form '\"tag\" @= Type'."

-- | Extract a string literal from a type-level string.
extractStringLiteral ::
  SrcLoc.SrcSpan ->
  String ->
  GHC.HsType GHC.GhcPs ->
  Either PluginError String
extractStringLiteral srcSpan bindingName hsType =
  case hsType of
    Syntax.HsTyLit _ tyLit ->
      case tyLit of
        Syntax.HsStrTy _ fs -> pure (FS.unpackFS fs)
        _ ->
          Left $
            mkPluginError srcSpan $
              "shrubbery source plugin: Tag names in the TaggedUnion list for '"
                <> bindingName
                <> "' must be string literals."
    _ ->
      Left $
        mkPluginError srcSpan $
          "shrubbery source plugin: Tag names in the TaggedUnion list for '"
            <> bindingName
            <> "' must be string literals."

-- | An ADT definition extracted from the module.
data ADTDef = ADTDef
  { adtDefConstructors :: [ConstructorInfo]
  }

-- | Information about a single constructor.
data ConstructorInfo = ConstructorInfo
  { constructorName :: String
  , constructorFieldType :: String
  , constructorFieldTypeNode :: Maybe (GHC.LHsType GHC.GhcPs)
  }

-- | Find the ADT definition in the module declarations.
findADTDef ::
  [GHC.LHsDecl GHC.GhcPs] ->
  String ->
  SrcLoc.SrcSpan ->
  Either PluginError ADTDef
findADTDef decls adtName srcSpan =
  let
    adts = concatMap (extractADTDef adtName) decls
  in
    case adts of
      [] ->
        Left $
          mkPluginError srcSpan $
            "shrubbery source plugin: Could not find a data type named '"
              <> adtName
              <> "' in this module. The ADT must be defined in the same module as the conversion function."
      (adt : _) ->
        pure adt

extractADTDef ::
  String ->
  GHC.LHsDecl GHC.GhcPs ->
  [ADTDef]
extractADTDef adtName (SrcLoc.L _ decl) =
  case decl of
    Syntax.TyClD _ tyClDecl ->
      case tyClDecl of
        Syntax.DataDecl {Syntax.tcdLName = SrcLoc.L _ name, Syntax.tcdDataDefn = dataDefn} ->
          if OccName.occNameString (RdrName.rdrNameOcc name) == adtName
            then [extractDataDefn dataDefn]
            else []
        _ -> []
    _ -> []

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
        (fieldTypeStr, fieldTypeNode) = extractConDeclH98Field args
      in
        ConstructorInfo
          { constructorName = rdrNameString name
          , constructorFieldType = fieldTypeStr
          , constructorFieldTypeNode = fieldTypeNode
          }
    Syntax.ConDeclGADT {} ->
      ConstructorInfo
        { constructorName = Compat.getGADTConName conDecl
        , constructorFieldType = "()"
        , constructorFieldTypeNode = Nothing
        }

extractConDeclH98Field ::
  Syntax.HsConDeclH98Details GHC.GhcPs ->
  (String, Maybe (GHC.LHsType GHC.GhcPs))
extractConDeclH98Field args =
  case args of
    Syntax.PrefixCon _ [GHC.HsScaled _ fieldTy] ->
      (typeNodeToString (SrcLoc.unLoc fieldTy), Just fieldTy)
    _ ->
      ("()", Nothing)

typeNodeToString :: GHC.HsType GHC.GhcPs -> String
typeNodeToString hsType =
  case hsType of
    Syntax.HsTyVar _ _ (SrcLoc.L _ name) -> rdrNameString name
    _ -> "()"

-- | Validate that the number of constructors matches the number of tags.
validateConstructorTagMatch ::
  SrcLoc.SrcSpan ->
  String ->
  [ConstructorInfo] ->
  [TagInfo] ->
  Either PluginError ()
validateConstructorTagMatch srcSpan bindingName constructors tags =
  let
    numCons = length constructors
    numTags = length tags
  in
    if numCons /= numTags
      then
        Left $
          mkPluginError srcSpan $
            "shrubbery source plugin: The data type has "
              <> show numCons
              <> " constructor(s) but the TaggedUnion for '"
              <> bindingName
              <> "' has "
              <> show numTags
              <> " tag(s). These must match."
              <> " Constructors: "
              <> List.intercalate ", " (fmap constructorName constructors)
              <> ". Tags: "
              <> List.intercalate ", " (fmap tagInfoName tags)
              <> "."
      else
        pure ()

-- | Generate a new declaration with the case expression replacing the magic body.
generateCaseBinding ::
  MagicBinding ->
  [ConstructorInfo] ->
  [TagInfo] ->
  GHC.LHsDecl GHC.GhcPs
generateCaseBinding mb constructors tags =
  let
    funName = magicBindingName mb
    argName = "shrubberyArg"

    funRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc funName)
    argRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc argName)

    argExpr = mkHsVar argRdrName
    argPat = mkVarPat argRdrName

    caseAlts = zipWith generateCaseAlt constructors tags

    caseExpr = mkHsCase argExpr caseAlts

    matchGroup = mkSingleMatchGroup funRdrName [argPat] caseExpr

    bind = Compat.mkFunBind (Compat.mkLocated funRdrName) matchGroup

    decl = Syntax.ValD GHC.noExtField bind
  in
    Compat.mkLocated decl

{- | Generate a type signature declaration for the infer-type case:
  @funName :: ADT -> TaggedUnion '[\"Con1\" \@= Type1, ...]@
-}
generateTypeSigDecl ::
  MagicBinding ->
  String ->
  [ConstructorInfo] ->
  GHC.LHsDecl GHC.GhcPs
generateTypeSigDecl mb adtName constructors =
  let
    funRdrName :: GHC.LIdP GHC.GhcPs
    funRdrName = Compat.mkLocated (RdrName.mkRdrUnqual (OccName.mkVarOcc (magicBindingName mb)))

    adtTy :: GHC.LHsType GHC.GhcPs
    adtTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTcOcc adtName))

    tagEntries :: [GHC.LHsType GHC.GhcPs]
    tagEntries = fmap mkTagEntry constructors

    tagListTy :: GHC.LHsType GHC.GhcPs
    tagListTy = Compat.mkLocated (Syntax.HsExplicitListTy Ann.noAnn Compat.isPromoted tagEntries)

    taggedUnionTy :: GHC.LHsType GHC.GhcPs
    taggedUnionTy =
      Compat.mkLocated
        ( Syntax.HsAppTy
            GHC.noExtField
            (mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTcOcc "TaggedUnion")))
            tagListTy
        )

    funTy :: GHC.LHsType GHC.GhcPs
    funTy = Compat.mkFunTy adtTy taggedUnionTy

    sigType :: GHC.LHsSigType GHC.GhcPs
    sigType =
      Compat.mkLocated
        Syntax.HsSig
          { Syntax.sig_ext = GHC.noExtField
          , Syntax.sig_bndrs = Syntax.HsOuterImplicit GHC.noExtField
          , Syntax.sig_body = funTy
          }

    sigWcType :: GHC.LHsSigWcType GHC.GhcPs
    sigWcType =
      Syntax.HsWC
        { Syntax.hswc_ext = GHC.noExtField
        , Syntax.hswc_body = sigType
        }

    sig = Syntax.TypeSig Ann.noAnn [funRdrName] sigWcType
  in
    Compat.mkLocated (Syntax.SigD GHC.noExtField sig)

mkTagEntry :: ConstructorInfo -> GHC.LHsType GHC.GhcPs
mkTagEntry conInfo =
  let
    tagTy :: GHC.LHsType GHC.GhcPs
    tagTy = Compat.mkLocated (Compat.mkHsTyLit (constructorName conInfo))

    fieldTy :: GHC.LHsType GHC.GhcPs
    fieldTy = case constructorFieldTypeNode conInfo of
      Just ty -> ty
      Nothing -> mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTcOcc (constructorFieldType conInfo)))

    opName :: GHC.LIdP GHC.GhcPs
    opName = Compat.mkLocated (RdrName.mkRdrUnqual (OccName.mkTcOcc "@="))
  in
    Compat.mkLocated (Compat.mkHsOpTy tagTy opName fieldTy)

mkHsTyVar :: RdrName.RdrName -> GHC.LHsType GHC.GhcPs
mkHsTyVar name =
  Compat.mkLocated (Syntax.HsTyVar Ann.noAnn Compat.notPromoted (Compat.mkLocated name))

{- | Generate a single case alternative:
  @ConstructorName val -> unifyTaggedUnion \@\"tag\" val@
-}
generateCaseAlt ::
  ConstructorInfo ->
  TagInfo ->
  GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
generateCaseAlt conInfo tag =
  let
    conRdrName = RdrName.mkRdrUnqual (OccName.mkDataOcc (constructorName conInfo))
    valRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "val")

    conPat = mkConPat conRdrName [mkVarPat valRdrName]

    tagTypeArg = mkHsTyLitString (tagInfoName tag)

    -- unifyTaggedUnion @"tag" val
    unifyExpr =
      Compat.mkHsAppType
        (mkHsVar (RdrName.mkRdrUnqual (OccName.mkVarOcc "unifyTaggedUnion")))
        tagTypeArg

    body = Compat.mkHsApp unifyExpr (mkHsVar valRdrName)

    grhs = Compat.mkGRHS body
    grhss = mkGRHSs [grhs]
  in
    Compat.mkLocated
      Syntax.Match
        { Syntax.m_ext = Ann.noAnn
        , Syntax.m_ctxt = Syntax.CaseAlt
        , Syntax.m_pats = [conPat]
        , Syntax.m_grhss = grhss
        }

-- AST construction helpers

mkHsVar :: RdrName.RdrName -> GHC.LHsExpr GHC.GhcPs
mkHsVar name =
  Compat.mkLocated (Syntax.HsVar GHC.noExtField (Compat.mkLocated name))

mkVarPat :: RdrName.RdrName -> GHC.LPat GHC.GhcPs
mkVarPat name =
  Compat.mkLocated (Syntax.VarPat GHC.noExtField (Compat.mkLocated name))

mkConPat :: RdrName.RdrName -> [GHC.LPat GHC.GhcPs] -> GHC.LPat GHC.GhcPs
mkConPat conName pats =
  Compat.mkLocated
    Syntax.ConPat
      { Syntax.pat_con_ext = Ann.noAnn
      , Syntax.pat_con = Compat.mkLocated conName
      , Syntax.pat_args = Syntax.PrefixCon [] pats
      }

mkHsTyLitString :: String -> GHC.LHsWcType GHC.GhcPs
mkHsTyLitString str =
  Syntax.HsWC GHC.noExtField (Compat.mkLocated (Compat.mkHsTyLit str))

mkHsCase ::
  GHC.LHsExpr GHC.GhcPs ->
  [GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)] ->
  GHC.LHsExpr GHC.GhcPs
mkHsCase scrutinee alts =
  let
    mg = Compat.mkMatchGroup (Compat.mkLocated alts)
  in
    Compat.mkLocated (Syntax.HsCase Ann.noAnn scrutinee mg)

mkGRHSs :: [GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)] -> GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
mkGRHSs grhsList =
  Syntax.GRHSs
    { Syntax.grhssExt = GHC.emptyComments
    , Syntax.grhssGRHSs = grhsList
    , Syntax.grhssLocalBinds = Syntax.EmptyLocalBinds GHC.noExtField
    }

mkSingleMatchGroup ::
  RdrName.RdrName ->
  [GHC.LPat GHC.GhcPs] ->
  GHC.LHsExpr GHC.GhcPs ->
  GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
mkSingleMatchGroup funName pats body =
  let
    grhs = Compat.mkGRHS body
    grhss = mkGRHSs [grhs]
    matchCtxt =
      Syntax.FunRhs
        { Syntax.mc_fun = Compat.mkLocated funName
        , Syntax.mc_fixity = Fixity.Prefix
        , Syntax.mc_strictness = Compat.noSrcStrict
        }
    match :: GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
    match =
      Compat.mkLocated
        Syntax.Match
          { Syntax.m_ext = Ann.noAnn
          , Syntax.m_ctxt = matchCtxt
          , Syntax.m_pats = pats
          , Syntax.m_grhss = grhss
          }
  in
    Compat.mkMatchGroup (Compat.mkLocated [match])

-- | Left-fold over a list, threading an accumulator that can fail.
foldlAccumEither :: (b -> a -> Either e b) -> b -> [a] -> Either e b
foldlAccumEither _ acc [] = Right acc
foldlAccumEither f acc (x : xs) =
  case f acc x of
    Left err -> Left err
    Right newAcc -> foldlAccumEither f newAcc xs
