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
-}
module Shrubbery.Plugin
  ( plugin
  , ShrubberyMagic (..)
  ) where

import GHC.Data.Bag qualified as Bag
import GHC.Driver.Plugins qualified as Plugins
import GHC.Hs qualified as GHC
import GHC.Parser.Annotation qualified as Ann
import GHC.Types.Fixity qualified as Fixity
import GHC.Types.Name.Occurrence qualified as OccName
import GHC.Types.Name.Reader qualified as RdrName
import GHC.Types.SrcLoc qualified as SrcLoc
import GHC.Unit.Module qualified as Module
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
magicQualifier :: Module.ModuleName
magicQualifier = Module.mkModuleName "Shrubbery_Magic_"

-- | Qualified imports added to any module where the plugin generates instances.
magicImports :: [GHC.LImportDecl GHC.GhcPs]
magicImports =
  [ Compat.mkQualifiedImportDecl "Shrubbery.Plugin" "Shrubbery_Magic_"
  , Compat.mkQualifiedImportDecl "Shrubbery.TaggedBranches" "Shrubbery_Magic_"
  , Compat.mkQualifiedImportDecl "Shrubbery.TypeList" "Shrubbery_Magic_"
  ]

-- | Process all declarations, replacing standalone deriving declarations that match the
--   @ShrubberyMagic => TaggedUnionable X@ pattern with generated instances.
--   Returns any imports to add and the new declarations.
processDecls ::
  [GHC.LHsDecl GHC.GhcPs] ->
  Either (SrcLoc.SrcSpan, String) ([GHC.LImportDecl GHC.GhcPs], [GHC.LHsDecl GHC.GhcPs])
processDecls decls =
  let
    adtDefs = collectADTDefs decls
    results = fmap (processDecl adtDefs) decls
    anyMagic = any processDeclWasMagic results
    newDecls = concatMap processDeclDecls results
    imports =
      if anyMagic
        then magicImports
        else []
  in
    Right (imports, newDecls)

-- | Result of processing a single declaration.
data ProcessDeclResult = ProcessDeclResult
  { processDeclWasMagic :: Bool
  , processDeclDecls :: [GHC.LHsDecl GHC.GhcPs]
  }

-- | Result of matching a standalone deriving declaration.
data DerivingMatch = DerivingMatch
  { derivingMatchAdtName :: String
  , derivingMatchInstType :: GHC.LHsSigType GHC.GhcPs
  }

{- | Process a single declaration. If it is a standalone deriving declaration matching
  @deriving instance ShrubberyMagic => TaggedUnionable X@, replace it with the generated
  instance. Otherwise, return it unchanged.
-}
processDecl ::
  [(String, ADTDef)] ->
  GHC.LHsDecl GHC.GhcPs ->
  ProcessDeclResult
processDecl adtDefs lDecl@(SrcLoc.L _ decl) =
  case decl of
    Syntax.DerivD _ derivDecl ->
      case matchShrubberyMagicDeriving derivDecl of
        Just match ->
          case lookup (derivingMatchAdtName match) adtDefs of
            Just adtDef ->
              let
                constructors = adtDefConstructors adtDef
              in
                ProcessDeclResult
                  { processDeclWasMagic = True
                  , processDeclDecls = [generateTaggedUnionableInstDecl (derivingMatchInstType match) (derivingMatchAdtName match) constructors]
                  }
            Nothing ->
              ProcessDeclResult {processDeclWasMagic = False, processDeclDecls = [lDecl]}
        Nothing ->
          ProcessDeclResult {processDeclWasMagic = False, processDeclDecls = [lDecl]}
    _ ->
      ProcessDeclResult {processDeclWasMagic = False, processDeclDecls = [lDecl]}

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

  Returns a 'DerivingMatch' containing the ADT name and the user's full instance type
  (for reuse as the generated instance head), or 'Nothing' if the pattern doesn't match.
-}
matchShrubberyMagicDeriving :: Syntax.DerivDecl GHC.GhcPs -> Maybe DerivingMatch
matchShrubberyMagicDeriving derivDecl =
  case Syntax.deriv_type derivDecl of
    Syntax.HsWC _ lSigTy@(SrcLoc.L _ sigTy) ->
      case SrcLoc.unLoc (Syntax.sig_body sigTy) of
        Syntax.HsQualTy _ ctxt bodyTy ->
          case (hasShrubberyMagicCtxt (SrcLoc.unLoc ctxt), extractTaggedUnionableApp (SrcLoc.unLoc bodyTy)) of
            (True, Just adtName) ->
              Just
                DerivingMatch
                  { derivingMatchAdtName = adtName
                  , derivingMatchInstType = lSigTy
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
  , constructorFieldTypeNode :: Maybe (GHC.LHsType GHC.GhcPs)
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

-- | Instance generation

{- | Generate:

  @
    instance ShrubberyMagic => TaggedUnionable ADT where
      type TaggedBranchTypes ADT = '[\"Con1\" \@= Type1, ...]
      dissectTagged shrubberyBranches shrubberyArg =
        case usingShrubberyMagic shrubberyArg of
          Con1 val -> selectBranchAtTag \@"Con1" shrubberyBranches val
          Con2 val -> selectBranchAtTag \@"Con2" shrubberyBranches val
      unifyTaggedWithTag _ =
        selectBranchAtTag \@tag
          ( taggedBranchBuild
            $ taggedBranch \@"Con1" Con1
            $ taggedBranch \@"Con2" Con2
            $ taggedBranchEnd
          )
  @
-}
generateTaggedUnionableInstDecl ::
  GHC.LHsSigType GHC.GhcPs ->
  String ->
  [ConstructorInfo] ->
  GHC.LHsDecl GHC.GhcPs
generateTaggedUnionableInstDecl userInstType adtName constructors =
  let
    -- Associated type family: type TaggedBranchTypes ADT = '[...]
    tyFamInst = generateTaggedBranchTypesFamInst adtName constructors

    -- dissectTagged method
    dissectionBind = generateDissectionBind constructors

    -- unifyTaggedWithTag method
    unificationBind = generateUnificationBind constructors
  in
    mkClsInstDeclWithTyFam userInstType [tyFamInst] [Compat.mkLocated dissectionBind, Compat.mkLocated unificationBind]

-- | Generate the associated type instance: @type TaggedBranchTypes ADT = '[\"Con1\" \@= Type1, ...]@
generateTaggedBranchTypesFamInst ::
  String ->
  [ConstructorInfo] ->
  Syntax.TyFamInstDecl GHC.GhcPs
generateTaggedBranchTypesFamInst adtName constructors =
  let
    tagEntries :: [GHC.LHsType GHC.GhcPs]
    tagEntries = fmap mkTagEntry constructors

    tagListTy :: GHC.LHsType GHC.GhcPs
    tagListTy = Compat.mkLocated (Syntax.HsExplicitListTy Ann.noAnn Compat.isPromoted tagEntries)

    tyConName :: GHC.LIdP GHC.GhcPs
    tyConName = Compat.mkLocated (RdrName.mkRdrUnqual (OccName.mkTcOcc "TaggedBranchTypes"))

    adtTy :: GHC.LHsType GHC.GhcPs
    adtTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTcOcc adtName))

    famEqn :: Syntax.TyFamInstEqn GHC.GhcPs
    famEqn =
      Syntax.FamEqn
        { Syntax.feqn_ext = Compat.noAnnEpAnn
        , Syntax.feqn_tycon = tyConName
        , Syntax.feqn_bndrs = Syntax.HsOuterImplicit GHC.noExtField
        , Syntax.feqn_pats = Compat.mkFamEqnPats [adtTy]
        , Syntax.feqn_fixity = Fixity.Prefix
        , Syntax.feqn_rhs = tagListTy
        }
  in
    Syntax.TyFamInstDecl
      { Syntax.tfid_xtn = Compat.noAnnEpAnn
      , Syntax.tfid_eqn = famEqn
      }

{- | Generate the dissectTagged method binding:

  @
    dissectTagged shrubberyBranches shrubberyArg =
      case usingShrubberyMagic shrubberyArg of
        Con1 val -> selectBranchAtTag \@"Con1" shrubberyBranches val
        Con2 val -> selectBranchAtTag \@"Con2" shrubberyBranches val
  @
-}
generateDissectionBind ::
  [ConstructorInfo] ->
  GHC.HsBind GHC.GhcPs
generateDissectionBind constructors =
  let
    branchesRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "shrubberyBranches")
    argRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "shrubberyArg")

    caseAlts = fmap generateDissectionCaseAlt constructors

    scrutinee =
      Compat.mkHsApp
        (mkHsVar (mkMagicQual (OccName.mkVarOcc "usingShrubberyMagic")))
        (mkHsVar argRdrName)

    caseExpr = mkHsCase scrutinee caseAlts

    methodRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "dissectTagged")

    matchGroup =
      mkSingleMatchGroup
        methodRdrName
        [mkVarPat branchesRdrName, mkVarPat argRdrName]
        caseExpr
  in
    Compat.mkFunBind (Compat.mkLocated methodRdrName) matchGroup

{- | Generate the unifyTaggedWithTag method binding:

  @
    unifyTaggedWithTag _ =
      selectBranchAtTag \@tag
        ( taggedBranchBuild
          $ taggedBranch \@"Con1" Con1
          $ taggedBranch \@"Con2" Con2
          $ taggedBranchEnd
        )
  @
-}
generateUnificationBind ::
  [ConstructorInfo] ->
  GHC.HsBind GHC.GhcPs
generateUnificationBind constructors =
  let
    methodRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "unifyTaggedWithTag")

    -- (_ :: proxy tag) pattern to bring 'tag' into scope
    proxyPat = mkTypedWildPat "proxy" "tag"

    -- selectBranchAtTag @tag (taggedBranchBuild $ taggedBranch @"Con1" Con1 $ ... $ taggedBranchEnd)
    tagTyVar = mkHsWcTyVar "tag"
    selectExpr =
      Compat.mkHsAppType
        (mkHsVar (mkMagicQual (OccName.mkVarOcc "selectBranchAtTag")))
        tagTyVar

    branchChain = generateTaggedBranchChain constructors

    body =
      Compat.mkHsApp
        selectExpr
        (mkHsPar branchChain)

    matchGroup =
      mkSingleMatchGroup
        methodRdrName
        [proxyPat]
        body
  in
    Compat.mkFunBind (Compat.mkLocated methodRdrName) matchGroup
generateDissectionCaseAlt ::
  ConstructorInfo ->
  GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
generateDissectionCaseAlt conInfo =
  let
    conRdrName = RdrName.mkRdrUnqual (OccName.mkDataOcc (constructorName conInfo))
    valRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "val")
    branchesRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "shrubberyBranches")

    conPat = mkConPat conRdrName [mkVarPat valRdrName]

    tagTypeArg = mkHsTyLitString (constructorName conInfo)

    -- selectBranchAtTag @"Con" shrubberyBranches val
    selectExpr =
      Compat.mkHsAppType
        (mkHsVar (mkMagicQual (OccName.mkVarOcc "selectBranchAtTag")))
        tagTypeArg

    body =
      Compat.mkHsApp
        (Compat.mkHsApp selectExpr (mkHsVar branchesRdrName))
        (mkHsVar valRdrName)

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

-- | Generate @taggedBranchBuild $ taggedBranch \@"Con1" Con1 $ taggedBranch \@"Con2" Con2 $ ... $ taggedBranchEnd@
generateTaggedBranchChain :: [ConstructorInfo] -> GHC.LHsExpr GHC.GhcPs
generateTaggedBranchChain constructors =
  let
    branchBuildExpr = mkHsVar (mkMagicQual (OccName.mkVarOcc "taggedBranchBuild"))

    branchExprs =
      fmap
        ( \conInfo ->
            let
              tagTypeArg = mkHsTyLitString (constructorName conInfo)
              taggedBranchExpr =
                Compat.mkHsAppType
                  (mkHsVar (mkMagicQual (OccName.mkVarOcc "taggedBranch")))
                  tagTypeArg
            in
              Compat.mkHsApp
                taggedBranchExpr
                (mkHsVar (RdrName.mkRdrUnqual (OccName.mkDataOcc (constructorName conInfo))))
        )
        constructors

    branchEndExpr = mkHsVar (mkMagicQual (OccName.mkVarOcc "taggedBranchEnd"))
  in
    mkDollarChain (branchBuildExpr : branchExprs ++ [branchEndExpr])

-- | Create a qualified RdrName under the magic qualifier.
mkMagicQual :: OccName.OccName -> RdrName.RdrName
mkMagicQual = RdrName.mkRdrQual magicQualifier

-- | Create a class instance declaration with associated type family instances.
mkClsInstDeclWithTyFam ::
  GHC.LHsSigType GHC.GhcPs ->
  [Syntax.TyFamInstDecl GHC.GhcPs] ->
  [GHC.LHsBind GHC.GhcPs] ->
  GHC.LHsDecl GHC.GhcPs
mkClsInstDeclWithTyFam instHead tyFamInsts binds =
  let
    clsInstDecl =
      Syntax.ClsInstDecl
        { Syntax.cid_ext = Compat.mkClsInstDeclExt
        , Syntax.cid_poly_ty = instHead
        , Syntax.cid_binds = Bag.listToBag binds
        , Syntax.cid_sigs = []
        , Syntax.cid_tyfam_insts = fmap Compat.mkLocated tyFamInsts
        , Syntax.cid_datafam_insts = []
        , Syntax.cid_overlap_mode = Nothing
        }

    instDecl =
      Syntax.ClsInstD
        { Syntax.cid_d_ext = GHC.noExtField
        , Syntax.cid_inst = clsInstDecl
        }
  in
    Compat.mkLocated (Syntax.InstD GHC.noExtField instDecl)

-- | Wrap an expression in parentheses: @(expr)@
mkHsPar :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
mkHsPar expr =
  Compat.mkHsPar expr

-- | Make a wildcard-wrapped type variable, for use in type applications.
mkHsWcTyVar :: String -> GHC.LHsWcType GHC.GhcPs
mkHsWcTyVar name =
  Syntax.HsWC
    GHC.noExtField
    (Compat.mkLocated (Syntax.HsTyVar Ann.noAnn Compat.notPromoted (Compat.mkLocated (RdrName.mkRdrUnqual (OccName.mkTyVarOcc name)))))

-- | Generate a pattern @(_ :: tyConName tyVarName)@ to bring a type variable into scope.
mkTypedWildPat :: String -> String -> GHC.LPat GHC.GhcPs
mkTypedWildPat tyConName tyVarName =
  let
    tyConTy :: GHC.LHsType GHC.GhcPs
    tyConTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTyVarOcc tyConName))

    tyVarTy :: GHC.LHsType GHC.GhcPs
    tyVarTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTyVarOcc tyVarName))

    appTy :: GHC.LHsType GHC.GhcPs
    appTy =
      Compat.mkLocated
        (Syntax.HsAppTy GHC.noExtField tyConTy tyVarTy)

    wildPat :: GHC.LPat GHC.GhcPs
    wildPat = Compat.mkLocated (Syntax.WildPat GHC.noExtField)
  in
    Compat.mkLocated (Compat.mkSigPat wildPat appTy)

-- | Chain expressions with ($): @e1 $ e2 $ e3@ becomes @e1 $ (e2 $ e3)@
mkDollarChain :: [GHC.LHsExpr GHC.GhcPs] -> GHC.LHsExpr GHC.GhcPs
mkDollarChain [] = error "mkDollarChain: empty list"
mkDollarChain [e] = e
mkDollarChain (e : es) =
  let
    dollarRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "$")
    rest = mkDollarChain es
  in
    Compat.mkLocated
      ( Syntax.OpApp
          Ann.noAnn
          e
          (mkHsVar dollarRdrName)
          rest
      )

-- AST construction helpers

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
    opName = Compat.mkLocated (mkMagicQual (OccName.mkTcOcc "@="))
  in
    Compat.mkLocated (Compat.mkHsOpTy tagTy opName fieldTy)

mkHsTyVar :: RdrName.RdrName -> GHC.LHsType GHC.GhcPs
mkHsTyVar name =
  Compat.mkLocated (Syntax.HsTyVar Ann.noAnn Compat.notPromoted (Compat.mkLocated name))

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
    Compat.mkHsCase scrutinee mg

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
