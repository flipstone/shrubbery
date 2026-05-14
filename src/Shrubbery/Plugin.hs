{-# LANGUAGE ImportQualifiedPost #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

This module provides a GHC source plugin that generates 'Shrubbery.Classes.TaggedBranchTypes',
'Shrubbery.Classes.TaggedDissection', and 'Shrubbery.Classes.TaggedUnification' instances for
user-defined ADTs.

To use the plugin, add @-fplugin Shrubbery.Plugin@ to your GHC options, then add a
@deriving ShrubberyMagic@ clause to your data type:

@
data Fruit = Apple Int | Banana String
  deriving ShrubberyMagic
@

The plugin generates the equivalent of:

@
type instance TaggedBranchTypes Fruit = \'[\"Apple\" \@= Int, \"Banana\" \@= String]
instance TaggedDissection Fruit where
  dissectTagged branches fruit =
    case fruit of
      Apple val -> selectBranchAtTag \@\"Apple\" branches val
      Banana val -> selectBranchAtTag \@\"Banana\" branches val
instance TaggedUnification Fruit where
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
  , ShrubberyMagic
  ) where

import GHC.Data.Bag qualified as Bag
import GHC.Driver.Plugins qualified as Plugins
import GHC.Hs qualified as GHC
import GHC.Parser.Annotation qualified as Ann
import GHC.Types.Fixity qualified as Fixity
import GHC.Types.Name.Occurrence qualified as OccName
import GHC.Types.Name.Reader qualified as RdrName
import GHC.Types.SrcLoc qualified as SrcLoc
import Language.Haskell.Syntax qualified as Syntax
import Shrubbery.GHCCompat qualified as Compat

{- | Marker type used in @deriving@ clauses. When the plugin is active, @deriving ShrubberyMagic@
  is intercepted and replaced with generated instances. The deriving clause is removed from the
  data declaration before the type-checker sees it.

@since 0.2.4.0
-}
data ShrubberyMagic

{- | The GHC plugin. Activate with @-fplugin Shrubbery.Plugin@.

@since 0.2.3.2
-}
plugin :: Plugins.Plugin
plugin =
  Plugins.defaultPlugin
    { Plugins.parsedResultAction =
        Compat.wrapParsedResultAction
          ["ShrubberyMagic"]
          processDecls
    , Plugins.pluginRecompile = Plugins.purePlugin
    }

-- | Process all declarations, replacing @deriving ShrubberyMagic@ with generated instances.
processDecls ::
  [GHC.LHsDecl GHC.GhcPs] ->
  Either (SrcLoc.SrcSpan, String) [GHC.LHsDecl GHC.GhcPs]
processDecls decls =
  Right (concatMap processDecl decls)

-- | Process a single declaration. If it is a data decl with @deriving ShrubberyMagic@,
--   strip the clause and emit the data decl followed by generated instance decls.
processDecl ::
  GHC.LHsDecl GHC.GhcPs ->
  [GHC.LHsDecl GHC.GhcPs]
processDecl lDecl@(SrcLoc.L loc decl) =
  case decl of
    Syntax.TyClD xTyCl tyClDecl ->
      case tyClDecl of
        Syntax.DataDecl {Syntax.tcdLName = SrcLoc.L _ name, Syntax.tcdDataDefn = dataDefn} ->
          let
            derivClauses = Syntax.dd_derivs dataDefn
          in
            case extractShrubberyMagicClause derivClauses of
              Nothing ->
                [lDecl]
              Just remainingClauses ->
                let
                  adtName = OccName.occNameString (RdrName.rdrNameOcc name)
                  strippedDataDefn = dataDefn {Syntax.dd_derivs = remainingClauses}
                  strippedDecl = SrcLoc.L loc (Syntax.TyClD xTyCl (tyClDecl {Syntax.tcdDataDefn = strippedDataDefn}))
                  adtDef = extractDataDefn dataDefn
                  constructors = adtDefConstructors adtDef
                  tyFamInst = generateTaggedBranchTypesDecl adtName constructors
                  dissectionInst = generateDissectionInstDecl adtName constructors
                  unificationInst = generateUnificationInstDecl adtName constructors
                in
                  [strippedDecl, tyFamInst, dissectionInst, unificationInst]
        _ ->
          [lDecl]
    _ ->
      [lDecl]

-- | Check if any deriving clause is @deriving ShrubberyMagic@. If found, return the
--   remaining clauses with the magic one removed.
extractShrubberyMagicClause ::
  [GHC.LHsDerivingClause GHC.GhcPs] ->
  Maybe [GHC.LHsDerivingClause GHC.GhcPs]
extractShrubberyMagicClause clauses =
  let
    (magic, rest) = foldr partitionClause ([], []) clauses
  in
    case magic of
      [] -> Nothing
      _ -> Just rest

partitionClause ::
  GHC.LHsDerivingClause GHC.GhcPs ->
  ([GHC.LHsDerivingClause GHC.GhcPs], [GHC.LHsDerivingClause GHC.GhcPs]) ->
  ([GHC.LHsDerivingClause GHC.GhcPs], [GHC.LHsDerivingClause GHC.GhcPs])
partitionClause lClause@(SrcLoc.L _ clause) (magicAcc, restAcc) =
  case clause of
    Syntax.HsDerivingClause {Syntax.deriv_clause_strategy = Nothing, Syntax.deriv_clause_tys = tys} ->
      case isShrubberyMagicTys (SrcLoc.unLoc tys) of
        True -> (lClause : magicAcc, restAcc)
        False -> (magicAcc, lClause : restAcc)
    _ ->
      (magicAcc, lClause : restAcc)

isShrubberyMagicTys :: Syntax.DerivClauseTys GHC.GhcPs -> Bool
isShrubberyMagicTys tys =
  case tys of
    Syntax.DctSingle _ sigTy ->
      isShrubberyMagicSigType sigTy
    Syntax.DctMulti _ sigTys ->
      any isShrubberyMagicSigType sigTys

isShrubberyMagicSigType :: GHC.LHsSigType GHC.GhcPs -> Bool
isShrubberyMagicSigType (SrcLoc.L _ sigTy) =
  case Syntax.sig_body sigTy of
    SrcLoc.L _ (Syntax.HsTyVar _ _ (SrcLoc.L _ name)) ->
      rdrNameString name == "ShrubberyMagic"
    _ ->
      False

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

{- | Generate: @type instance TaggedBranchTypes ADT = '[\"Con1\" \@= Type1, ...]@ -}
generateTaggedBranchTypesDecl ::
  String ->
  [ConstructorInfo] ->
  GHC.LHsDecl GHC.GhcPs
generateTaggedBranchTypesDecl adtName constructors =
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
        { Syntax.feqn_ext = []
        , Syntax.feqn_tycon = tyConName
        , Syntax.feqn_bndrs = Syntax.HsOuterImplicit GHC.noExtField
        , Syntax.feqn_pats = Compat.mkFamEqnPats [adtTy]
        , Syntax.feqn_fixity = Fixity.Prefix
        , Syntax.feqn_rhs = tagListTy
        }

    tyFamInstDecl :: Syntax.TyFamInstDecl GHC.GhcPs
    tyFamInstDecl =
      Syntax.TyFamInstDecl
        { Syntax.tfid_xtn = []
        , Syntax.tfid_eqn = famEqn
        }

    instDecl :: Syntax.InstDecl GHC.GhcPs
    instDecl =
      Syntax.TyFamInstD
        { Syntax.tfid_ext = GHC.noExtField
        , Syntax.tfid_inst = tyFamInstDecl
        }
  in
    Compat.mkLocated (Syntax.InstD GHC.noExtField instDecl)

{- | Generate:

  @
    instance TaggedDissection ADT where
      dissectTagged shrubberyBranches shrubberyArg =
        case shrubberyArg of
          Con1 val -> selectBranchAtTag \@"Con1" shrubberyBranches val
          Con2 val -> selectBranchAtTag \@"Con2" shrubberyBranches val
  @
-}
generateDissectionInstDecl ::
  String ->
  [ConstructorInfo] ->
  GHC.LHsDecl GHC.GhcPs
generateDissectionInstDecl adtName constructors =
  let
    branchesRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "shrubberyBranches")
    argRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "shrubberyArg")

    caseAlts = fmap generateDissectionCaseAlt constructors

    caseExpr = mkHsCase (mkHsVar argRdrName) caseAlts

    methodRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "dissectTagged")

    matchGroup =
      mkSingleMatchGroup
        methodRdrName
        [mkVarPat branchesRdrName, mkVarPat argRdrName]
        caseExpr

    methodBind = Compat.mkFunBind (Compat.mkLocated methodRdrName) matchGroup

    instHead = mkClassInstHead "TaggedDissection" adtName
  in
    mkClsInstDecl instHead [Compat.mkLocated methodBind]

{- | Generate a single case alternative for TaggedDissection:

  @Con val -> selectBranchAtTag \@"Con" branches val@ -}
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
        (mkHsVar (RdrName.mkRdrUnqual (OccName.mkVarOcc "selectBranchAtTag")))
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

{- | Generate:

  @
    instance TaggedUnification ADT where
      unifyTaggedWithTag _ =
        selectBranchAtTag \@tag
          ( taggedBranchBuild
            $ taggedBranch \@"Con1" Con1
            $ taggedBranch \@"Con2" Con2
            $ taggedBranchEnd
          )
  @
-}
generateUnificationInstDecl ::
  String ->
  [ConstructorInfo] ->
  GHC.LHsDecl GHC.GhcPs
generateUnificationInstDecl adtName constructors =
  let
    methodRdrName = RdrName.mkRdrUnqual (OccName.mkVarOcc "unifyTaggedWithTag")

    -- (_ :: proxy tag) pattern to bring 'tag' into scope
    proxyPat = mkTypedWildPat "proxy" "tag"

    -- selectBranchAtTag @tag (taggedBranchBuild $ taggedBranch @"Con1" Con1 $ ... $ taggedBranchEnd)
    tagTyVar = mkHsWcTyVar "tag"
    selectExpr =
      Compat.mkHsAppType
        (mkHsVar (RdrName.mkRdrUnqual (OccName.mkVarOcc "selectBranchAtTag")))
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

    methodBind = Compat.mkFunBind (Compat.mkLocated methodRdrName) matchGroup

    instHead = mkClassInstHead "TaggedUnification" adtName
  in
    mkClsInstDecl instHead [Compat.mkLocated methodBind]

-- | Generate @taggedBranchBuild $ taggedBranch \@"Con1" Con1 $ taggedBranch \@"Con2" Con2 $ ... $ taggedBranchEnd@
generateTaggedBranchChain :: [ConstructorInfo] -> GHC.LHsExpr GHC.GhcPs
generateTaggedBranchChain constructors =
  let
    branchBuildExpr = mkHsVar (RdrName.mkRdrUnqual (OccName.mkVarOcc "taggedBranchBuild"))

    branchExprs =
      fmap
        ( \conInfo ->
            let
              tagTypeArg = mkHsTyLitString (constructorName conInfo)
              taggedBranchExpr =
                Compat.mkHsAppType
                  (mkHsVar (RdrName.mkRdrUnqual (OccName.mkVarOcc "taggedBranch")))
                  tagTypeArg
            in
              Compat.mkHsApp
                taggedBranchExpr
                (mkHsVar (RdrName.mkRdrUnqual (OccName.mkDataOcc (constructorName conInfo))))
        )
        constructors

    branchEndExpr = mkHsVar (RdrName.mkRdrUnqual (OccName.mkVarOcc "taggedBranchEnd"))
  in
    mkDollarChain (branchBuildExpr : branchExprs ++ [branchEndExpr])

-- | Create a class instance head: @ClassName ADTName@
mkClassInstHead :: String -> String -> GHC.LHsSigType GHC.GhcPs
mkClassInstHead className adtName =
  let
    classTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTcOcc className))
    adtTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTcOcc adtName))
    appTy =
      Compat.mkLocated
        ( Syntax.HsAppTy
            GHC.noExtField
            classTy
            adtTy
        )
  in
    Compat.mkLocated
      Syntax.HsSig
        { Syntax.sig_ext = GHC.noExtField
        , Syntax.sig_bndrs = Syntax.HsOuterImplicit GHC.noExtField
        , Syntax.sig_body = appTy
        }

-- | Create a class instance declaration.
mkClsInstDecl ::
  GHC.LHsSigType GHC.GhcPs ->
  [GHC.LHsBind GHC.GhcPs] ->
  GHC.LHsDecl GHC.GhcPs
mkClsInstDecl instHead binds =
  let
    clsInstDecl =
      Syntax.ClsInstDecl
        { Syntax.cid_ext = Compat.mkClsInstDeclExt
        , Syntax.cid_poly_ty = instHead
        , Syntax.cid_binds = Bag.listToBag binds
        , Syntax.cid_sigs = []
        , Syntax.cid_tyfam_insts = []
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
  Compat.mkLocated (Syntax.HsPar Ann.noAnn expr)

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
    tyConTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTyVarOcc tyConName))
    tyVarTy = mkHsTyVar (RdrName.mkRdrUnqual (OccName.mkTyVarOcc tyVarName))
    appTy =
      Compat.mkLocated
        (Syntax.HsAppTy GHC.noExtField tyConTy tyVarTy)
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
    opName = Compat.mkLocated (RdrName.mkRdrUnqual (OccName.mkTcOcc "@="))
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

