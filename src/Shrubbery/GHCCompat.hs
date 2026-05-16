{- FOURMOLU_DISABLE -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

Compatibility shim for GHC API differences across versions. This module isolates all CPP
conditionals so that the rest of the plugin code can be version-agnostic.

@since 0.2.3.2
-}
module Shrubbery.GHCCompat
  ( mkLocated
  , mkHsApp
  , mkHsAppType
  , noSrcStrict
  , notPromoted
  , isPromoted
  , mkMatchGroup
  , getModuleDecls
  , setModuleDecls
  , getModuleImports
  , setModuleImports
  , mkQualifiedImportDecl
  , getDataDefnCons
  , getGADTConName
  , mkHsTyLit
  , mkFunBind
  , mkHsOpTy
  , mkGRHS
  , mkHsPar
  , mkHsCase
  , mkFamEqnPats
  , mkClsInstDeclExt
  , mkSigPat
  , noAnnEpAnn
  , wrapParsedResultAction
  ) where

#if !MIN_VERSION_ghc(9,6,0)
import GHC.Core.DataCon qualified as DataCon
import GHC.Unit.Types qualified as UnitTypes
#endif

import GHC.Data.FastString qualified as FS
import GHC.Driver.Plugins qualified as Plugins
import GHC.Hs qualified as GHC
import GHC.Parser.Annotation qualified as Ann
import GHC.Types.Name.Occurrence qualified as OccName
import GHC.Types.Name.Reader qualified as RdrName
import GHC.Types.SrcLoc qualified as SrcLoc
import GHC.Types.SourceText qualified as SourceText
import GHC.Utils.Outputable qualified as Outputable
import Language.Haskell.Syntax qualified as Syntax

import GHC.Types.Basic qualified as BasicTypes

import GHC.Driver.Env.Types qualified as HscTypes
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Types qualified as PsErrors
import GHC.Types.Error qualified as Error
import GHC.Types.PkgQual qualified as PkgQual
import GHC.Unit.Module qualified as Module
import GHC.Unit.Module.ModSummary qualified as ModSummary
import GHC.Utils.Error qualified as ErrUtils

#if MIN_VERSION_ghc(9,6,0)
import Data.List.NonEmpty qualified as NEL
import Language.Haskell.Syntax.Basic qualified as SyntaxBasic
#endif

{- | Wrap a value with an empty source location annotation.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,10,0)
mkLocated :: Ann.HasAnnotation ann => a -> SrcLoc.GenLocated ann a
mkLocated = SrcLoc.L Ann.noSrcSpanA
#else
mkLocated :: a -> SrcLoc.GenLocated (Ann.SrcAnn ann) a
mkLocated = SrcLoc.L Ann.noSrcSpanA
#endif

{- | Construct a 'Syntax.MatchGroup' with the appropriate extension field for the GHC version.

  In GHC 9.6+, 'Syntax.MG' carries an 'Origin' value; in earlier versions, the extension is
  'NoExtField'.

@since 0.2.3.2
-}
mkMatchGroup ::
  GHC.XRec GHC.GhcPs [GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)] ->
  Syntax.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
mkMatchGroup alts =
  Syntax.MG
#if MIN_VERSION_ghc(9,6,0)
    { Syntax.mg_ext = mgExtField
    , Syntax.mg_alts = alts
    }
#else
    { Syntax.mg_ext = GHC.noExtField
    , Syntax.mg_alts = alts
    , Syntax.mg_origin = BasicTypes.Generated
    }
#endif

#if MIN_VERSION_ghc(9,10,0)
mgExtField :: BasicTypes.Origin
mgExtField = BasicTypes.Generated BasicTypes.OtherExpansion BasicTypes.DoPmc
#elif MIN_VERSION_ghc(9,8,0)
mgExtField :: BasicTypes.Origin
mgExtField = BasicTypes.Generated BasicTypes.DoPmc
#elif MIN_VERSION_ghc(9,6,0)
mgExtField :: BasicTypes.Origin
mgExtField = BasicTypes.Generated
#endif

{- | Construct an 'HsApp' expression node.

@since 0.2.3.2
-}
mkHsApp :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
mkHsApp fun arg =
#if MIN_VERSION_ghc(9,10,0)
  mkLocated (Syntax.HsApp GHC.noExtField fun arg)
#else
  mkLocated (Syntax.HsApp Ann.noAnn fun arg)
#endif

{- | Construct an 'HsAppType' expression node (visible type application).

@since 0.2.3.2
-}
mkHsAppType :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsWcType GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
mkHsAppType fun tyArg =
#if MIN_VERSION_ghc(9,10,0)
  mkLocated (Syntax.HsAppType Ann.noAnn fun tyArg)
#elif MIN_VERSION_ghc(9,6,0)
  mkLocated (Syntax.HsAppType GHC.noExtField fun (SrcLoc.L Ann.NoTokenLoc GHC.HsTok) tyArg)
#else
  mkLocated (Syntax.HsAppType SrcLoc.noSrcSpan fun tyArg)
#endif

{- | Wrap a declaration-processing function into a full 'parsedResultAction' handler.

  The callback receives the module's declarations and returns a list of import declarations to
  add and the new list of declarations. Errors are added to 'PsMessages' and returned to GHC.

@since 0.2.3.2
-}
wrapParsedResultAction ::
  ([GHC.LHsDecl GHC.GhcPs] -> Either (SrcLoc.SrcSpan, String) ([GHC.LImportDecl GHC.GhcPs], [GHC.LHsDecl GHC.GhcPs])) ->
  [Plugins.CommandLineOption] ->
  ModSummary.ModSummary ->
  Plugins.ParsedResult ->
  HscTypes.Hsc Plugins.ParsedResult
wrapParsedResultAction processDecls _opts _modSummary parsedResult =
  let
    hpm = Plugins.parsedResultModule parsedResult
  in
    case GHC.hpm_module hpm of
      SrcLoc.L locMod hsModule ->
        let
          decls = getModuleDecls hsModule
        in
          case processDecls decls of
            Left (srcSpan, msg) ->
              let
                errMsg = mkPsError srcSpan msg

                oldMessages = Plugins.parsedResultMessages parsedResult
                newMessages =
                  oldMessages
                    { Plugins.psErrors = Plugins.psErrors oldMessages <> Error.singleMessage errMsg
                    }
              in
                pure
                  parsedResult
                    { Plugins.parsedResultMessages = newMessages
                    }
            Right (newImports, newDecls) ->
              let
                existingImports = getModuleImports hsModule
                newModule =
                  setModuleImports (existingImports ++ newImports)
                    . setModuleDecls newDecls
                    $ hsModule
                newHpm = hpm {GHC.hpm_module = SrcLoc.L locMod newModule}
              in
                pure
                  parsedResult
                    { Plugins.parsedResultModule = newHpm
                    }

mkPsError :: SrcLoc.SrcSpan -> String -> Error.MsgEnvelope PsErrors.PsMessage
mkPsError srcSpan msg =
  ErrUtils.mkPlainErrorMsgEnvelope
    srcSpan
    . PsErrors.PsUnknownMessage
#if MIN_VERSION_ghc(9,8,0)
    . Error.mkSimpleUnknownDiagnostic
#elif MIN_VERSION_ghc(9,6,0)
    . Error.UnknownDiagnostic
#endif
    $ Error.mkPlainError [] (Outputable.text msg)

{- | The 'NoSrcStrict' value, whose module location varies across GHC versions.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,6,0)
noSrcStrict :: SyntaxBasic.SrcStrictness
noSrcStrict = SyntaxBasic.NoSrcStrict
#else
noSrcStrict :: DataCon.SrcStrictness
noSrcStrict = DataCon.NoSrcStrict
#endif

{- | The 'NotPromoted' value, whose module location varies across GHC versions.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,6,0)
notPromoted :: Syntax.PromotionFlag
notPromoted = Syntax.NotPromoted

{- | The 'IsPromoted' value, whose module location varies across GHC versions.

@since 0.2.3.2
-}
isPromoted :: Syntax.PromotionFlag
isPromoted = Syntax.IsPromoted
#else
notPromoted :: BasicTypes.PromotionFlag
notPromoted = BasicTypes.NotPromoted

{- | The 'IsPromoted' value, whose module location varies across GHC versions.

@since 0.2.3.2
-}
isPromoted :: BasicTypes.PromotionFlag
isPromoted = BasicTypes.IsPromoted
#endif

{- | Extract the list of declarations from a parsed module, abstracting over the
  change in 'HsModule' parameterization in GHC 9.6.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,6,0)
getModuleDecls :: Syntax.HsModule GHC.GhcPs -> [GHC.LHsDecl GHC.GhcPs]
getModuleDecls = Syntax.hsmodDecls
#else
getModuleDecls :: GHC.HsModule -> [GHC.LHsDecl GHC.GhcPs]
getModuleDecls = GHC.hsmodDecls
#endif

{- | Set the declarations in a parsed module, abstracting over the change in 'HsModule'
  parameterization in GHC 9.6.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,6,0)
setModuleDecls :: [GHC.LHsDecl GHC.GhcPs] -> Syntax.HsModule GHC.GhcPs -> Syntax.HsModule GHC.GhcPs
setModuleDecls newDecls hsModule = hsModule {Syntax.hsmodDecls = newDecls}
#else
setModuleDecls :: [GHC.LHsDecl GHC.GhcPs] -> GHC.HsModule -> GHC.HsModule
setModuleDecls newDecls hsModule = hsModule {GHC.hsmodDecls = newDecls}
#endif

{- | Extract the import declarations from a parsed module.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,6,0)
getModuleImports :: Syntax.HsModule GHC.GhcPs -> [GHC.LImportDecl GHC.GhcPs]
getModuleImports = Syntax.hsmodImports
#else
getModuleImports :: GHC.HsModule -> [GHC.LImportDecl GHC.GhcPs]
getModuleImports = GHC.hsmodImports
#endif

{- | Set the import declarations in a parsed module.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,6,0)
setModuleImports :: [GHC.LImportDecl GHC.GhcPs] -> Syntax.HsModule GHC.GhcPs -> Syntax.HsModule GHC.GhcPs
setModuleImports newImports hsModule = hsModule {Syntax.hsmodImports = newImports}
#else
setModuleImports :: [GHC.LImportDecl GHC.GhcPs] -> GHC.HsModule -> GHC.HsModule
setModuleImports newImports hsModule = hsModule {GHC.hsmodImports = newImports}
#endif

{- | Create a qualified import declaration: @import qualified ModuleName as Qualifier@.

  The import is marked implicit ('ideclImplicit = True') so that GHC's unused-import
  checker does not flag it as redundant. GHC does not track usage from plugin-generated
  code when determining whether an import is used, so without this flag the injected
  imports would always be reported as redundant despite being required.

@since 0.2.5.0
-}
mkQualifiedImportDecl :: String -> String -> GHC.LImportDecl GHC.GhcPs
mkQualifiedImportDecl modName qualifier =
  mkLocated
#if MIN_VERSION_ghc(9,6,0)
      Syntax.ImportDecl
        { Syntax.ideclExt =
            GHC.XImportDeclPass
              { GHC.ideclAnn = Ann.noAnn
              , GHC.ideclSourceText = SourceText.NoSourceText
              , GHC.ideclImplicit = True
              }
        , Syntax.ideclName = mkLocated (Module.mkModuleName modName)
        , Syntax.ideclPkgQual = PkgQual.NoRawPkgQual
        , Syntax.ideclSource = Syntax.NotBoot
        , Syntax.ideclSafe = False
        , Syntax.ideclQualified = Syntax.QualifiedPre
        , Syntax.ideclAs = Just (mkLocated (Module.mkModuleName qualifier))
        , Syntax.ideclImportList = Nothing
        }
#else
      GHC.ImportDecl
        { GHC.ideclExt = Ann.noAnn
        , GHC.ideclSourceSrc = SourceText.NoSourceText
        , GHC.ideclName = mkLocated (Module.mkModuleName modName)
        , GHC.ideclPkgQual = PkgQual.NoRawPkgQual
        , GHC.ideclSource = UnitTypes.NotBoot
        , GHC.ideclSafe = False
        , GHC.ideclQualified = GHC.QualifiedPre
        , GHC.ideclImplicit = True
        , GHC.ideclAs = Just (mkLocated (Module.mkModuleName qualifier))
        , GHC.ideclHiding = Nothing
        }
#endif

{- | Extract the list of constructors from an 'HsDataDefn', abstracting over the
  'DataDefnCons' wrapper added in GHC 9.6.

@since 0.2.3.2
-}
getDataDefnCons :: Syntax.HsDataDefn GHC.GhcPs -> [GHC.LConDecl GHC.GhcPs]
getDataDefnCons dataDefn =
#if MIN_VERSION_ghc(9,6,0)
  case Syntax.dd_cons dataDefn of
    Syntax.NewTypeCon con -> [con]
    Syntax.DataTypeCons _ consList -> consList
#else
  Syntax.dd_cons dataDefn
#endif

{- | Extract the first constructor name from a GADT constructor declaration, abstracting
  over the change from a list to 'NonEmpty' in GHC 9.6.

@since 0.2.3.2
-}
getGADTConName :: Syntax.ConDecl GHC.GhcPs -> String
getGADTConName conDecl =
  case conDecl of
    Syntax.ConDeclGADT {Syntax.con_names = names} ->
#if MIN_VERSION_ghc(9,6,0)
      case NEL.head names of
#else
      case head names of
#endif
        SrcLoc.L _ firstName ->
          OccName.occNameString (RdrName.rdrNameOcc firstName)
    Syntax.ConDeclH98 {Syntax.con_name = SrcLoc.L _ name} ->
      OccName.occNameString (RdrName.rdrNameOcc name)

{- | Construct an 'HsType' for a string type literal, abstracting over the
  change in 'HsTyLit' parameterization in GHC 9.6.

@since 0.2.3.2
-}
mkHsTyLit :: String -> GHC.HsType GHC.GhcPs
mkHsTyLit str =
  let
#if MIN_VERSION_ghc(9,6,0)
    tyLit :: Syntax.HsTyLit GHC.GhcPs
#else
    tyLit :: Syntax.HsTyLit
#endif
    tyLit = Syntax.HsStrTy SourceText.NoSourceText (FS.mkFastString str)
  in
    Syntax.HsTyLit GHC.noExtField tyLit

{- | Construct a 'FunBind', abstracting over the extra @fun_tick@ field present in GHC 9.4.

@since 0.2.3.2
-}
mkFunBind ::
  GHC.LIdP GHC.GhcPs ->
  Syntax.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) ->
  GHC.HsBind GHC.GhcPs
mkFunBind funId matchGroup =
  Syntax.FunBind
    { Syntax.fun_ext = GHC.noExtField
    , Syntax.fun_id = funId
    , Syntax.fun_matches = matchGroup
#if !MIN_VERSION_ghc(9,6,0)
    , Syntax.fun_tick = []
#endif
    }

{- | Construct an 'HsOpTy' node, abstracting over the 'PromotionFlag' field added in GHC 9.4.

@since 0.2.3.2
-}
mkHsOpTy ::
  GHC.LHsType GHC.GhcPs ->
  GHC.LIdP GHC.GhcPs ->
  GHC.LHsType GHC.GhcPs ->
  GHC.HsType GHC.GhcPs
mkHsOpTy lhs op rhs =
  Syntax.HsOpTy Ann.noAnn notPromoted lhs op rhs

{- | Construct a guarded right-hand side.

@since 0.2.3.2
-}
mkGRHS :: GHC.LHsExpr GHC.GhcPs -> Syntax.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
mkGRHS body =
  mkLocated (Syntax.GRHS Ann.noAnn [] body)

{- | Construct an 'HsPar' expression, abstracting over the token fields added in GHC 9.4
  and removed again in GHC 9.10.

@since 0.2.4.0
-}
mkHsPar :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
#if MIN_VERSION_ghc(9,10,0)
mkHsPar expr =
  mkLocated (Syntax.HsPar Ann.noAnn expr)
#else
mkHsPar expr =
  mkLocated (Syntax.HsPar Ann.noAnn (SrcLoc.L Ann.NoTokenLoc GHC.HsTok) expr (SrcLoc.L Ann.NoTokenLoc GHC.HsTok))
#endif

{- | Construct an 'HsCase' expression, abstracting over the token field added in GHC 9.4
  and removed again in GHC 9.10.

@since 0.2.4.0
-}
mkHsCase ::
  GHC.LHsExpr GHC.GhcPs ->
  Syntax.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) ->
  GHC.LHsExpr GHC.GhcPs
#if MIN_VERSION_ghc(9,10,0)
mkHsCase scrutinee mg =
  mkLocated (Syntax.HsCase Ann.noAnn scrutinee mg)
#else
mkHsCase scrutinee mg =
  mkLocated (Syntax.HsCase Ann.noAnn scrutinee mg)
#endif

{- | Construct family equation patterns, abstracting over the difference between
  GHC versions (some use @[LHsTypeArg]@, others use @[LHsType]@).

@since 0.2.4.0
-}
#if MIN_VERSION_ghc(9,10,0)
mkFamEqnPats :: [GHC.LHsType GHC.GhcPs] -> Syntax.HsFamEqnPats GHC.GhcPs
mkFamEqnPats tys =
  fmap (\ty -> Syntax.HsValArg GHC.noExtField ty) tys
#elif MIN_VERSION_ghc(9,8,0)
mkFamEqnPats :: [GHC.LHsType GHC.GhcPs] -> Syntax.HsTyPats GHC.GhcPs
mkFamEqnPats tys =
  fmap Syntax.HsValArg tys
#else
mkFamEqnPats :: [GHC.LHsType GHC.GhcPs] -> [GHC.LHsTypeArg GHC.GhcPs]
mkFamEqnPats tys =
  fmap Syntax.HsValArg tys
#endif

{- | An empty annotation value suitable for extension fields that are @[AddEpAnn]@ on GHC 9.10+
  and @EpAnn [AddEpAnn]@ on earlier versions.

@since 0.2.4.0
-}
#if MIN_VERSION_ghc(9,10,0)
noAnnEpAnn :: [Ann.AddEpAnn]
noAnnEpAnn = []
#else
noAnnEpAnn :: Ann.EpAnn [Ann.AddEpAnn]
noAnnEpAnn = Ann.noAnn
#endif

{- | Construct the extension field for 'ClsInstDecl', which differs across GHC versions.

@since 0.2.4.0
-}
mkClsInstDeclExt :: Syntax.XCClsInstDecl GHC.GhcPs
#if MIN_VERSION_ghc(9,10,0)
mkClsInstDeclExt =
  (Nothing, [], Ann.NoAnnSortKey)
#else
mkClsInstDeclExt =
  (Ann.noAnn, Ann.NoAnnSortKey)
#endif

{- | Construct a signature pattern @(pat :: ty)@.

@since 0.2.4.0
-}
mkSigPat :: GHC.LPat GHC.GhcPs -> GHC.LHsType GHC.GhcPs -> GHC.Pat GHC.GhcPs
mkSigPat pat ty =
  let
    patSigType =
      Syntax.HsPS
        { Syntax.hsps_ext = Ann.noAnn
        , Syntax.hsps_body = ty
        }
  in
#if MIN_VERSION_ghc(9,10,0)
    Syntax.SigPat [] pat patSigType
#else
    Syntax.SigPat Ann.noAnn pat patSigType
#endif
