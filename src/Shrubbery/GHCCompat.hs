{- FOURMOLU_DISABLE -}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

Compatibility shim for GHC API differences across versions. This module isolates all CPP
conditionals so that the rest of the plugin code can be version-agnostic.

Rather than constructing the generated instance as GHC AST nodes by hand (which requires
tracking the AST's shape across every GHC release), the plugin renders the instance as Haskell
/source text/ and feeds it to GHC's own parser via 'parseInstanceDecls'. The parser produces
whatever AST shape the running compiler expects, so this module only has to abstract over a small,
stable surface: invoking the parser, reading data declarations, and splicing decls/imports into
the parsed module.

@since 0.2.3.2
-}
module Shrubbery.GHCCompat
  ( parseInstanceDecls
  , showHsType
  , getModuleDecls
  , setModuleDecls
  , getModuleImports
  , setModuleImports
  , mkQualifiedImportDecl
  , getDataDefnCons
  , getGADTConName
  , wrapParsedResultAction
  ) where

#if !MIN_VERSION_ghc(9,6,0)
import GHC.Unit.Types qualified as UnitTypes
#endif

import GHC.Data.FastString qualified as FS
import GHC.Data.StringBuffer qualified as StringBuffer
import GHC.Driver.Config.Parser qualified as ParserConfig
import GHC.Driver.Plugins qualified as Plugins
import GHC.Driver.Session qualified as Session
import GHC.Hs qualified as GHC
import GHC.LanguageExtensions qualified as LangExt
import GHC.Parser qualified as Parser
import GHC.Parser.Annotation qualified as Ann
import GHC.Parser.Lexer qualified as Lexer
import GHC.Types.Name.Occurrence qualified as OccName
import GHC.Types.Name.Reader qualified as RdrName
import GHC.Types.SrcLoc qualified as SrcLoc
import GHC.Types.SourceText qualified as SourceText
import GHC.Utils.Outputable qualified as Outputable
import Language.Haskell.Syntax qualified as Syntax

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
#endif

{- | Parse a fragment of Haskell module source into a list of declarations using GHC's own
  parser. The source is expected to be a complete module (with a header), as produced by the
  plugin's instance renderer; only its declarations are returned.

  The extensions required by the generated code ('TypeApplications', 'DataKinds', etc.) are
  force-enabled in the parser options so that parsing does not depend on which extensions the
  user's module happens to have turned on. The user's module must still enable whatever is needed
  for the spliced instance to /type-check/, but that was already true before this plugin parsed
  anything.

@since 0.2.5.0
-}
parseInstanceDecls :: Session.DynFlags -> String -> Either String [GHC.LHsDecl GHC.GhcPs]
parseInstanceDecls dflags src =
  let
    parserOpts = ParserConfig.initParserOpts (enableGeneratedExtensions dflags)
    buffer = StringBuffer.stringToStringBuffer src
    startLoc = SrcLoc.mkRealSrcLoc (FS.mkFastString "<shrubbery-plugin-generated>") 1 1
    parseState = Lexer.initParserState parserOpts buffer startLoc
  in
    case Lexer.unP Parser.parseModule parseState of
      Lexer.POk _ (SrcLoc.L _ hsModule) ->
        Right (getModuleDecls hsModule)
      Lexer.PFailed _ ->
        Left "Shrubbery.Plugin: internal error - failed to parse generated instance source"

{- | The language extensions the generated instance source relies on, force-enabled for parsing.

@since 0.2.5.0
-}
enableGeneratedExtensions :: Session.DynFlags -> Session.DynFlags
enableGeneratedExtensions dflags =
  foldl
    Session.xopt_set
    dflags
    [ LangExt.TypeApplications
    , LangExt.DataKinds
    , LangExt.TypeFamilies
    , LangExt.TypeOperators
    , LangExt.ScopedTypeVariables
    , LangExt.FlexibleContexts
    ]

{- | Render a parsed type back to source text using GHC's pretty-printer. Used to reproduce
  user-written field types and instance heads inside the generated instance source.

@since 0.2.5.0
-}
showHsType :: GHC.LHsType GHC.GhcPs -> String
showHsType =
  Outputable.showPprUnsafe . SrcLoc.unLoc

{- | Wrap a declaration-processing function into a full 'parsedResultAction' handler.

  The callback receives the module's 'Session.DynFlags' (needed to parse generated source) and
  declarations, and returns a list of import declarations to add and the new list of declarations.
  Errors are added to 'PsMessages' and returned to GHC.

@since 0.2.3.2
-}
wrapParsedResultAction ::
  (Session.DynFlags -> [GHC.LHsDecl GHC.GhcPs] -> Either (SrcLoc.SrcSpan, String) ([GHC.LImportDecl GHC.GhcPs], [GHC.LHsDecl GHC.GhcPs])) ->
  [Plugins.CommandLineOption] ->
  ModSummary.ModSummary ->
  Plugins.ParsedResult ->
  HscTypes.Hsc Plugins.ParsedResult
wrapParsedResultAction processDecls _opts modSummary parsedResult =
  let
    dflags = ModSummary.ms_hspp_opts modSummary
    hpm = Plugins.parsedResultModule parsedResult
  in
    case GHC.hpm_module hpm of
      SrcLoc.L locMod hsModule ->
        let
          decls = getModuleDecls hsModule
        in
          case processDecls dflags decls of
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

{- | Wrap a value with an empty source location annotation. Used internally to build the injected
  qualified imports.

@since 0.2.3.2
-}
#if MIN_VERSION_ghc(9,10,0)
mkLocated :: Ann.HasAnnotation ann => a -> SrcLoc.GenLocated ann a
mkLocated = SrcLoc.L Ann.noSrcSpanA
#else
mkLocated :: a -> SrcLoc.GenLocated (Ann.SrcAnn ann) a
mkLocated = SrcLoc.L Ann.noSrcSpanA
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
  imports would always be reported as redundant despite being required. Because this flag
  cannot be expressed in source syntax, the injected imports must be built as AST nodes
  rather than rendered as text and parsed.

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
