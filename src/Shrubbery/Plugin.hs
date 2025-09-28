{- |
Copyright : Flipstone Technology Partners 2025
License   : BSD3
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-unused-top-binds -Wno-unused-imports #-}
module Shrubbery.Plugin (
    plugin, DeriveMatchable(..)
) where


import Data.Data (Data)
import GHC.Plugins (Plugin(..), defaultPlugin, CoreM, ModGuts(..), CoreToDo(..), TyCon, DataCon, mkSysLocalM, fsLit, tyConDataCons, Type, mkTyConApp, Alt(..), AltCon(..), Boxity(..), mkStrLitTy, manyDataConTy, Id, AnnTarget(NamedTarget), deserializeWithData, findAnns, mkAnnEnv, tyConName, mkTcOcc, ModuleName, FastString, Module, OccName, Name)
import GHC.Core.Make (mkCoreTup, mkCoreApps)
import GHC.Core.Multiplicity (scaledMult, scaledThing)
import GHC.Core (CoreBind, Bind(..), Expr(..))
import GHC.Types.Name (getOccString, getOccFS)
import GHC.Types.TyThing (lookupTyCon)
import GHC.Core.DataCon (dataConOrigArgTys, dataConName)
import GHC.Builtin.Types (unitTyCon, tupleTyCon)
import GHC.Builtin.Names (mkUnboundName)
import Control.Monad (zipWithM, foldM)
import Control.Monad.IO.Class (liftIO)
import GHC.Tc.Plugin (FindResult(..), lookupOrig)
import GHC.Core.Opt.Monad (getHscEnv)
import GHC.Unit.Finder (findImportedModule)
import GHC (moduleNameString)

-- | @since 1
data DeriveMatchable =
  DeriveMatchable
    deriving Data

-- | @since 1
plugin :: Plugin
plugin = 
  defaultPlugin { installCoreToDos = const myCorePlugin }

myCorePlugin :: [CoreToDo] -> CoreM [CoreToDo]
myCorePlugin todos = do
  let myPass = CoreDoPluginPass "DeriveMatchable" deriveMatchablePass
  pure (myPass : todos)

-- | Core pass to generate Matchable instances for annotated types
deriveMatchablePass :: ModGuts -> CoreM ModGuts
deriveMatchablePass guts = do
  let
    allTyCons = mg_tcs guts
    annotatedTyCons = filter (hasDeriveMatchableAnnotation guts) allTyCons

  mapM_ (liftIO . print . getOccString . tyConName) allTyCons

  foldM generateMatchableInstance guts annotatedTyCons

hasDeriveMatchableAnnotation :: ModGuts -> TyCon -> Bool
hasDeriveMatchableAnnotation guts tyCon =
  let
    annEnv =
      mkAnnEnv (mg_anns guts)

    annTarget =
      NamedTarget (tyConName tyCon)
      
    anns :: [DeriveMatchable]
    anns =
      findAnns deserializeWithData annEnv annTarget
  in
    not . null $ anns


-- | Generate and inject a Matchable instance for a TyCon
-- This is a stub; actual implementation will require constructing Core bindings
-- and updating ModGuts appropriately
generateMatchableInstance :: ModGuts -> TyCon -> CoreM ModGuts
generateMatchableInstance guts tc = do
  liftIO . print . getOccString . tyConName $ tc
  let cons = tyConDataCons tc
  caseTyCon <- lookupCaseTyCon
  let _caseTypeList = map (mkCaseType caseTyCon) cons
  matchDoBind <- mkMatchDoBind tc cons
  injectInstances guts matchDoBind

-- | Inject type family instance and class instance into ModGuts
injectInstances :: ModGuts -> CoreBind -> CoreM ModGuts
injectInstances guts matchableBind = do
  let newBinds = matchableBind : mg_binds guts
  pure guts { mg_binds = newBinds }

-- | Lookup the Case TyCon from Shrubbery.TypeLevel
lookupCaseTyCon :: CoreM TyCon
lookupCaseTyCon =
  let caseName = mkUnboundName (mkTcOcc "Case")
  in  lookupTyCon caseName

-- typeLevelModule :: ModuleName
-- typeLevelModule =
--   mkModuleNameFS (mkFastString "Shrubbery.TypeLevel")

-- | Construct a promoted Case type for a constructor
mkCaseType :: TyCon -> DataCon -> Type
mkCaseType caseTyCon con =
  let conNameStr = getOccFS (dataConName con)
      argTys = dataConOrigArgTys con
      -- Case "ConName" argType
      -- If no args, use ()
      caseArgTy = case argTys of
        [] -> mkTyConApp unitTyCon []
        [ty] -> scaledThing ty
        _ -> mkTyConApp (tupleTyCon Boxed (length argTys)) (map scaledThing argTys)
  in mkTyConApp caseTyCon [mkStrLitTy conNameStr, caseArgTy]

-- | Construct Core binding for matchDo
mkMatchDoBind :: TyCon -> [DataCon] -> CoreM CoreBind
mkMatchDoBind tc cons = do
  matchDoVar <- mkSysLocalM (fsLit "matchDo") manyDataConTy (mkTyConApp tc [])
  matchVar <- mkSysLocalM (fsLit "match") manyDataConTy (mkTyConApp tc [])
  matchCallVar <- mkSysLocalM (fsLit "matchCall") manyDataConTy (mkTyConApp tc [])
  valVar <- mkSysLocalM (fsLit "val") manyDataConTy (mkTyConApp tc [])

  let
    mkAlt :: DataCon -> CoreM (Alt Id)
    mkAlt con = do
        let _conNameStr = getOccString (dataConName con)
            argTys = dataConOrigArgTys con

        argVars <-
          zipWithM (\i ty -> mkSysLocalM (fsLit ("x" ++ show i)) (scaledMult ty) (scaledThing ty))
          [(1::Int)..]
          argTys

        let
            -- Handle zero, one, or multiple constructor arguments
            matchCallArg = case argVars of
              [] -> mkCoreTup [] -- unit value
              [v] -> Var v
              vs -> mkCoreTup (map Var vs)
            -- matchCall @conNameStr matchVar matchCallArg
            matchCallExpr =
              mkCoreApps
                (Var matchCallVar)
                [Var matchVar, matchCallArg]

        pure (Alt (DataAlt con) argVars matchCallExpr)

  alts <- traverse mkAlt cons
  let caseExpr = Case (Var valVar) matchVar (error "case type") alts
  pure $ NonRec matchDoVar (Lam valVar caseExpr)

lookupModule :: ModuleName -> Maybe FastString -> CoreM Module
lookupModule mod_nm pkg = do
  error "lookupModule"
    -- hsc_env <- getHscEnv
    -- found_module <- liftIO $ findImportedModule hsc_env mod_nm pkg
    -- case found_module of
    --   Found _ md -> return md
    --   _          -> error $ "Unable to resolve module looked up by plugin: " ++ moduleNameString mod_nm

lookupName :: Module -> OccName -> CoreM Name
lookupName md occ = do
  hsc_env <- getHscEnv
  liftIO $ error "lookupName" -- initTcForLookup hsc_env $ lookupOrig md occ
