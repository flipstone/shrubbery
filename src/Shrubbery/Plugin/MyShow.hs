{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Shrubbery.Plugin.MyShow (plugin) where

import Control.Monad
-- import GHC.Builtin.Names
import GHC.Core
import GHC.Core.FamInstEnv
-- import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Core.TyCo.Compare
import GHC.Core.TyCo.Rep
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence

--------------------------------------------------------------------------------
-- Plugin Entry
--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const (Just myTcPlugin)
  , pluginRecompile = purePlugin
  }

myTcPlugin :: TcPlugin
myTcPlugin = TcPlugin
  { tcPluginInit  = pure ()
  , tcPluginSolve = solveDissectSum
  , tcPluginRewrite = const mempty
  , tcPluginStop  = const (pure ())
  }

--------------------------------------------------------------------------------
-- Constraint Solving
--------------------------------------------------------------------------------

solveDissectSum :: () -> TcPluginSolver
solveDissectSum _ _evBindsVar _givens wanteds = do
  tcPluginIO $ putStrLn "Solving"
  solved <- mapM solveOne wanteds
  let oks = [(ev, ct) | Just (ev,ct) <- solved]
  pure (TcPluginOk oks [])
  -- Only care about wanted constraints
  -- solutions <- mapM solveOne (mapMaybe isWantedCT wanteds)
  -- pure (TcPluginOk (concatMap fst solutions) (concatMap snd solutions))


-- Try solving a single wanted constraint
solveOne :: Ct -> TcPluginM (Maybe (EvTerm, Ct))
solveOne ct = case classifyPredType (ctPred ct) of
  ClassPred cls [sumTy, taggedTy, resTy]
    | occNameString (getOccName cls) == "DissectSum" -> do
        tcPluginIO $ putStrLn "DissectSum found"
        normalizedTaggedTy <- normalize taggedTy
        errOrTags <- pure (extractTags normalizedTaggedTy)
        case errOrTags of
          Left err -> do
            tcPluginIO $ putStrLn ("No Tags found: " ++ err)
            fail "No Tags found"
          Right tags -> do
            -- Validate constructors and fields here
            ok <- validateTagsAndTypes sumTy tags
            if ok
              then do
                ev <- buildEvidence sumTy taggedTy resTy tags
                pure (Just (ev, ct))
              else do
                tcPluginIO $ putStrLn "Validation failed"
                fail "Validation failed"
    
  _ ->  pure Nothing

--------------------------------------------------------------------------------
-- Extract tag info from type-level list
--------------------------------------------------------------------------------

type TagInfo = (OccName, Type)

normalize :: Type -> TcPluginM Type
normalize ty = do
  famTyEnv <- getFamInstEnvs
  let normedTy = topNormaliseType famTyEnv ty
  pure normedTy

extractTags :: Type -> Either String [TagInfo]
extractTags = go
  where
    go ty = case splitTyConApp_maybe ty of
      -- End of list
      Just (tc, [_kind]) | tc == promotedNilDataCon -> Right []

      -- Cons case with invisible kind constructor
      Just (tc, [_kind, headTy, tailTy]) | tc == promotedConsDataCon -> do
        (occ, payload) <- extractTag headTy
        rest <- go tailTy
        pure ((occ, payload) : rest)

      Just (tc, tys) ->
        Left ("Not a type list: " ++ getOccString tc ++ " " ++ show (length tys))
        
      Nothing ->
        Left "Unable to split ty con app"

    -- One element like ("A" @= Int)
    extractTag ty = case splitTyConApp_maybe ty of
      Just (tc, [symTy, payloadTy])
        | occNameString (getOccName tc) == "Tag" -> do
            sym <- getSymbolFromType symTy
            pure (mkVarOcc sym, payloadTy)
      _ -> Left "Not a Tag type"

    -- Pull a Symbol out of a type literal
    getSymbolFromType = \case
      LitTy (StrTyLit fs) -> Right (unpackFS fs)
      _ -> Left "Not a symbol type"

--------------------------------------------------------------------------------
-- Validation (stub for now)
--------------------------------------------------------------------------------

validateTagsAndTypes :: Type -> [TagInfo] -> TcPluginM Bool
validateTagsAndTypes sumTy tags = do
  -- Expand any type synonyms first
  -- env <- getTopEnv
  -- let famTyEnv = hsc_FC env
  -- let normalised = snd (normaliseType famTyEnv Nominal sumTy)

  case splitTyConApp_maybe sumTy {- normalised -} of
    Nothing -> do
      -- tcPluginTrace "validateTagsAndTypes" ("Not a TyConApp: " ++ showSDocUnsafe (ppr sumTy))
      pure False
    Just (tycon, args) -> do
      case algTyConRhs tycon of
        DataTyCon { data_cons = cons } -> do
          let expected = map fst tags
          let actual   = map (mkVarOcc . occNameString . occName . dataConName) cons
          if expected /= actual
            then do
              -- tcPluginTrace "validateTagsAndTypes"
              --   ("Constructor name mismatch.\nExpected: " ++ show expected ++
              --    "\nActual:   " ++ show actual)
              pure False
            else do
              -- Check argument types match
              allOk <- and <$> zipWithM (checkConstructor args) cons tags
              pure allOk
        _ -> do
          -- tcPluginTrace "validateTagsAndTypes" ("Not a data tycon: " ++ showSDocUnsafe (ppr tycon))
          pure False
  where
    checkConstructor :: [Type] -> DataCon -> TagInfo -> TcPluginM Bool
    checkConstructor args con (_occ, expectedTy) = do
      let conTy   = map scaledThing (dataConOrigArgTys con)
          -- instantiate type variables of constructor with args
          instTy  = substTyWith (tyConTyVars (dataConTyCon con)) args <$> conTy
      if length instTy == 1
        then do
          let actualTy = head instTy
          let ok = eqType actualTy expectedTy
          -- unless ok $
          --   tcPluginTrace "validateTagsAndTypes"
          --     ("Type mismatch in constructor " ++ showSDocUnsafe (ppr con) ++
          --      "\nExpected: " ++ showSDocUnsafe (ppr expectedTy) ++
          --      "\nActual:   " ++ showSDocUnsafe (ppr actualTy))
          pure ok
        else do
          -- tcPluginTrace "validateTagsAndTypes"
          --   ("Constructor " ++ showSDocUnsafe (ppr con) ++ " has multiple fields, not yet handled")
          pure False


--------------------------------------------------------------------------------
-- Evidence construction (stub for now)
--------------------------------------------------------------------------------

buildEvidence :: Type -> Type -> Type -> [TagInfo] -> TcPluginM EvTerm
buildEvidence sumTy taggedTy resTy tags = do
  -- Get the TyCon of the sum
  case splitTyConApp_maybe sumTy of
    Just (tycon, _tyArgs) -> do
      let cons = tyConDataCons tycon

      -- Variables for the two lambda args
      xVar       <- freshLocal "x" sumTy
      xBndr       <- freshLocal "x_bindr" sumTy
      taggedBranchesTyCon  <- getTaggedBranchesTyCon
      branchesVar <- freshLocal "branches" (mkTyConApp taggedBranchesTyCon [taggedTy, resTy])

      -- Build alternatives
      alts <- zipWithM (mkAlt branchesVar) (zip cons [0..]) tags

      let caseExpr = Case (Var xVar) xBndr sumTy alts
          lamExpr  = Lam branchesVar (Lam xVar caseExpr)
      pure (EvExpr lamExpr)

    _ -> fail "buildEvidence: sumTy was not a TyConApp"
  where
    -- Generate one alternative for each constructor/tag
    mkAlt :: CoreBndr -> (DataCon, Integer) -> TagInfo -> TcPluginM CoreAlt
    mkAlt branchesVar (con, index) (occ, _) = do
      -- bind args as dummies for now
      let argTys = map scaledThing (dataConOrigArgTys con)
      argVars <- zipWithM (\i ty -> freshLocal ("arg" ++ show i) ty) [(0::Int)..] argTys

      let symTy = LitTy (StrTyLit (occNameFS occ))

      dummyDictVar <- freshLocal "dummyDict" sumTy

      let dummyDict = Lam dummyDictVar (Var dummyDictVar)

      -- Build: selectTaggedBranchAtProxy @"CName" branches Proxy (tuple args)
      proxyTyCon <- getProxyTyCon
      proxyExpr <- mkProxyExpr occ
      let payload = mkTupleExpr argVars
      selectId <- getSelectTaggedBranchId
      let rhs = mkCoreApps (Var selectId)
                  [ Type symTy
                  , Type (head argTys)
                  , Type taggedTy
                  , Type (LitTy (NumTyLit index))
                  , Type (mkTyConTy proxyTyCon)
                  , Type resTy
                  , dummyDict
                  , dummyDict
                  , dummyDict
                  , dummyDict
                  , Var branchesVar
                  , proxyExpr
                  , payload
                  ]

      pure (Alt (DataAlt con) argVars rhs)

    ----------------------------------------------------------------
    -- helpers
    ----------------------------------------------------------------
    freshLocal nm ty = do
      u <- newUnique
      pure (mkLocalId (mkSystemName u (mkVarOcc nm)) manyDataConTy ty)

    mkTupleExpr []  = mkCoreConApps unitDataCon []
    mkTupleExpr [v] = Var v
    mkTupleExpr vs  = mkCoreTup (map Var vs)

      
mkProxyExpr :: OccName -> TcPluginM CoreExpr
mkProxyExpr occ = do
  let symTy = LitTy (StrTyLit (occNameFS occ))   -- "Foo" :: Symbol
  symTyCon <- getSymbolTyCon
  proxyDataCon <- getProxyDataCon
  pure $ mkTyApps (Var proxyDataCon) [mkTyConTy symTyCon, symTy]

getSelectTaggedBranchId :: TcPluginM Id
getSelectTaggedBranchId = do
  let modName = mkModuleName "Shrubbery.TaggedUnion"
  found <-findImportedModule modName NoPkgQual
  mod_ <- case found of
    Found _ m -> pure m
    _ -> fail "Could not find module Shrubbery.TaggedUnion"

  let occ = mkVarOcc "selectTaggedBranchAtProxy"
  tcLookupId =<< lookupOrig mod_ occ

getTaggedBranchesTyCon :: TcPluginM TyCon
getTaggedBranchesTyCon = do
  let modName = mkModuleName "Shrubbery.TaggedUnion"  -- adjust as needed
  found <- findImportedModule modName NoPkgQual
  mod_ <- case found of
    Found _ m -> pure m
    _ -> fail "Could not find module Shrubbery.TaggedUnion"

  let occ = mkTcOcc "TaggedBranches"
  tcLookupTyCon =<< lookupOrig mod_ occ

getProxyDataCon :: TcPluginM Id
getProxyDataCon = do
  let modName = mkModuleName "Data.Proxy"  -- adjust as needed
  found <- findImportedModule modName NoPkgQual
  mod_ <- case found of
    Found _ m -> pure m
    _ -> fail "Could not find module Data.Proxy"

  let occ = mkVarOcc "Proxy"
  tcLookupId =<< lookupOrig mod_ occ

getProxyTyCon :: TcPluginM TyCon
getProxyTyCon = do
  let modName = mkModuleName "Data.Proxy"  -- adjust as needed
  found <- findImportedModule modName NoPkgQual
  mod_ <- case found of
    Found _ m -> pure m
    _ -> fail "Could not find module Data.Proxy"

  let occ = mkTcOcc "Proxy"
  tcLookupTyCon =<< lookupOrig mod_ occ

getSymbolTyCon :: TcPluginM TyCon
getSymbolTyCon = do
  let modName = mkModuleName "GHC.Types"  -- adjust as needed
  found <- findImportedModule modName NoPkgQual
  mod_ <- case found of
    Found _ m -> pure m
    _ -> fail "Could not find module GHC.Types"

  let occ = mkTcOcc "Symbol"
  tcLookupTyCon =<< lookupOrig mod_ occ
