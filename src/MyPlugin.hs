module MyPlugin (plugin) where

import Control.Monad
import GHC.Builtin.Names
import GHC.Core
import GHC.Core.Class
import GHC.Core.Predicate
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence

-- Entry point
plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const (Just myTcPlugin)
  , pluginRecompile = purePlugin
  }

myTcPlugin :: TcPlugin
myTcPlugin = TcPlugin
  { tcPluginInit  = pure ()
  , tcPluginSolve = solveMyShow
  , tcPluginRewrite = const mempty
  , tcPluginStop  = const (pure ())
  }

-- Solver
solveMyShow :: () -> TcPluginSolver
solveMyShow _ _evBindsVar _givens wanteds = do
  solved <- mapM solveOne wanteds
  let oks = [(ev, ct) | Just (ev,ct) <- solved]
  pure (TcPluginOk oks [])

-- Try to solve one wanted
solveOne :: Ct -> TcPluginM (Maybe (EvTerm, Ct))
solveOne ct =
  case classifyPredType (ctPred ct) of
    ClassPred cls [ty]
      | occNameString (getOccName cls) == "MyShow" -> do
          dict <- mkDictForMyShow cls ty
          pure (Just (EvExpr dict, ct))
    _ -> pure Nothing

------------------------------------------------------------
-- Building the dictionary
------------------------------------------------------------

-- Construct a dictionary CoreExpr for (MyShow ty)
mkDictForMyShow :: Class -> Type -> TcPluginM CoreExpr
mkDictForMyShow cls ty = do
  methodExpr <- mkMethodExprCase ty
  let dc = classDataCon cls
  -- For single-method class: DataCon :: forall a. (a -> String) -> MyShow a
  pure (mkCoreConApps dc [Type ty, methodExpr])

myShowModName :: ModuleName
myShowModName = mkModuleName "MyShow"

findConstructorNameId :: TcPluginM Id
findConstructorNameId = do
  findResult <- findImportedModule myShowModName NoPkgQual
  case findResult of
    Found _loc mod_ -> do
      let costructorNameOccName = mkVarOcc "constructorName"
      name <- lookupOrig mod_ costructorNameOccName
      tcLookupId name
    _ -> fail "MyShow module or constructorName not found"

mkMethodExprCase :: Type -> TcPluginM CoreExpr
mkMethodExprCase ty = do
  -- lookup unpackCString# :: Addr# -> [Char]
  unpackId <- tcLookupId =<< lookupOrig gHC_CSTRING (mkVarOcc "unpackCString#")

  constructorNameId <- findConstructorNameId

  -- lambda argument: x :: ty
  x_uniq <- newUnique
  let x_name = mkInternalName x_uniq (mkVarOcc "x") noSrcSpan
      x_id   = mkLocalIdOrCoVar x_name manyDataConTy ty

  case splitTyConApp_maybe ty of
    Nothing ->
      fail ("Not a data type: " ++ showSDocUnsafe (ppr ty))

    Just (tc, _tcArgs) -> do
      let dcs = tyConDataCons tc

          mkAlt :: DataCon -> TcPluginM CoreAlt
          mkAlt dc = do
            -- create dummy binders for constructor fields
            bs <- forM [1 .. dataConSourceArity dc] $ \i -> do
              u <- newUnique
              let nm = mkInternalName u (mkVarOcc ("f" ++ show i)) noSrcSpan
              pure (mkLocalIdOrCoVar nm manyDataConTy liftedTypeKind) -- dummy type is OK if ignored
            -- RHS: unpackCString# "CtorName"
            let 
              rhs :: CoreExpr
              rhs =
                mkApps
                  (Var constructorNameId)
                  [App (Var unpackId) (Lit (mkLitString (occNameString (getOccName dc))))]

            pure (Alt (DataAlt dc) bs rhs)

      alts <- mapM mkAlt dcs

      let caseExpr = Case (Var x_id) x_id stringTy alts
      pure (Lam x_id caseExpr)

