{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright : Flipstone Technology Partners 2025
License   : BSD3
-}
module Shrubbery.TH
  ( deriveMatchable
  ) where

import qualified Language.Haskell.TH as TH

{- | Template Haskell function to derive Matchable for a given type
Usage: $(deriveMatchable ''FooBarBaz)
-}
deriveMatchable :: TH.Name -> TH.Q [TH.Dec]
deriveMatchable typeName = do
  let
    failIfMissing :: (String -> TH.Q (Maybe a)) -> String -> TH.Q a
    failIfMissing f n = do
      mbA <- f n
      case mbA of
        Nothing -> fail ("deriveMatchable: " <> n <> " could not be found in scope")
        Just a -> pure a

  caseConsName <- failIfMissing TH.lookupValueName "Case"
  casesName <- failIfMissing TH.lookupTypeName "Cases"
  matchableName <- failIfMissing TH.lookupTypeName "Matchable"

  reified <- TH.reify typeName

  case reified of
    TH.TyConI (TH.DataD _ _ _ _ cons _) -> do
      mName <- TH.newName "m"
      vName <- TH.newName "v"
      let
        caseType :: TH.Con -> TH.Q TH.Type
        caseType con = case con of
          TH.NormalC conName [(_, _conType)] ->
            pure $ TH.AppT (TH.AppT (TH.PromotedT caseConsName) (TH.LitT (TH.StrTyLit (TH.nameBase conName)))) _conType
          TH.NormalC conName [] ->
            pure $ TH.AppT (TH.AppT (TH.PromotedT caseConsName) (TH.LitT (TH.StrTyLit (TH.nameBase conName)))) (TH.ConT ''())
          _ -> fail "deriveMatchable: Unsupported constructor type."

      casesList <- mapM caseType cons

      let
        casesType = foldr (TH.AppT . TH.AppT TH.PromotedConsT) TH.PromotedNilT casesList
        casesTypeFam = TH.TySynInstD $ TH.TySynEqn Nothing (TH.AppT (TH.ConT casesName) (TH.ConT typeName)) casesType

        mkMatchDoClause :: TH.Con -> TH.Q TH.Match
        mkMatchDoClause con = case con of
          TH.NormalC conName [(_, _conType)] -> do
            xName <- TH.newName "x"
            pat <- TH.conP conName [TH.varP xName]
            let
              typ = TH.LitT (TH.StrTyLit (TH.nameBase conName))
              body =
                TH.AppE
                  ( TH.AppE
                      (TH.AppTypeE (TH.VarE (TH.mkName "matchCall")) typ)
                      (TH.VarE mName)
                  )
                  (TH.VarE xName)
            pure (TH.Match pat (TH.NormalB body) [])
          TH.NormalC conName [] -> do
            pat <- TH.conP conName []
            let
              typ = TH.LitT (TH.StrTyLit (TH.nameBase conName))
              body =
                TH.AppE
                  ( TH.AppE
                      (TH.AppTypeE (TH.VarE (TH.mkName "matchCall")) typ)
                      (TH.VarE mName)
                  )
                  (TH.ConE '())
            pure (TH.Match pat (TH.NormalB body) [])
          _ -> fail "deriveMatchable: Unspported data constructor"
      let
        matches = map mkMatchDoClause cons
      matchDoBody <-
        TH.lamE
          [TH.varP mName, TH.varP vName]
          (TH.caseE (TH.varE vName) matches)
      let
        matchDoDec = TH.FunD (TH.mkName "matchDo") [TH.Clause [] (TH.NormalB matchDoBody) []]
        matchableInst = TH.InstanceD Nothing [] (TH.AppT (TH.ConT matchableName) (TH.ConT typeName)) [casesTypeFam, matchDoDec]
      pure [matchableInst]
    _ -> fail "deriveMatchable: Expected data type."
