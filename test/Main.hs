{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  ) where

import           Data.Proxy (Proxy(Proxy))
import           Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as HHM
import qualified Hedgehog.Range as Range
import           Text.Read (readMaybe)

import Shrubbery.Branches
import Shrubbery.Classes
import Shrubbery.Union
import Shrubbery.Parser

main :: IO ()
main =
  HHM.defaultMain
    [ HH.checkSequential $
        HH.Group
          "Shrubbery Tests"
          [ ("Four way branching on type works", prop_fourWayBranchingOnType)
          , ("Four way branching on index works", prop_fourWayBranchingOnIndex)
          , ("Four way union dissection", prop_fourWayUnionDissection)
          , ("Parser runs all options", prop_parserRunsAllOptions)
          ]
    ]

prop_fourWayBranchingOnType :: HH.Property
prop_fourWayBranchingOnType =
  let
    branches :: Branches [String, Int, Bool, (Bool, Bool)] String
    branches =
      branchBuild
      $ branch (indexResult 0)
      $ branch (indexResult 1)
      $ branch (indexResult 2)
      $ branch (indexResult 3)
      $ branchEnd

    inputGen :: HH.Gen (Either String (Either Int (Either Bool (Bool, Bool))))
    inputGen =
      Gen.choice
        [ Left                  <$> Gen.string (Range.constant 0 10) Gen.unicodeAll
        , Right . Left          <$> Gen.integral (Range.constant minBound maxBound)
        , Right . Right . Left  <$> Gen.bool
        , Right . Right . Right <$> ((,) <$> Gen.bool <*> Gen.bool)
        ]

    indexResult :: Show a => Int -> a -> String
    indexResult n a =
      "Index " ++ show n ++ ": " ++ show a
  in
    HH.property $ do
      input <- HH.forAll inputGen

      case input of
        Left string ->
          selectBranch branches string === indexResult 0 string

        Right (Left int) ->
          selectBranch branches int === indexResult 1 int

        Right (Right (Left bool)) ->
          selectBranch branches bool === indexResult 2 bool

        Right (Right (Right twoBools)) ->
          selectBranch branches twoBools === indexResult 3 twoBools

prop_fourWayBranchingOnIndex :: HH.Property
prop_fourWayBranchingOnIndex =
  let
    branches :: Branches [Bool, Bool, Bool, Bool] String
    branches =
      branchBuild
      $ branch (indexResult 0)
      $ branch (indexResult 1)
      $ branch (indexResult 2)
      $ branch (indexResult 3)
      $ branchEnd

    inputGen =
      Gen.choice
        [ Left                  <$> Gen.bool
        , Right . Left          <$> Gen.bool
        , Right . Right . Left  <$> Gen.bool
        , Right . Right . Right <$> Gen.bool
        ]

    indexResult :: Show a => Int -> a -> String
    indexResult n a =
      "Index " ++ show n ++ ": " ++ show a
  in
    HH.property $ do
      input <- HH.forAll inputGen

      case input of
        Left bool ->
          selectBranchAtProxy (Proxy :: Proxy 0) branches bool === indexResult 0 bool

        Right (Left bool) ->
          selectBranchAtProxy (Proxy :: Proxy 1) branches bool === indexResult 1 bool

        Right (Right (Left bool)) ->
          selectBranchAtProxy (Proxy :: Proxy 2) branches bool === indexResult 2 bool

        Right (Right (Right bool)) ->
          selectBranchAtProxy (Proxy :: Proxy 3) branches bool === indexResult 3 bool

prop_fourWayUnionDissection :: HH.Property
prop_fourWayUnionDissection =
  let
    branches :: Branches [String, Int, Bool, Double] String
    branches =
      branchBuild
      $ branch (indexResult 0)
      $ branch (indexResult 1)
      $ branch (indexResult 2)
      $ branch (indexResult 3)
      $ branchEnd

    indexResult :: Show a => Int -> a -> String
    indexResult n a =
      "Index " ++ show n ++ ": " ++ show a
  in
    HH.property $ do
      dissectionCase <- HH.forAll Gen.enumBounded

      (input, expected) <-
        case dissectionCase of
          DissectString -> do
            string <- HH.forAll $ Gen.string (Range.constant 0 10) Gen.unicodeAll
            pure (unify string, indexResult 0 string)

          DissectInt -> do
            int <- HH.forAll $ Gen.integral (Range.constant minBound maxBound)
            pure (unify (int :: Int), indexResult 1 int)

          DissectBool  -> do
            bool <- HH.forAll $ Gen.bool
            pure (unify bool, indexResult 2 bool)

          DissectDouble -> do
            double <- HH.forAll $ Gen.double (Range.constant 0 100)
            pure (unify double, indexResult 3 double)

      dissect branches (input :: Union [String, Int, Bool, Double]) === expected

data DissectionCase
  = DissectString
  | DissectInt
  | DissectBool
  | DissectDouble
  deriving (Show, Enum, Bounded)

prop_parserRunsAllOptions :: HH.Property
prop_parserRunsAllOptions =
  let
    parser :: Parser Maybe String [Int, Bool, String]
    parser =
        parseOption @Int readMaybe
      $ parseOption @Bool readMaybe
      $ parseOption @String Just
      $ parseEnd

    mkExpected :: String -> [Maybe (Union [Int, Bool, String])]
    mkExpected input =
      [ fmap (unify @Int)    (readMaybe input)
      , fmap (unify @Bool)   (readMaybe input)
      , fmap (unify @String) (Just input)
      ]
  in
    HH.property $ do
      value <-
        HH.forAll $
          Gen.choice
            [ show <$> Gen.bool
            , show <$> Gen.integral @_ @Int (Range.constant minBound maxBound)
            , Gen.string (Range.constant 0 10) Gen.unicodeAll
            ]

      let
        actual :: [Maybe (Union [Int, Bool, String])]
        actual = parse parser value

      fmap show actual === fmap show (mkExpected value)


