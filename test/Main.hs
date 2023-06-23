{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (
    main,
) where

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic, Rep)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as HHM
import qualified Hedgehog.Range as Range
import Text.Read (readMaybe)

import Shrubbery.BranchIndex
import Shrubbery.Branches
import Shrubbery.Classes
import Shrubbery.Generic
import Shrubbery.Parser
import Shrubbery.Union

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
                , ("Generic dissection", prop_genericDissection)
                , ("Generic unification", prop_genericUnification)
                ]
        ]

prop_fourWayBranchingOnType :: HH.Property
prop_fourWayBranchingOnType =
    let
        branches :: Branches [String, Int, Bool, (Bool, Bool)] String
        branches =
            branchBuild $
                branch (indexResult 0) $
                    branch (indexResult 1) $
                        branch (indexResult 2) $
                            branch (indexResult 3) $
                                branchEnd

        inputGen :: HH.Gen (Either String (Either Int (Either Bool (Bool, Bool))))
        inputGen =
            Gen.choice
                [ Left <$> Gen.string (Range.constant 0 10) Gen.unicodeAll
                , Right . Left <$> Gen.integral (Range.constant minBound maxBound)
                , Right . Right . Left <$> Gen.bool
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
            branchBuild $
                branch (indexResult 0) $
                    branch (indexResult 1) $
                        branch (indexResult 2) $
                            branch (indexResult 3) $
                                branchEnd

        inputGen =
            Gen.choice
                [ Left <$> Gen.bool
                , Right . Left <$> Gen.bool
                , Right . Right . Left <$> Gen.bool
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
            branchBuild $
                branch (indexResult 0) $
                    branch (indexResult 1) $
                        branch (indexResult 2) $
                            branch (indexResult 3) $
                                branchEnd

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
                        pure (unify @String string, indexResult 0 string)
                    DissectInt -> do
                        int <- HH.forAll $ Gen.integral (Range.constant minBound maxBound)
                        pure (unify @Int int, indexResult 1 int)
                    DissectBool -> do
                        bool <- HH.forAll $ Gen.bool
                        pure (unify @Bool bool, indexResult 2 bool)
                    DissectDouble -> do
                        double <- HH.forAll $ Gen.double (Range.constant 0 100)
                        pure (unify @Double double, indexResult 3 double)

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
            parseOption @Int readMaybe $
                parseOption @Bool readMaybe $
                    parseOption @String Just $
                        parseEnd

        mkExpected :: String -> [Maybe (Union [Int, Bool, String])]
        mkExpected input =
            [ fmap (unify @Int) (readMaybe input)
            , fmap (unify @Bool) (readMaybe input)
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

data GenericSum
    = GenericSumBool Bool
    | GenericSumIntA Int
    | GenericSumIntB Int
    | GenericSumNoData
    deriving (Show, Eq, Generic)

type instance BranchTypes GenericSum = GenericBranchTypes (Rep GenericSum)

instance Dissection GenericSum where
    dissect = genericDissect

instance Unification GenericSum where
    unifyWithIndex = genericUnifyWithIndex

genericSumGen :: HH.Gen GenericSum
genericSumGen =
    Gen.choice
        [ GenericSumBool <$> Gen.bool
        , GenericSumIntA <$> Gen.integral @_ @Int (Range.constant minBound maxBound)
        , GenericSumIntB <$> Gen.integral @_ @Int (Range.constant minBound maxBound)
        , pure GenericSumNoData
        ]

prop_genericDissection :: HH.Property
prop_genericDissection =
    HH.property $ do
        value <- HH.forAll genericSumGen

        let
            branches =
                branchBuild $
                    branch (\b -> "Bool: " <> show b) $
                        branch (\i -> "IntA: " <> show i) $
                            branch (\i -> "IntB: " <> show i) $
                                branch (\() -> "NoData") $
                                    branchEnd

            expected =
                case value of
                    GenericSumBool b -> "Bool: " <> show b
                    GenericSumIntA i -> "IntA: " <> show i
                    GenericSumIntB i -> "IntB: " <> show i
                    GenericSumNoData -> "NoData"

        dissect branches value === expected

prop_genericUnification :: HH.Property
prop_genericUnification =
    HH.property $ do
        value <- HH.forAll genericSumGen

        let
            actual =
                case value of
                    GenericSumBool b -> unifyWithIndex index0 b
                    GenericSumIntA i -> unifyWithIndex index1 i
                    GenericSumIntB i -> unifyWithIndex index2 i
                    GenericSumNoData -> unifyWithIndex index3 ()

        actual === value
