{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  ) where

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic, Rep)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as HHM
import qualified Hedgehog.Range as Range
import Text.Read (readMaybe)

import Shrubbery
import Shrubbery.Generic
import Shrubbery.Parser

main :: IO ()
main =
  HHM.defaultMain
    [ HH.checkSequential $
        HH.Group
          "Shrubbery Tests"
          [ ("Four way branching on type works", prop_fourWayBranchingOnType)
          , ("Four way branching on index works", prop_fourWayBranchingOnIndex)
          , ("Branching with default branches works", prop_branchingWithDefaultBranches)
          , ("Mixing branchSet and branch on top of default", prop_mixingSetBranchAndBranchOnTopOfDefault)
          , ("Mixing branchSet and branch on top of branchEnd", prop_mixingSetBranchAndBranchOnTopOfBranchEnd)
          , ("Four way union dissection", prop_fourWayUnionDissection)
          , ("Four way union tagged dissection", prop_fourWayTaggedUnionDissection)
          , ("Union Equality", prop_unionEquality)
          , ("Tagged union dissection with default", prop_taggedUnionDissectionWithDefault)
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
      branchBuild
        . branch (indexResult 0)
        . branch (indexResult 1)
        . branch (indexResult 2)
        . branch (indexResult 3)
        $ branchEnd

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
      branchBuild
        . branch (indexResult 0)
        . branch (indexResult 1)
        . branch (indexResult 2)
        . branch (indexResult 3)
        $ branchEnd

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

prop_branchingWithDefaultBranches :: HH.Property
prop_branchingWithDefaultBranches =
  let
    branches :: Branches [String, Int, Bool] String
    branches =
      branchBuild
        . branchSet @Int show
        $ branchDefault "default-value"

    inputGen :: HH.Gen (Either String (Either Int Bool))
    inputGen =
      Gen.choice
        [ Left <$> Gen.string (Range.constant 0 10) Gen.unicodeAll
        , Right . Left <$> Gen.integral (Range.constant minBound maxBound)
        , Right . Right <$> Gen.bool
        ]
  in
    HH.property $ do
      input <- HH.forAll inputGen

      case input of
        Left string ->
          selectBranch branches string === "default-value"
        Right (Left int) ->
          selectBranch branches int === show int
        Right (Right bool) ->
          selectBranch branches bool === "default-value"

prop_mixingSetBranchAndBranchOnTopOfDefault :: HH.Property
prop_mixingSetBranchAndBranchOnTopOfDefault =
  let
    branches :: Branches [(), String, Int, Bool] String
    branches =
      branchBuild
        . branchSet @String (const "string-value")
        . branch @() (const "unit-value")
        . branch @String (const "string-value-hidden")
        . branchSet @Int (const "int-value")
        $ branchDefault "default-value"

    inputGen :: HH.Gen (Either () (Either String (Either Int Bool)))
    inputGen =
      Gen.choice
        [ pure (Left ())
        , Right . Left <$> Gen.string (Range.constant 0 10) Gen.unicodeAll
        , Right . Right . Left <$> Gen.integral (Range.constant minBound maxBound)
        , Right . Right . Right <$> Gen.bool
        ]
  in
    HH.property $ do
      input <- HH.forAll inputGen

      case input of
        Left () ->
          selectBranch branches () === "unit-value"
        Right (Left string) ->
          selectBranch branches string === "string-value"
        Right (Right (Left int)) ->
          selectBranch branches int === "int-value"
        Right (Right (Right bool)) ->
          selectBranch branches bool === "default-value"

prop_mixingSetBranchAndBranchOnTopOfBranchEnd :: HH.Property
prop_mixingSetBranchAndBranchOnTopOfBranchEnd =
  let
    branches :: Branches [(), String, Int, Bool] String
    branches =
      branchBuild
        . branchSet @String (const "string-value")
        . branch @() (const "unit-value")
        . branch @String (const "string-value-hidden")
        . branchSet @Bool (const "bool-value")
        . branch @Int (const "int-value")
        . branch @Bool (const "bool-value-hidden")
        $ branchEnd

    inputGen :: HH.Gen (Either () (Either String (Either Int Bool)))
    inputGen =
      Gen.choice
        [ pure (Left ())
        , Right . Left <$> Gen.string (Range.constant 0 10) Gen.unicodeAll
        , Right . Right . Left <$> Gen.integral (Range.constant minBound maxBound)
        , Right . Right . Right <$> Gen.bool
        ]
  in
    HH.property $ do
      input <- HH.forAll inputGen

      case input of
        Left () ->
          selectBranch branches () === "unit-value"
        Right (Left string) ->
          selectBranch branches string === "string-value"
        Right (Right (Left int)) ->
          selectBranch branches int === "int-value"
        Right (Right (Right bool)) ->
          selectBranch branches bool === "bool-value"

prop_fourWayUnionDissection :: HH.Property
prop_fourWayUnionDissection =
  let
    branches :: Branches [String, Int, Bool, Double] String
    branches =
      branchBuild
        . branch (indexResult 0)
        . branch (indexResult 1)
        . branch (indexResult 2)
        . branch (indexResult 3)
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
            pure (unify @String string, indexResult 0 string)
          DissectInt -> do
            int <- HH.forAll $ Gen.integral (Range.constant minBound maxBound)
            pure (unify @Int int, indexResult 1 int)
          DissectBool -> do
            bool <- HH.forAll Gen.bool
            pure (unify @Bool bool, indexResult 2 bool)
          DissectDouble -> do
            double <- HH.forAll $ Gen.double (Range.constant 0 100)
            pure (unify @Double double, indexResult 3 double)

      dissect branches (input :: Union [String, Int, Bool, Double]) === expected

prop_unionEquality :: HH.Property
prop_unionEquality =
  let
    modelEq =
      dissect
        . branchBuild
        . branch
          ( \a ->
              dissect
                . branchBuild
                . branch (== a)
                . branch (const False)
                . branch (const False)
                . branch (const False)
                $ branchEnd
          )
        . branch
          ( \a ->
              dissect
                . branchBuild
                . branch (const False)
                . branch (== a)
                . branch (const False)
                . branch (const False)
                $ branchEnd
          )
        . branch
          ( \a ->
              dissect
                . branchBuild
                . branch (const False)
                . branch (const False)
                . branch (== a)
                . branch (const False)
                $ branchEnd
          )
        . branch
          ( \a ->
              dissect
                . branchBuild
                . branch (const False)
                . branch (const False)
                . branch (const False)
                . branch (== a)
                $ branchEnd
          )
        $ branchEnd

    -- Make sure the same type appears more than once to validate that our index
    -- calculation takes this into account
    genValue :: HH.Gen (Union [String, Int, String, Bool])
    genValue =
      Gen.choice
        [ unifyWithIndex index0 <$> Gen.string (Range.linear 0 10) Gen.unicodeAll
        , unifyWithIndex index1 <$> Gen.int (Range.linear minBound 1024)
        , unifyWithIndex index2 <$> Gen.string (Range.linear 0 10) Gen.unicodeAll
        , unifyWithIndex index3 <$> Gen.enumBounded
        ]
  in
    HH.property $ do
      left <- HH.forAll genValue
      right <- HH.forAll genValue

      (left == right) === modelEq left right

prop_fourWayTaggedUnionDissection :: HH.Property
prop_fourWayTaggedUnionDissection =
  let
    branches ::
      TaggedBranches
        [ "string" @= String
        , "int" @= Int
        , "bool" @= Bool
        , "double" @= Double
        ]
        String
    branches =
      taggedBranchBuild
        . taggedBranch @"string" (indexResult 0)
        . taggedBranch @"int" (indexResult 1)
        . taggedBranch @"bool" (indexResult 2)
        . taggedBranch @"double" (indexResult 3)
        $ taggedBranchEnd

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
            pure (unifyTaggedUnion @"string" string, indexResult 0 string)
          DissectInt -> do
            int <- HH.forAll $ Gen.integral (Range.constant minBound maxBound)
            pure (unifyTaggedUnion @"int" int, indexResult 1 int)
          DissectBool -> do
            bool <- HH.forAll Gen.bool
            pure (unifyTaggedUnion @"bool" bool, indexResult 2 bool)
          DissectDouble -> do
            double <- HH.forAll $ Gen.double (Range.constant 0 100)
            pure (unifyTaggedUnion @"double" double, indexResult 3 double)

      let
        result =
          dissectTaggedUnion
            branches
            ( input ::
                TaggedUnion
                  [ "string" @= String
                  , "int" @= Int
                  , "bool" @= Bool
                  , "double" @= Double
                  ]
            )

      result === expected

prop_taggedUnionDissectionWithDefault :: HH.Property
prop_taggedUnionDissectionWithDefault =
  let
    branches ::
      TaggedBranches
        [ "string" @= String
        , "int" @= Int
        , "bool" @= Bool
        , "double" @= Double
        ]
        String

    branches =
      taggedBranchBuild
        . taggedBranchSet @"int" (const "int-value")
        . taggedBranchSet @"bool" (const "bool-value")
        $ taggedBranchDefault "default-value"
  in
    HH.property $ do
      dissectionCase <- HH.forAll Gen.enumBounded

      let
        (input, expected) =
          case dissectionCase of
            DissectString ->
              (unifyTaggedUnion @"string" "foo", "default-value")
            DissectInt ->
              (unifyTaggedUnion @"int" 0, "int-value")
            DissectBool ->
              (unifyTaggedUnion @"bool" True, "bool-value")
            DissectDouble ->
              (unifyTaggedUnion @"double" 3.14159, "default-value")

      let
        result =
          dissectTaggedUnion
            branches
            ( input ::
                TaggedUnion
                  [ "string" @= String
                  , "int" @= Int
                  , "bool" @= Bool
                  , "double" @= Double
                  ]
            )

      result === expected

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
        branchBuild
          . branch (\b -> "Bool: " <> show b)
          . branch (\i -> "IntA: " <> show i)
          . branch (\i -> "IntB: " <> show i)
          . branch (\() -> "NoData")
          $ branchEnd

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
