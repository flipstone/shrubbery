{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Shrubbery
import qualified Test.Tasty.Bench as TB

import qualified Bench.Union as Union

main :: IO ()
main =
  TB.defaultMain
    [ TB.bgroup
        "dissectUnion"
        benchDissectUnion
    ]

benchDissectUnion :: [TB.Benchmark]
benchDissectUnion =
  [ TB.bench "Union1" $ TB.nf Union.dissectUnion1 (Shrubbery.unify Union.T1)
  , TB.bench "Union10" $ TB.nf Union.dissectUnion10 (Shrubbery.unify Union.T10)
  , TB.bench "Union100" $ TB.nf Union.dissectUnion100 (Shrubbery.unify Union.T100)
  , TB.bench "Union1000" $ TB.nf Union.dissectUnion1000 (Shrubbery.unify Union.T1000)
  , TB.bench "Union1 - dynamic" $ TB.nf (Union.dissectUnion1Dyn "x") (Shrubbery.unify Union.T1)
  , TB.bench "Union10 - dynamic" $ TB.nf (Union.dissectUnion10Dyn "x") (Shrubbery.unify Union.T10)
  , TB.bench "Union100 - dynamic" $ TB.nf (Union.dissectUnion100Dyn "x") (Shrubbery.unify Union.T100)
  , TB.bench "Union1000 - dynamic" $ TB.nf (Union.dissectUnion1000Dyn "x") (Shrubbery.unify Union.T1000)
  , TB.bench "Union1 - floated" $ TB.nf Union.dissectUnion1Floated (Shrubbery.unify Union.T1)
  , TB.bench "Union1 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union1 Union.T1, Shrubbery.unify Union.T1)
  , TB.bench "Union10 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union10 Union.T1, Shrubbery.unify Union.T10)
  , TB.bench "Union10 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union10 Union.T10, Shrubbery.unify Union.T10)
  , TB.bench "Union100 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union100 Union.T1, Shrubbery.unify Union.T100)
  , TB.bench "Union100 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union100 Union.T100, Shrubbery.unify Union.T100)
  , TB.bench "Union1000 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union1000 Union.T1, Shrubbery.unify Union.T1000)
  , TB.bench "Union1000 - ==" $ TB.nf (uncurry (==)) (Shrubbery.unify @_ @Union.Union1000 Union.T1000, Shrubbery.unify Union.T1000)
  , TB.bench "Union1 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union1 Union.T1, Shrubbery.unify Union.T1)
  , TB.bench "Union10 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union10 Union.T1, Shrubbery.unify Union.T10)
  , TB.bench "Union10 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union10 Union.T10, Shrubbery.unify Union.T10)
  , TB.bench "Union100 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union100 Union.T1, Shrubbery.unify Union.T100)
  , TB.bench "Union100 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union100 Union.T100, Shrubbery.unify Union.T100)
  , TB.bench "Union1000 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union1000 Union.T1, Shrubbery.unify Union.T1000)
  , TB.bench "Union1000 - compare" $ TB.nf (uncurry compare) (Shrubbery.unify @_ @Union.Union1000 Union.T1000, Shrubbery.unify Union.T1000)
  ]
