{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
  This module provides a implementation of a sum type whose members are
  known in the type sysem and provides implementations of 'Dissection'
  and 'Unification'. It can be used as such:

  @
    type MyUnion = Union [Int, String]

    anInt :: MyUnion
    anInt = unify (0 :: Int)

    aString :: MyUnion
    aString = unify \"Foo\"

    doSomething :: MyUnion -> String
    doSomething =
      dissect
        $ branchBuild
        $ branch (show :: Int -> String)
        $ branch id
        $ branchEnd
  @

  Or you can use type applications:

  @
    anInt :: MyUnion
    anInt = unify \@Int 0

    aString :: MyUnion
    aString = unify \@String \"Foo\"

    doSomething :: MyUnion -> String
    doSomething =
      dissect
        $ branchBuild
        $ branch \@Int    show
        $ branch \@String id
        $ branchEnd
  @
-}
module Shrubbery.Union
  ( Union (Union)
  , unifyUnion
  , dissectUnion
  ) where

import Shrubbery.BranchIndex (BranchIndex)
import Shrubbery.Branches (Branches, selectBranchAtIndex)
import Shrubbery.Classes (BranchTypes, Dissection (..), EqBranches, ShowBranches, Unification (..), eqViaDissect, showsPrecViaDissect)
import Shrubbery.TypeList (KnownLength)

{- |
  Defines a type whose value can be a value of any one of the specified types.
-}
data Union types where
  Union :: BranchIndex t types -> t -> Union types

instance (ShowBranches types, KnownLength types) => Show (Union types) where
  showsPrec = showsPrecViaDissect

instance (EqBranches types, KnownLength types, types ~ (first : rest)) => Eq (Union types) where
  (==) = eqViaDissect

{- |
  Selects a function from the branches based on the value contained within the
  union. This choice is based entirely on the index specified (or inferred) at
  the time the union was constructed, so there is no ambiguity if a type
  appears multiple times in the 'Union'

  This is also available as the 'dissect' function from the 'Dissection' class.
-}
dissectUnion ::
  Branches types result ->
  Union types ->
  result
dissectUnion branches (Union branchIndex t) =
  selectBranchAtIndex branchIndex branches t

{- |
  Constructs a union based on the index of a member type in the list. This
  function can be used rather than 'Shrubbery.Classes.unify' to disambiguate types that appear
  multiple times in the list.

  This is also available as 'unifyWithIndex' from the 'Unification' class.
-}
unifyUnion :: BranchIndex t types -> t -> Union types
unifyUnion =
  Union

type instance BranchTypes (Union types) = types

instance Dissection (Union types) where
  dissect = dissectUnion

instance Unification (Union types) where
  unifyWithIndex = unifyUnion
