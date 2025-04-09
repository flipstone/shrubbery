{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

This module provides a implementation of a sum type whose members are known in the type sysem and
provides implementations of 'Dissection' and 'Unification'. It can be used as such:

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

@since 0.1.0.0
-}
module Shrubbery.Union
  ( Union (Union)
  , unifyUnion
  , dissectUnion
  , matchUnion
  , matchUnionProxy
  ) where

import qualified Control.DeepSeq as DeepSeq
import Data.Type.Equality ((:~:) (..))
import GHC.TypeLits (KnownNat)

import Shrubbery.BranchIndex (BranchIndex, firstIndexOfType, testBranchIndexEquality)
import Shrubbery.Branches (Branches, selectBranchAtIndex)
import Shrubbery.Classes (BranchTypes, Dissection (..), EqBranches, NFDataBranches, OrdBranches, ShowBranches, Unification (..), compareViaDissect, eqViaDissect, rnfViaDissect, showsPrecViaDissect)
import Shrubbery.TypeList (FirstIndexOf, KnownLength)

{- | Defines a type whose value can be a value of any one of the specified types.

@since 0.1.0.0
-}
data Union types where
  Union :: BranchIndex t types -> t -> Union types

type role Union nominal

instance (ShowBranches types, KnownLength types) => Show (Union types) where
  showsPrec = showsPrecViaDissect

instance (EqBranches types, KnownLength types, types ~ (first : rest)) => Eq (Union types) where
  (==) = eqViaDissect

instance (EqBranches types, OrdBranches types, KnownLength types, types ~ (first : rest)) => Ord (Union types) where
  compare = compareViaDissect

instance (NFDataBranches types, KnownLength types) => DeepSeq.NFData (Union types) where
  rnf = rnfViaDissect

{- | Selects a function from the branches based on the value contained within the union. This choice
  is based entirely on the index specified (or inferred) at the time the union was constructed, so
  there is no ambiguity if a type appears multiple times in the 'Union'

  This is also available as the 'dissect' function from the 'Dissection' class.

@since 0.1.0.0
-}
dissectUnion ::
  Branches types result ->
  Union types ->
  result
dissectUnion branches (Union branchIndex t) =
  selectBranchAtIndex branchIndex branches t
{-# INLINE dissectUnion #-}

{- | Matches a 'Union' against one of its types, returning 'Just' the value if the
  'Union' value is of that type, or 'Nothing' otherwise. Use with @TypeApplications@:

  @
    matchUnion @Int someUnion
  @

  Note that this will always match the first instance of the type in the union types list.

  See 'matchUnionProxy' for a version that does not require @TypeApplications@.

@since 0.2.2.0
-}
matchUnion ::
  forall t types branchIndex.
  ( KnownNat branchIndex
  , branchIndex ~ FirstIndexOf t types
  ) =>
  Union types ->
  Maybe t
matchUnion (Union branchIndex t) =
  let
    indexOfT :: BranchIndex t types
    indexOfT = firstIndexOfType @branchIndex
  in
    case testBranchIndexEquality indexOfT branchIndex of
      Just Refl -> Just t
      Nothing -> Nothing

{- | Matches a 'Union' against one of its types, returning 'Just' the value if the 'Union' value is of
  that type, or 'Nothing' otherwise, e.g.:

  @
    matchUnionProxy (Proxy :: Proxy Int) myUnion
  @

  Note that this will always match the first instance of the type in the union types list.

  See 'matchUnion' for a version that uses @TypeApplications@ instead of a proxy.

@since 0.2.2.0
-}
matchUnionProxy ::
  forall t types branchIndex proxy.
  ( KnownNat branchIndex
  , branchIndex ~ FirstIndexOf t types
  ) =>
  proxy t ->
  Union types ->
  Maybe t
matchUnionProxy _ = matchUnion

{- | Constructs a union based on the index of a member type in the list. This function can be used
  rather than 'Shrubbery.Classes.unify' to disambiguate types that appear multiple times in the
  list.

  This is also available as 'unifyWithIndex' from the 'Unification' class.

@since 0.1.0.0
-}
unifyUnion :: BranchIndex t types -> t -> Union types
unifyUnion =
  Union

type instance BranchTypes (Union types) = types

instance Dissection (Union types) where
  dissect = dissectUnion

instance Unification (Union types) where
  unifyWithIndex = unifyUnion
