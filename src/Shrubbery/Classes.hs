{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ <= 902
{-# LANGUAGE ConstraintKinds #-}
#endif

{- |
  This module provides typeclasses for constructing and deconstructing
  types that have branches in them. This is to say, sum types or any type
  that can present a sum-type-like interface.

  See 'Shrubbery.Union' for examples using the provided 'Shrubbery.Union.Union'
  type. Intances of these classes can also be provided for regular Haskell
  sum types to allow them to be used with the programmable branching provided
  by 'Shrubbery.Branches', like so:

  @
    data MySum = AnInt Int | AString String

    type instance BranchTypes MySum = [Int, String]

    instance Dissection MySum where
      dissect branches sum =
        case sum of
          AnInt int -> selectBranch branches int
          AString string -> selectBranch branches string

    instance Unification MySum where
      unifyWithIndex idx =
        selectBranchAtIndex idx
          $ branchBuild
          $ branch AnInt
          $ branch AString
          $ branchEnd
  @
-}
module Shrubbery.Classes
  ( BranchTypes
  , Dissection (..)
  , Unification (..)
  , unify
  , ShowBranches
  , showsPrecViaDissect
  , EqBranches
  , eqViaDissect
  , OrdBranches
  , compareViaDissect
  , NFDataBranches
  , rnfViaDissect
  ) where

import qualified Control.DeepSeq as DeepSeq
import Data.Kind (Constraint, Type)
import GHC.TypeLits (KnownNat)

import Shrubbery.BranchIndex (BranchIndex, TypeZipper, branchIndexToInt, firstIndexOfType, indexOfFocusedType, moveZipperNext, startZipper)
import Shrubbery.Branches (BranchBuilder, Branches, branch, branchBuild, branchDefault, branchDefaultWithIndex, branchEnd, branchSetAtIndex)
import Shrubbery.TypeList (FirstIndexOf, KnownLength, ZippedTypes)

{- |
  This type family is used by both 'Dissection' and 'Unification' to specify
  the types of the values available in the branching type. If you provided
  instances of 'Dissection' or 'Unification', you'll need to provide an
  instance of this type family as well.
-}
type family BranchTypes a :: [Type]

{- |
  A 'Dissection' provides a way to "dissect" a sum type via case analysis. The
  branches for handling the cases are given via a 'Branches' value.
-}
class Dissection a where
  -- |
  --    Implementations of this must call the appropriate function in the given
  --    branches depending on the construction of the value @a@. If 'BranchTypes a'
  --    contains duplicate types, the implmentation should be careful to call the
  --    correct one in each case.
  dissect :: Branches (BranchTypes a) result -> a -> result

{- |
  A 'Unification' provides a means to construct a sum type by embedding the
  members of the sum.
-}
class Unification a where
  -- |
  --    Embeds a member of the sum in the sum by specifying its index. This
  --    index-based interface is required by implementors to ensure there is not
  --    ambiguity when 'BranchTypes a' contains duplicates.
  unifyWithIndex :: BranchIndex t (BranchTypes a) -> t -> a

{- |
  Constructs a sum type by embedding a member type within it.

  This function always embeds the member based on the first time it is found
  in 'BranchTypes a'. If there are duplicate types in 'BranchTypes a', you
  should use 'unifyWithIndex' to disambiguate them.
-}
unify ::
  forall t a branchIndex.
  ( KnownNat branchIndex
  , branchIndex ~ FirstIndexOf t (BranchTypes a)
  , Unification a
  ) =>
  t ->
  a
unify =
  unifyWithIndex firstIndexOfType

{- |
  'ShowBranches' is provided as a convenience for implementing 'Show' on
  sum types via 'Dissection'. See 'showsPrecViaDissect' for the most common
  way to make use of this.
-}
class ShowBranches types where
  showsPrecBranches :: BranchBuilder types (Int -> ShowS)

instance ShowBranches '[] where
  showsPrecBranches =
    branchEnd

instance (Show a, ShowBranches rest) => ShowBranches (a : rest) where
  showsPrecBranches =
    branch (flip showsPrec) showsPrecBranches

{- |
  'showsPrecViaDissect' can be used as the implementation of 'showsPrec' in the
  'Show' class for types that implement 'Dissection' when all the member types
  implement 'Show'
-}
showsPrecViaDissect ::
  ( Dissection a
  , KnownLength (BranchTypes a)
  , ShowBranches (BranchTypes a)
  ) =>
  Int ->
  a ->
  ShowS
showsPrecViaDissect prec a =
  dissect (branchBuild showsPrecBranches) a prec

data BranchesList (c :: Type -> Constraint) (allTypes :: [Type]) (someTypes :: [Type]) where
  BranchesNil :: BranchesList c allTypes '[]
  BranchesCons ::
    c a =>
    BranchesList c allTypes types ->
    BranchesList c allTypes (a : types)

{- |
  'EqBranches' is provided as a convenience for implementing 'Eq' on
  sum types via 'Dissection'. See 'eqViaDissect' for the most common
  way to make use of this.
-}
class EqBranches types where
  eqBranchesList :: BranchesList Eq allTypes types

instance EqBranches '[] where
  eqBranchesList =
    BranchesNil

instance (Eq a, EqBranches rest) => EqBranches (a : rest) where
  eqBranchesList =
    BranchesCons eqBranchesList

{- |
  'OrdBranches' is provided as a convenience for implementing 'Ord' on
  sum types via 'Dissection'. See 'compareViaDissect' for the most common
  way to make use of this.
-}
class OrdBranches types where
  compareBranchesList :: BranchesList Ord allTypes types

instance OrdBranches '[] where
  compareBranchesList =
    BranchesNil

instance (Ord a, OrdBranches rest) => OrdBranches (a : rest) where
  compareBranchesList =
    BranchesCons compareBranchesList

{- |
  'eqViaDissect' can be used as the implementation of '==' for an 'Eq' instance
  for types that implement 'Dissection' when all the  member types implement 'Eq'
-}
eqViaDissect ::
  ( Dissection a
  , types ~ BranchTypes a
  , EqBranches types
  , KnownLength types
  , types ~ (first : rest)
  ) =>
  a ->
  a ->
  Bool
eqViaDissect =
  dissect . dissect eqBranches

eqBranches ::
  ( EqBranches types
  , KnownLength types
  , types ~ (first : rest)
  ) =>
  Branches types (Branches types Bool)
eqBranches =
  let
    go ::
      ( KnownLength allTypes
      , allTypes ~ ZippedTypes front focus rest
      ) =>
      TypeZipper front focus rest ->
      BranchesList Eq allTypes (focus : rest) ->
      BranchBuilder (focus : rest) (Branches allTypes Bool)
    go zipper eb =
      case eb of
        BranchesCons eqBranchesRest ->
          let
            index =
              indexOfFocusedType zipper

            branchesRest =
              case eqBranchesRest of
                BranchesNil -> branchEnd
                BranchesCons _ -> go (moveZipperNext zipper) eqBranchesRest
          in
            branch
              (\a -> branchBuild . branchSetAtIndex index (a ==) $ branchDefault False)
              branchesRest
  in
    branchBuild $ go startZipper eqBranchesList

{- |
  'compareViaDissect' can be used as the implementation of '==' for an 'Ord' instance
  for types that implement 'Dissection' when all the  member types implement 'Ord'
-}
compareViaDissect ::
  ( Dissection a
  , types ~ BranchTypes a
  , OrdBranches types
  , KnownLength types
  , types ~ (first : rest)
  ) =>
  a ->
  a ->
  Ordering
compareViaDissect =
  dissect . dissect compareBranches

compareBranches ::
  ( OrdBranches types
  , KnownLength types
  , types ~ (first : rest)
  ) =>
  Branches types (Branches types Ordering)
compareBranches =
  let
    go ::
      ( KnownLength allTypes
      , allTypes ~ ZippedTypes front focus rest
      ) =>
      TypeZipper front focus rest ->
      BranchesList Ord allTypes (focus : rest) ->
      BranchBuilder (focus : rest) (Branches allTypes Ordering)
    go zipper eb =
      case eb of
        BranchesCons compareBranchesRest ->
          let
            index =
              indexOfFocusedType zipper

            indexInt =
              branchIndexToInt index

            branchesRest =
              case compareBranchesRest of
                BranchesNil -> branchEnd
                BranchesCons _ -> go (moveZipperNext zipper) compareBranchesRest

            defaultComparison =
              branchDefaultWithIndex (indexInt `compare`)
          in
            branch
              (\a -> branchBuild $ branchSetAtIndex index (a `compare`) defaultComparison)
              branchesRest
  in
    branchBuild $ go startZipper compareBranchesList

{- |
  'rnfViaDissect' can be used as the implementation of 'DeepSeq.rnf' in the
  'DeepSeq.NFData' class for types that implement 'Dissection' when all the member types
  implement 'DeepSeq.NFData'
-}
rnfViaDissect ::
  ( Dissection a
  , KnownLength (BranchTypes a)
  , NFDataBranches (BranchTypes a)
  ) =>
  a ->
  ()
rnfViaDissect =
  dissect (branchBuild rnfBranches)

class NFDataBranches types where
  rnfBranches :: BranchBuilder types ()

instance NFDataBranches '[] where
  rnfBranches =
    branchEnd

instance (DeepSeq.NFData a, NFDataBranches rest) => NFDataBranches (a : rest) where
  rnfBranches =
    branch DeepSeq.rnf rnfBranches
