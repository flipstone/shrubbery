{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  , NFDataBranches
  , rnfViaDissect
  ) where

import qualified Control.DeepSeq as DeepSeq
import Data.Kind (Type)
import GHC.TypeLits (KnownNat)

import Shrubbery.BranchIndex (BranchIndex, TypeZipper, firstIndexOfType, indexOfFocusedType, moveZipperNext, startZipper)
import Shrubbery.Branches (BranchBuilder, Branches, branch, branchBuild, branchDefault, branchEnd, branchSetAtIndex)
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
{-# INLINEABLE unify #-}
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
  {-# INLINEABLE showsPrecBranches #-}
  showsPrecBranches =
    branchEnd

instance (Show a, ShowBranches rest) => ShowBranches (a : rest) where
  {-# INLINEABLE showsPrecBranches #-}
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
{-# INLINEABLE showsPrecViaDissect #-}
showsPrecViaDissect prec a =
  dissect (branchBuild showsPrecBranches) a prec

data EqBranchesList (allTypes :: [Type]) (someTypes :: [Type]) where
  EqBranchesNil :: EqBranchesList allTypes '[]
  EqBranchesCons ::
    Eq a =>
    EqBranchesList allTypes types ->
    EqBranchesList allTypes (a : types)

{- |
  'EqBranches' is provided as a convenience for implementing 'Eq' on
  sum types via 'Dissection'. See 'eqViaDissect' for the most common
  way to make use of this.
-}
class EqBranches types where
  eqBranchesList :: EqBranchesList allTypes types

instance EqBranches '[] where
  {-# INLINEABLE eqBranchesList #-}
  eqBranchesList =
    EqBranchesNil

instance (Eq a, EqBranches rest) => EqBranches (a : rest) where
  {-# INLINEABLE eqBranchesList #-}
  eqBranchesList =
    EqBranchesCons eqBranchesList

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
{-# INLINEABLE eqViaDissect #-}
eqViaDissect a =
  dissect (dissect eqBranches a)

eqBranches ::
  ( EqBranches types
  , KnownLength types
  , types ~ (first : rest)
  ) =>
  Branches types (Branches types Bool)
{-# INLINEABLE eqBranches #-}
eqBranches =
  let
    go ::
      ( KnownLength allTypes
      , allTypes ~ ZippedTypes front focus rest
      ) =>
      TypeZipper front focus rest ->
      EqBranchesList allTypes (focus : rest) ->
      BranchBuilder (focus : rest) (Branches allTypes Bool)
    go zipper eb =
      case eb of
        EqBranchesCons eqBranchesRest ->
          let
            index =
              indexOfFocusedType zipper

            branchesRest =
              case eqBranchesRest of
                EqBranchesNil -> branchEnd
                EqBranchesCons _ -> go (moveZipperNext zipper) eqBranchesRest
          in
            branch
              (\a -> branchBuild . branchSetAtIndex index (a ==) $ branchDefault False)
              branchesRest
  in
    branchBuild $ go startZipper eqBranchesList

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
{-# INLINEABLE rnfViaDissect #-}
rnfViaDissect =
  dissect (branchBuild rnfBranches)

class NFDataBranches types where
  rnfBranches :: BranchBuilder types ()

instance NFDataBranches '[] where
  {-# INLINEABLE rnfBranches #-}
  rnfBranches =
    branchEnd

instance (DeepSeq.NFData a, NFDataBranches rest) => NFDataBranches (a : rest) where
  {-# INLINEABLE rnfBranches #-}
  rnfBranches =
    branch DeepSeq.rnf rnfBranches
