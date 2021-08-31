{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-|
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
  , Dissection(..)
  , Unification(..)
  , unify
  , ShowBranches(..)
  , showsPrecViaDissect
  ) where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat)

import Shrubbery.BranchIndex (BranchIndex, firstIndexOfType)
import Shrubbery.Branches (Branches, BranchBuilder, branchBuild, branch, branchEnd)
import Shrubbery.TypeList (FirstIndexOf, KnownLength)

{-|
  This type family is used by both 'Dissection' and 'Unification' to specify
  the types of the values available in the branching type. If you provided
  instances of 'Dissection' or 'Unification', you'll need to provide an
  instance of this type family as well.
-}
type family BranchTypes a :: [Type]

{-|
  A 'Dissection' provides a way to "dissect" a sum type via case analysis. The
  branches for handling the cases are given via a 'Branches' value.
-}
class Dissection a where
  {-|
    Implementations of this must call the appropriate function in the given
    branches depending on the construction of the value @a@. If 'BranchTypes a'
    contains duplicate types, the implmentation should be careful to call the
    correct one in each case.
  -}
  dissect :: Branches (BranchTypes a) result -> a -> result

{-|
  A 'Unification' provides a means to construct a sum type by embedding the
  members of the sum.
-}
class Unification a where
  {-|
    Embeds a member of the sum in the sum by specifying its index. This
    index-based interface is required by implementors to ensure there is not
    ambiguity when 'BranchTypes a' contains duplicates.
  -}
  unifyWithIndex :: BranchIndex t (BranchTypes a) -> t -> a

{-|
  Constructs a sum type by embedding a member type within it.

  This function always embeds the member based on the first time it is found
  in 'BranchTypes a'. If there are duplicate types in 'BranchTypes a', you
  should use 'unifyWithIndex' to disambiguate them.
-}
unify :: forall t a branchIndex.
        ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf t (BranchTypes a)
        , Unification a
        )
      => t
      -> a
unify =
  unifyWithIndex firstIndexOfType


{-|
  'ShowBranches' is provided as a convenience for implementing 'Show' on
  sum types via 'Dissection'
-}
class ShowBranches types where
  showsPrecBranches :: BranchBuilder types (Int -> ShowS)

instance ShowBranches '[] where
  showsPrecBranches =
    branchEnd

instance (Show a, ShowBranches rest) => ShowBranches (a : rest) where
  showsPrecBranches =
    branch (flip showsPrec) showsPrecBranches

{-|
  'showsPrecViaDissect' can be used as the implementation of
  'showsPrec' for types that implement 'Dissection' when all the member
  types implement 'Show'
-}
showsPrecViaDissect :: ( Dissection a
                       , KnownLength (BranchTypes a)
                       , ShowBranches (BranchTypes a))
                       => Int
                       -> a
                       -> ShowS
showsPrecViaDissect prec a =
  dissect (branchBuild showsPrecBranches) a prec
