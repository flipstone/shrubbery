{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-|
  This module provides types and functions for finding and using the index
  of a type within a type-level list.
-}
module Shrubbery.BranchIndex
  ( BranchIndex
  , firstIndexOfType
  , indexOfTypeAt
  , branchIndexToInt
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, natVal)

import Shrubbery.TypeList (FirstIndexOf, TypeAtIndex)

{-|
  A 'BranchIndex' is an zero-based index into a list of types for which
  the type at the index is known. This type is used with 'Shrubbery.Branches'
  to perform efficient branching at runtime.
-}
newtype BranchIndex t (types :: [Type]) =
  BranchIndex Int

{-|
  Finds the first index of a type in a list of types. The type list and the
  type being searched for must be inferrable based on the usage, or be
  supplied via @TypeApplications@.
-}

firstIndexOfType :: ( KnownNat branchIndex
                    , branchIndex ~ FirstIndexOf t types
                    )
                 => BranchIndex t types
firstIndexOfType =
  firstIndexOfTypeByProxy Proxy

firstIndexOfTypeByProxy :: ( KnownNat branchIndex
                           , branchIndex ~ FirstIndexOf t types
                           )
                        => Proxy branchIndex
                        -> BranchIndex t types
firstIndexOfTypeByProxy =
  BranchIndex . fromInteger . natVal

{-|
  Builds an index based on an known index value from the type system,
  associating it with the appropriate type from the list. The index is
  specified via a proxy value.
-}
indexOfTypeAt :: (KnownNat branchIndex, t ~ TypeAtIndex branchIndex types)
              => proxy branchIndex
              -> BranchIndex t types
indexOfTypeAt =
  BranchIndex . fromInteger . natVal

{-|
  Retrieves the 'Int' representation of the branch index. It it up to the
  caller to use this integer in a responsible fashion ;)
-}
branchIndexToInt :: BranchIndex t types -> Int
branchIndexToInt (BranchIndex n) =
  n

