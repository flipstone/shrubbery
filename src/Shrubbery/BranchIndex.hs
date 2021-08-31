{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-|
  This module provides types and functions for finding and using the index
  of a type within a type-level list.
-}
module Shrubbery.BranchIndex
  ( BranchIndex
  , firstIndexOfType
  , indexOfTypeAt
  , branchIndexToInt
  , TypeZipper
  , startZipper
  , moveZipperNext
  , indexOfFocusedType
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, natVal)

import Shrubbery.TypeList (FirstIndexOf, TypeAtIndex, ZippedTypes)

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

{-|
  'TypeZipper' facilitates interating through a list of known types while
  keeping track of the index as you go. You might need this if you're keeping
  track of a list of operations that use shrubbery functions that require a
  branch index, such as 'Shrubbery.Classes.unifyWithIndex'.

  Note: Types are added to the top of the @front@ list as the are encountered,
  so the list may be reversed from you expected. See the type of 'nextZipper'.
-}
newtype TypeZipper (front :: [Type]) (focus :: Type) (back :: [Type]) =
  TypeZipper Int

{-|
  Initializes a 'TypeZipper' at the start of the type list
-}
startZipper :: TypeZipper '[] first back
startZipper =
  TypeZipper 0

{-|
  Moves the zipper to the next type in the list. The currently focused item
  is added to the to the beginning of the @front@ list.
-}
moveZipperNext :: TypeZipper front focus (next : back)
               -> TypeZipper (focus : front) next rest
moveZipperNext (TypeZipper n) =
  TypeZipper (n + 1)

{-|
  Builds a 'BranchIndex' for the currently focused type of the zipper. This
  index can then be used with other functions such as
  'Shrubbery.Classes.unifyWithIndex'.
-}
indexOfFocusedType :: TypeZipper front focus back
                   -> BranchIndex t (ZippedTypes front focus back)
indexOfFocusedType (TypeZipper n) =
  BranchIndex n

