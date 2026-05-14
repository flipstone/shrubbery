{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

This module provides the 'TaggedBranches' and 'TaggedBranchBuilder' types, which mirror 'Branches'
and 'BranchBuilder' but use 'Tag'-annotated type lists. These are factored out from
'Shrubbery.TaggedUnion' so that they can be used by 'Shrubbery.Classes' without creating a circular
dependency.

@since 0.2.4.0
-}
module Shrubbery.TaggedBranches
  ( TaggedBranches (TaggedBranches)
  , taggedBranchBuild
  , selectTaggedBranchAtIndex
  , selectBranchAtTag
  , TaggedBranchBuilder
  , taggedBranch
  , taggedSingleBranch
  , taggedBranchEnd
  , appendTaggedBranches
  , taggedBranchSet
  , taggedBranchDefault
  ) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Symbol)

import Shrubbery.BranchIndex (BranchIndex, indexOfTypeAt)
import Shrubbery.Branches (BranchBuilder, Branches, appendBranches, branchBuild, branchDefault, branchEnd, branchSetAtIndex, selectBranchAtIndex, singleBranch)
import Shrubbery.TypeList (Append, KnownLength, Tag (..), TagIndex, TaggedTypes, TypeAtIndex, type (@=))

{- | Similar to 'Branches', 'TaggedBranches' contains an array of functions that have different
  parameter types, but produce the same result. The @taggedTypes@ list of tags indicates the types
  of the input parameters, in order and tagged with their associated type-level symbols for
  reference.

@since 0.1.3.0
-}
newtype TaggedBranches (taggedTypes :: [Tag]) result
  = TaggedBranches (Branches (TaggedTypes taggedTypes) result)

{- | Similar to 'BranchBuilder', 'TaggedBranchBuilder' is an efficient interface for building a
  'TaggedBranches'. Use 'taggedBranchBuild' to "execute" the 'TaggedBranchBuilder' to make
  'TaggedBranches'.

@since 0.1.3.0
-}
newtype TaggedBranchBuilder (taggedTypes :: [Tag]) result
  = TaggedBranchBuilder (BranchBuilder (TaggedTypes taggedTypes) result)

{- | Like 'branchBuild', this finishes the building of a tagged branch set so that it can be used with
  'dissectTaggedUnion'.

@since 0.1.3.0
-}
taggedBranchBuild ::
  ( KnownLength types
  , types ~ TaggedTypes taggedTypes
  ) =>
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranches taggedTypes result
taggedBranchBuild (TaggedBranchBuilder builder) =
  TaggedBranches (branchBuild builder)
{-# INLINE taggedBranchBuild #-}

{- | Selects a function from a 'TaggedBranches' at the given index so that it can be used with
  the correct input parameter type.

@since 0.2.4.0
-}
selectTaggedBranchAtIndex ::
  BranchIndex t (TaggedTypes taggedTypes) ->
  TaggedBranches taggedTypes result ->
  t ->
  result
selectTaggedBranchAtIndex idx (TaggedBranches branches) =
  selectBranchAtIndex idx branches
{-# INLINE selectTaggedBranchAtIndex #-}

{- | Selects a function from a 'TaggedBranches' by its tag symbol, so that it can be applied to the
  corresponding input value. The tag must be supplied via @TypeApplications@:

  @
    selectBranchAtTag \@"foo" myBranches fooValue
  @

@since 0.2.4.0
-}
selectBranchAtTag ::
  forall (tag :: Symbol) typ taggedTypes n result.
  ( KnownNat n
  , TagIndex tag taggedTypes ~ n
  , TypeAtIndex n (TaggedTypes taggedTypes) ~ typ
  ) =>
  TaggedBranches taggedTypes result ->
  typ ->
  result
selectBranchAtTag (TaggedBranches branches) =
  selectBranchAtIndex (indexOfTypeAt (Proxy :: Proxy n)) branches
{-# INLINE selectBranchAtTag #-}

{- | Similar to 'branch'. Specifies how to handle a given tag in a list of tagged types. The function
  parameter type and tag is added to the front of the list of tagged types for the branches that are
  being constructed. This means that the branches must be specified (from "top" to "bottom") in the
  same order they are given in the list or else you get a compilation error. The tag must be
  supplied via @TypeApplications@:

  @
    branches :: TaggedBranches ["foo" \@= String, "bar" \@= String] String
    branches =
      taggedBranchBuild
      . taggedBranch \@"foo" doSomethingWithFooString
      . taggedBranch \@"bar" doSomethingwithBarString
      $ taggedBranchEnd
  @

@since 0.1.3.0
-}
taggedBranch ::
  forall (tag :: Symbol) typ result taggedTypes.
  (typ -> result) ->
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranchBuilder ((tag @= typ) : taggedTypes) result
taggedBranch =
  appendTaggedBranches . taggedSingleBranch
{-# INLINE taggedBranch #-}

{- | Similar to 'branchEnd'. Indicates that there are no more branches to specify.  This must appear
  as the final entry in a sequence of 'taggedBranch' calls to handle the base case of an empty type
  list, unless 'taggedBranchDefault' is used.

@since 0.1.3.0
-}
taggedBranchEnd :: TaggedBranchBuilder '[] result
taggedBranchEnd =
  TaggedBranchBuilder branchEnd
{-# INLINE taggedBranchEnd #-}

{- | Similar to 'singleBranch'. Specifies how to handle a given a single case in a standalone
  fashion. This can be used togther with `appendTaggedBranches` to assemble a branch builder with to
  cover all the desired cases without needing to begin with `taggedBranchEnd` and add branches via
  `taggedBranch`.
-}
taggedSingleBranch ::
  forall (tag :: Symbol) typ result.
  (typ -> result) ->
  TaggedBranchBuilder '[tag @= typ] result
taggedSingleBranch branchFunction =
  TaggedBranchBuilder (singleBranch branchFunction)
{-# INLINE taggedSingleBranch #-}

{- | Similar to 'appendBranches'. Appends two 'TaggedBranchBuilder's to form a new
  'TaggedBranchBuilder' that has branches for all the types from both the original.

@since 0.1.3.0
-}
appendTaggedBranches ::
  Append (TaggedTypes taggedTypesA) (TaggedTypes taggedTypesB)
    ~ TaggedTypes (Append taggedTypesA taggedTypesB) =>
  TaggedBranchBuilder taggedTypesA result ->
  TaggedBranchBuilder taggedTypesB result ->
  TaggedBranchBuilder (Append taggedTypesA taggedTypesB) result
appendTaggedBranches (TaggedBranchBuilder branchesA) (TaggedBranchBuilder branchesB) =
  TaggedBranchBuilder (appendBranches branchesA branchesB)
{-# INLINE appendTaggedBranches #-}

{- | Similar to 'setBranchAtIndex'. Sets the function that should be used to handle values of a
  particular tag symbol in the branch set. This function will replace the existing handler for the
  first occurence of the type in the branches. The tag must be specified via @TypeApplications@:

  @
    branches :: TaggedBranches ["foo" \@= String, "bar" \@= String, "baz" \@= Int] String
    branches =
      taggedBranchBuild
      . taggedBranchSet \@"bar" doSomethingwithBarString
      $ taggedBranchDefault "default-value"
  @

  See also 'taggedBranchDefault'.

@since 0.2.0.0
-}
taggedBranchSet ::
  forall (tag :: Symbol) typ result taggedTypes n.
  ( KnownNat n
  , n ~ TagIndex tag taggedTypes
  , typ ~ TypeAtIndex n (TaggedTypes taggedTypes)
  ) =>
  (typ -> result) ->
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranchBuilder taggedTypes result
taggedBranchSet branchFunction (TaggedBranchBuilder builder) =
  TaggedBranchBuilder (branchSetAtIndex (indexOfTypeAt (Proxy :: Proxy n)) branchFunction builder)
{-# INLINE taggedBranchSet #-}

{- | Similar to 'branchDefault'. Initializes a branch builder that will return the specified value for
  all branches, regardless of the value passed to select the branch. Usually this is used in
  conjunction with 'taggedBranchSet'.

@since 0.2.0.0
-}
taggedBranchDefault :: result -> TaggedBranchBuilder taggedTypes result
taggedBranchDefault =
  TaggedBranchBuilder . branchDefault
{-# INLINE taggedBranchDefault #-}
