{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Shrubbery.TaggedUnion
  ( TaggedUnion
  , unifyTaggedUnion
  , TaggedBranches
  , taggedBranchBuild
  , dissectTaggedUnion
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
import Shrubbery.Branches (BranchBuilder, Branches, appendBranches, branchBuild, branchDefault, branchEnd, branchSetAtIndex, singleBranch)
import Shrubbery.Classes (unifyWithIndex)
import Shrubbery.TypeList (Append, KnownLength, Tag (..), TagIndex, TagType, TaggedTypes, TypeAtIndex, type (@=))
import Shrubbery.Union (Union, dissectUnion)

{- |
  'TaggedUnion' provides a variation on the 'Union' concept that allows a type-level
  string symbol to be associated with each union entry. This allows this symbol to
  be used with @TypeApplications@ in the api functions to provide more robust selection
  of the union member even when there are multiple instances of the same type in the
  union because they can be tagged with different symbols.
-}
newtype TaggedUnion (taggedTypes :: [Tag])
  = TaggedUnion (Union (TaggedTypes taggedTypes))

{- |
  Similar to 'Branches', 'TaggedBranches' contains an array of functions that
  have different parameter types, but produce the same result. The @taggedTypes@
  list of tags indicates the types of the input parameters, in order and tagged
  with their associated type-level symbols for reference.
-}
newtype TaggedBranches (taggedTypes :: [Tag]) result
  = TaggedBranches (Branches (TaggedTypes taggedTypes) result)

{- |
  Similar to 'BranchBuilder', 'TaggedBranchBuilder' is an efficient interface
  for building a 'TaggedBranches'. Use 'taggedBranchBuild' to "execute" the
  'TaggedBranchBuilder' to make 'TaggedBranches'.
-}
newtype TaggedBranchBuilder (taggedTypes :: [Tag]) result
  = TaggedBranchBuilder (BranchBuilder (TaggedTypes taggedTypes) result)

{- |
  Constructs a tagged union based on the tag of a member type in the list.
  The tag must be supplied via @TypeApplications@. For instance:

  @
    a :: TaggedUnion ["foo" @= String, "bar" @= String]
    a = unifyTaggedUnion @"bar" "bar-value"
  @
-}
unifyTaggedUnion ::
  forall (tag :: Symbol) typ taggedTypes n.
  ( TagType tag taggedTypes ~ typ
  , KnownNat n
  , TagIndex tag taggedTypes ~ n
  , TypeAtIndex n (TaggedTypes taggedTypes) ~ typ
  ) =>
  typ ->
  TaggedUnion taggedTypes
unifyTaggedUnion value =
  let
    tagIndex :: Proxy n
    tagIndex =
      Proxy

    index :: BranchIndex typ (TaggedTypes taggedTypes)
    index =
      indexOfTypeAt tagIndex
  in
    TaggedUnion (unifyWithIndex index value)

{- |
  Selects a function from the branches based on the value contained within the
  union. This choice is based on the tag symbol that was specified to
  'unifyTaggedUnion' when the value vas constructed.
-}
dissectTaggedUnion ::
  TaggedBranches taggedTypes result ->
  TaggedUnion taggedTypes ->
  result
dissectTaggedUnion (TaggedBranches branches) (TaggedUnion union) =
  dissectUnion branches union

{- |
  Like 'branchBuild', this finishes the building of a tagged branch set
  so that it can be used with 'dissectTagUnion'.
-}
taggedBranchBuild ::
  ( KnownLength types
  , types ~ TaggedTypes taggedTypes
  ) =>
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranches taggedTypes result
taggedBranchBuild (TaggedBranchBuilder builder) =
  TaggedBranches (branchBuild builder)

{- |
  Similar to 'branch'. Specifies how to handle a given tag in a list of tagged
  types. The function parameter type and tag is added to the front of the list of
  tagged types for the branches that are being constructed. This means that the
  branches must be specified (from "top" to "bottom") in the same order they are
  given in the list or else you get a compilation error. The tag must be supplied
  via @TypeApplications@:

  @
    branches :: TaggedBranches ["foo" @= String, "bar" @= String] String
    branches =
      taggedBranchBuild
      . taggedBranch @"foo" doSomethingWithFooString
      . taggedBranch @"bar" doSomethingwithBarString
      $ taggedBranchEnd
  @
-}
taggedBranch ::
  forall (tag :: Symbol) typ result taggedTypes.
  (typ -> result) ->
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranchBuilder ((tag @= typ) : taggedTypes) result
taggedBranch =
  appendTaggedBranches . taggedSingleBranch

{- |
  Similar to 'branchEnd'. Indicates that there are no more branches to specify.
  This must appear as the final entry in a sequence of 'taggedBranch' calls to
  handle the base case of an empty type list, unless 'taggedBranchDefault' is
  used.
-}
taggedBranchEnd :: TaggedBranchBuilder '[] result
taggedBranchEnd =
  TaggedBranchBuilder branchEnd

{- |
  Similar to 'singleBranch'. Specifies how to handle a given a single case in a
  standalone fashion. This can be used togther with `appendTaggedBranches` to
  assemble a branch builder with to cover all the desired cases without needing
  to begin with `taggedBranchEnd` and add branches via `taggedBranch`.
-}
taggedSingleBranch ::
  forall (tag :: Symbol) typ result.
  (typ -> result) ->
  TaggedBranchBuilder '[tag @= typ] result
taggedSingleBranch branchFunction =
  TaggedBranchBuilder (singleBranch branchFunction)

{- |
  Similar to 'appendBranches'. Appends two 'TaggedBranchBuilder's to form a new
  'TaggedBranchBuilder' that has branches for all the types from both the
  original.
-}
appendTaggedBranches ::
  Append (TaggedTypes taggedTypesA) (TaggedTypes taggedTypesB)
    ~ TaggedTypes (Append taggedTypesA taggedTypesB) =>
  TaggedBranchBuilder taggedTypesA result ->
  TaggedBranchBuilder taggedTypesB result ->
  TaggedBranchBuilder (Append taggedTypesA taggedTypesB) result
appendTaggedBranches (TaggedBranchBuilder branchesA) (TaggedBranchBuilder branchesB) =
  TaggedBranchBuilder (appendBranches branchesA branchesB)

{- |
  Similar to  'setBranchAtIndex'. Sets the function that should be used to
  handle values of a particular tag symbal in the branch set. This function will
  replace the existing handler for the first occurence of the type in the
  branches. The tag must be specified via @TypeApplications@:

  @
    branches :: TaggedBranches ["foo" @= String, "bar" @= String, "baz" @= Int] String
    branches =
      taggedBranchBuild
      . taggedBranchSet @"bar" doSomethingwithBarString
      $ taggedBranchDefault "default-value"
  @

  See also 'taggedBranchDefault'.
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
  let
    tagIndex :: Proxy n
    tagIndex =
      Proxy

    index :: BranchIndex typ (TaggedTypes taggedTypes)
    index =
      indexOfTypeAt tagIndex
  in
    TaggedBranchBuilder (branchSetAtIndex index branchFunction builder)

{- |
  Similar to 'branchDefault'. Initializes a branch builder that will return the
  specified value for all branches, regardles of the value passed to select the
  branch. Usually this is used in conjuctions with 'taggedBranchSet'.
-}
taggedBranchDefault :: result -> TaggedBranchBuilder taggedTypes result
taggedBranchDefault defaultValue =
  TaggedBranchBuilder (branchDefault defaultValue)
