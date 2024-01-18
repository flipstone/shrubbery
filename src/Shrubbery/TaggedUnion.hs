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
  , taggedBranchEnd
  , appendTaggedBranches
  ) where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Symbol)

import Shrubbery.BranchIndex (BranchIndex, indexOfTypeAt)
import Shrubbery.Branches (BranchBuilder, Branches, appendBranches, branchBuild, branchEnd, singleBranch)
import Shrubbery.Classes (unifyWithIndex)
import Shrubbery.TypeList (Append, KnownLength, Tag (..), TagIndex, TagType, TaggedTypes, TypeAtIndex, type (@=))
import Shrubbery.Union (Union, dissectUnion)

newtype TaggedUnion (taggedTypes :: [Tag])
  = TaggedUnion (Union (TaggedTypes taggedTypes))

newtype TaggedBranches (taggedTypes :: [Tag]) result
  = TaggedBranches (Branches (TaggedTypes taggedTypes) result)

newtype TaggedBranchBuilder (taggedTypes :: [Tag]) result
  = TaggedBranchBuilder (BranchBuilder (TaggedTypes taggedTypes) result)

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

dissectTaggedUnion ::
  TaggedBranches taggedTypes result ->
  TaggedUnion taggedTypes ->
  result
dissectTaggedUnion (TaggedBranches branches) (TaggedUnion union) =
  dissectUnion branches union

taggedBranchBuild ::
  ( KnownLength types
  , types ~ TaggedTypes taggedTypes
  ) =>
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranches taggedTypes result
taggedBranchBuild (TaggedBranchBuilder builder) =
  TaggedBranches (branchBuild builder)

taggedBranch ::
  forall (tag :: Symbol) typ result taggedTypes.
  (typ -> result) ->
  TaggedBranchBuilder taggedTypes result ->
  TaggedBranchBuilder ((tag @= typ) : taggedTypes) result
taggedBranch =
  appendTaggedBranches . singleTaggedBranch

taggedBranchEnd :: TaggedBranchBuilder '[] result
taggedBranchEnd =
  TaggedBranchBuilder branchEnd

singleTaggedBranch ::
  forall (tag :: Symbol) typ result.
  (typ -> result) ->
  TaggedBranchBuilder '[tag @= typ] result
singleTaggedBranch branchFunction =
  TaggedBranchBuilder (singleBranch branchFunction)

appendTaggedBranches ::
  Append (TaggedTypes taggedTypesA) (TaggedTypes taggedTypesB)
    ~ TaggedTypes (Append taggedTypesA taggedTypesB) =>
  TaggedBranchBuilder taggedTypesA result ->
  TaggedBranchBuilder taggedTypesB result ->
  TaggedBranchBuilder (Append taggedTypesA taggedTypesB) result
appendTaggedBranches (TaggedBranchBuilder branchesA) (TaggedBranchBuilder branchesB) =
  TaggedBranchBuilder (appendBranches branchesA branchesB)
