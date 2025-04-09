{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

@since 0.1.3.0
-}
module Shrubbery.TaggedUnion
  ( TaggedUnion
  , unifyTaggedUnion
  , TaggedBranches
  , taggedBranchBuild
  , dissectTaggedUnion
  , matchTaggedUnion
  , matchTaggedUnionProxy
  , TaggedBranchBuilder
  , taggedBranch
  , taggedSingleBranch
  , taggedBranchEnd
  , appendTaggedBranches
  , taggedBranchSet
  , taggedBranchDefault
  ) where

import qualified Control.DeepSeq as DeepSeq
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, Symbol)

import Data.Type.Equality ((:~:) (..))
import Shrubbery.BranchIndex (indexOfTypeAt, testBranchIndexEquality)
import Shrubbery.Branches (BranchBuilder, Branches, appendBranches, branchBuild, branchDefault, branchEnd, branchSetAtIndex, singleBranch)
import Shrubbery.Classes (EqBranches, NFDataBranches, OrdBranches, ShowBranches, unifyWithIndex)
import Shrubbery.TypeList (Append, KnownLength, Tag (..), TagIndex, TagType, TaggedTypes, TypeAtIndex, type (@=))
import Shrubbery.Union (Union (Union), dissectUnion)

{- | 'TaggedUnion' provides a variation on the 'Union' concept that allows a type-level string symbol
  to be associated with each union entry. This allows this symbol to be used with @TypeApplications@
  in the api functions to provide more robust selection of the union member even when there are
  multiple instances of the same type in the union because they can be tagged with different
  symbols.

@since 0.1.3.0
-}
newtype TaggedUnion (taggedTypes :: [Tag])
  = TaggedUnion (Union (TaggedTypes taggedTypes))

type role TaggedUnion nominal

instance
  ( TaggedTypes taggedTypes ~ types
  , ShowBranches types
  , KnownLength types
  ) =>
  Show (TaggedUnion taggedTypes)
  where
  showsPrec prec (TaggedUnion union) =
    showsPrec prec union
  {-# INLINE showsPrec #-}

instance
  ( TaggedTypes taggedTypes ~ types
  , types ~ (first : rest)
  , EqBranches types
  , KnownLength types
  ) =>
  Eq (TaggedUnion taggedTypes)
  where
  (==) (TaggedUnion left) (TaggedUnion right) =
    left == right
  {-# INLINE (==) #-}

instance
  ( TaggedTypes taggedTypes ~ types
  , types ~ (first : rest)
  , EqBranches types
  , OrdBranches types
  , KnownLength types
  ) =>
  Ord (TaggedUnion taggedTypes)
  where
  compare (TaggedUnion left) (TaggedUnion right) =
    compare left right
  {-# INLINE compare #-}

instance
  ( TaggedTypes taggedTypes ~ types
  , NFDataBranches types
  , KnownLength types
  ) =>
  DeepSeq.NFData (TaggedUnion taggedTypes)
  where
  rnf (TaggedUnion union) =
    DeepSeq.rnf union

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

{- | Constructs a tagged union based on the tag of a member type in the list.  The tag must be
  supplied via @TypeApplications@. For instance:

  @
    a :: TaggedUnion ["foo" @= String, "bar" @= String]
    a = unifyTaggedUnion @"bar" "bar-value"
  @

@since 0.1.3.0
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
unifyTaggedUnion =
  TaggedUnion . unifyWithIndex (indexOfTypeAt (Proxy :: Proxy n))
{-# INLINE unifyTaggedUnion #-}

{- | Selects a function from the branches based on the value contained within the union. This choice
  is based on the tag symbol that was specified to 'unifyTaggedUnion' when the value vas
  constructed.

@since 0.1.3.0
-}
dissectTaggedUnion ::
  TaggedBranches taggedTypes result ->
  TaggedUnion taggedTypes ->
  result
dissectTaggedUnion (TaggedBranches branches) (TaggedUnion union) =
  dissectUnion branches union
{-# INLINE dissectTaggedUnion #-}

{- | Matches a 'TaggedUnion' against one of its tags, returning 'Just' the value of the tag if the
  'TaggedUnion' value is of that tag and type, or 'Nothing' otherwise.  Use with @TypeApplications@:

  @
    matchTaggedUnion @"tag" someTaggedUnion
  @

  See 'matchTaggedUnionProxy' for a version that does not require @TypeApplications@.

@since 0.2.2.0
-}
matchTaggedUnion ::
  forall (tag :: Symbol) t taggedTypes n.
  ( TagType tag taggedTypes ~ t
  , KnownNat n
  , TagIndex tag taggedTypes ~ n
  , TypeAtIndex n (TaggedTypes taggedTypes) ~ t
  ) =>
  TaggedUnion taggedTypes ->
  Maybe t
matchTaggedUnion (TaggedUnion (Union branchIndex t)) =
  case testBranchIndexEquality (indexOfTypeAt (Proxy :: Proxy n)) branchIndex of
    Just Refl -> Just t
    Nothing -> Nothing
{-# INLINE matchTaggedUnion #-}

{- | Matches a 'TaggedUnion' against one of its tags, returning 'Just' the value of the tag if the
  'TaggedUnion' value is of that tag and type, or 'Nothing' otherwise, e.g.:

  @
    matchTaggedUnionProxy (Proxy :: Proxy "tag") myTaggedUnion
  @

  See 'matchTaggedUnion' for a version that uses @TypeApplications@ instead of a proxy.

@since 0.2.2.0
-}
matchTaggedUnionProxy ::
  forall (tag :: Symbol) t taggedTypes n proxy.
  ( TagType tag taggedTypes ~ t
  , KnownNat n
  , TagIndex tag taggedTypes ~ n
  , TypeAtIndex n (TaggedTypes taggedTypes) ~ t
  ) =>
  proxy tag ->
  TaggedUnion taggedTypes ->
  Maybe t
matchTaggedUnionProxy _ = matchTaggedUnion @tag
{-# INLINE matchTaggedUnionProxy #-}

{- | Like 'branchBuild', this finishes the building of a tagged branch set so that it can be used with
  'dissectTagUnion'.

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

{- | Similar to 'branch'. Specifies how to handle a given tag in a list of tagged types. The function
  parameter type and tag is added to the front of the list of tagged types for the branches that are
  being constructed. This means that the branches must be specified (from "top" to "bottom") in the
  same order they are given in the list or else you get a compilation error. The tag must be
  supplied via @TypeApplications@:

  @
    branches :: TaggedBranches ["foo" @= String, "bar" @= String] String
    branches =
      taggedBranchBuild
      . taggedBranch @"foo" doSomethingWithFooString
      . taggedBranch @"bar" doSomethingwithBarString
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
  particular tag symbal in the branch set. This function will replace the existing handler for the
  first occurence of the type in the branches. The tag must be specified via @TypeApplications@:

  @
    branches :: TaggedBranches ["foo" @= String, "bar" @= String, "baz" @= Int] String
    branches =
      taggedBranchBuild
      . taggedBranchSet @"bar" doSomethingwithBarString
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
  all branches, regardles of the value passed to select the branch. Usually this is used in
  conjuctions with 'taggedBranchSet'.

@since 0.2.0.0
-}
taggedBranchDefault :: result -> TaggedBranchBuilder taggedTypes result
taggedBranchDefault =
  TaggedBranchBuilder . branchDefault
{-# INLINE taggedBranchDefault #-}
