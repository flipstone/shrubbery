{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , TaggedBranches (TaggedBranches)
  , taggedBranchBuild
  , selectTaggedBranchAtIndex
  , selectBranchAtTag
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
import Shrubbery.Classes (EqBranches, NFDataBranches, OrdBranches, ShowBranches, TaggedBranchTypes, TaggedDissection (..), TaggedUnification (..), unifyWithIndex)
import Shrubbery.TaggedBranches (TaggedBranchBuilder, TaggedBranches (TaggedBranches), appendTaggedBranches, selectBranchAtTag, selectTaggedBranchAtIndex, taggedBranch, taggedBranchBuild, taggedBranchDefault, taggedBranchEnd, taggedBranchSet, taggedSingleBranch)
import Shrubbery.TypeList (KnownLength, Tag (..), TagIndex, TagType, TaggedTypes, TypeAtIndex)
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

{- |

@since 0.2.4.0
-}
type instance TaggedBranchTypes (TaggedUnion taggedTypes) = taggedTypes

{- |

@since 0.2.4.0
-}
instance TaggedDissection (TaggedUnion taggedTypes) where
  dissectTagged = dissectTaggedUnion

{- |

@since 0.2.4.0
-}
instance
  KnownLength (TaggedTypes taggedTypes) =>
  TaggedUnification (TaggedUnion taggedTypes)
  where
  unifyTaggedWithTag (_ :: proxy tag) =
    unifyTaggedUnion @tag

{- | Constructs a tagged union based on the tag of a member type in the list.  The tag must be
  supplied via @TypeApplications@. For instance:

  @
    a :: TaggedUnion ["foo" \@= String, "bar" \@= String]
    a = unifyTaggedUnion \@"bar" "bar-value"
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
    matchTaggedUnion \@"tag" someTaggedUnion
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
