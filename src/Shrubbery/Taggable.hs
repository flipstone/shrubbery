{- |
Copyright : Flipstone Technology Partners 2025
License   : BSD3
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shrubbery.Taggable
  ( Taggable(..) 
  ) where

import qualified GHC.Generics as G
import GHC.Generics ((:+:))
import Data.Kind (Type)

import Shrubbery.TypeList (Append, Tag, type (@=), TagType, TagIndex, TypeAtIndex, TaggedTypes, KnownLength)
import Shrubbery.TaggedUnion (TaggedUnion, unifyTaggedUnion, TaggedBranchBuilder, dissectTaggedUnion, taggedBranchBuild, TaggedBranches, taggedBranch, taggedSingleBranch, appendTaggedBranches)
import GHC.TypeLits (KnownNat)

data FooBar
  = Foo Int
  | Bar String
  | Baz
  deriving stock G.Generic
  deriving Taggable via (G.Generically FooBar)

data Bax
  = Bax Int
  deriving stock G.Generic
  deriving Taggable via (G.Generically Bax)

taggedFooBar :: TaggedUnion (Tags FooBar)
taggedFooBar =
  toTaggedUnion (Bax 0)


class Taggable a where
  type Tags a :: [Tag]

  toTaggedUnion :: a -> TaggedUnion (Tags a)
  -- fromTaggedUnion :: TaggedUnion (Tags a) -> a

instance
  ( G.Generic a
  , ToTaggedUnionG (GenericBranchTags (G.Rep a)) (G.Rep a)
  -- , FromTaggedUnionG (GenericBranchTags (G.Rep a)) (G.Rep a)
  , KnownLength (GenericBranchTags (G.Rep a))
  , KnownLength (TaggedTypes (GenericBranchTags (G.Rep a)))
  ) => Taggable (G.Generically a) where
  type Tags (G.Generically a) = GenericBranchTags (G.Rep a)

  toTaggedUnion (G.Generically a) = toTaggedUnionG (G.from a)
  -- fromTaggedUnion =
  --   let
  --     branches :: TaggedBranches (GenericBranchTags (G.Rep a)) (G.Rep a f)
  --     branches =
  --       taggedBranchBuild fromTaggedUnionG
  --   in
  --     G.Generically . G.to . dissectTaggedUnion branches

class ToTaggedUnionG tags rep where
  toTaggedUnionG :: rep f -> TaggedUnion tags

instance
  ( TagType constr tags ~ value
  , KnownNat n
  , TagIndex constr tags ~ n
  , TypeAtIndex n (TaggedTypes tags) ~ value
  ) =>
  ToTaggedUnionG tags (G.C1 (G.MetaCons constr p mi) (G.S1 si (G.K1 ki value))) where
  toTaggedUnionG (G.M1 (G.M1 (G.K1 val))) =
    unifyTaggedUnion @constr @value @tags val

instance
  ( TagType constr tags ~ ()
  , KnownNat n
  , TagIndex constr tags ~ n
  , TypeAtIndex n (TaggedTypes tags) ~ ()
  ) =>
  ToTaggedUnionG tags (G.C1 (G.MetaCons constr p mi) G.U1) where
  toTaggedUnionG _c1  =
    unifyTaggedUnion @constr @() @tags ()

instance ToTaggedUnionG tags f => ToTaggedUnionG tags (G.D1 c f) where
  toTaggedUnionG (G.M1 f) = toTaggedUnionG f

instance (ToTaggedUnionG tags a, ToTaggedUnionG tags b) => ToTaggedUnionG tags (a :+: b) where
  toTaggedUnionG genericSum =
    case genericSum of
      G.L1 left -> toTaggedUnionG left
      G.R1 right-> toTaggedUnionG right

class FromTaggedUnionG tags rep where
  fromTaggedUnionG :: (rep f -> a) -> TaggedBranchBuilder tags a

instance FromTaggedUnionG tags f => FromTaggedUnionG tags (G.D1 c f) where
  fromTaggedUnionG f = fromTaggedUnionG (f . G.M1)

instance FromTaggedUnionG '[constr @= value] (G.C1 (G.MetaCons constr p mi) (G.S1 si (G.K1 ki value))) where
  fromTaggedUnionG f =
    taggedSingleBranch @constr (f . G.M1 . G.M1 . G.K1)

instance FromTaggedUnionG '[constr @= ()] (G.C1 (G.MetaCons constr p mi) G.U1) where
  fromTaggedUnionG f =
    taggedSingleBranch @constr (const (f . G.M1 $ G.U1))

-- instance
--   ( FromTaggedUnionG left a
--   , FromTaggedUnionG right b
--   , Append left right ~ combined
--   , Append (TaggedTypes left) (TaggedTypes right) ~ TaggedTypes combined
--   ) => FromTaggedUnionG combined (a :+: b) where
--   fromTaggedUnionG f =
--     appendTaggedBranches 
--       (fromTaggedUnionG @left (f . G.L1))
--       (fromTaggedUnionG @right (f . G.R1))

type family GenericBranchTags (rep :: Type -> Type) :: [Tag] where
  GenericBranchTags (G.M1 G.C (G.MetaCons constr p mi) (G.S1 si (G.K1 ki value))) = '[constr @= value] 
  GenericBranchTags (G.M1 G.C (G.MetaCons constr p mi) G.U1) = '[constr @= ()] 
  GenericBranchTags (G.M1 i c f) = GenericBranchTags f
  GenericBranchTags (a :+: b) = Append (GenericBranchTags a) (GenericBranchTags b)

