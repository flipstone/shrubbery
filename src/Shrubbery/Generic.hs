{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
  This module provides tools that can be use in conjuction with the
  'GHC.Generics' module to implement Shrubbery's 'Dissection' and 'Unification'
  classes (and the 'BranchTypes' type family) with simple one line
  implementations. For example:

  @
  data MySum
    = MySumBool Bool
    | MySumIntA Int
    | MySumIntB Int -- don't repeat the same data type in practice, see warning below
    | MySumWithNoData
    deriving (Show, Eq, Generic)

  type instance BranchTypes MySum = GenericBranchTypes (Rep MySum)

  instance Dissection MySum where
    dissect = genericDissect

  instance Unification MySum where
    unifyWithIndex = genericUnifyWithIndex
  @

  Warning: 'GenericBranchTypes' orders the list of types according to the order
  of the constructors in the definition of your sum type (@MySum@ above). The
  results in any users of 'dissect' or 'unifyWithIndex' to be implicitly
  dependent on the ordering. In the best case scenario, this means re-ordering
  constructors might produce a compilation error for a user of the type. In the
  worst case, it might slightly change the callers logic if the types happen to
  match. In the example above flipping the order of @MySumIntA@ and @MySumIntB@
  will not produce any type error detectable by the compiler, but will
  nonetheless result in the use of constructors being flipped in all usages of
  'dissect' and 'unifyWithIndex'. This is almost certainly not what users of
  those functions were expected. It is strongly recommended that you create a
  specific and unique type to represent branch of your sum type if you plan to
  use 'genericDissect' and 'genericUnifyWithIndex' with it.
-}
module Shrubbery.Generic
  ( GenericBranchTypes
  , genericDissect
  , genericFromUnion
  , genericToUnion
  , genericUnifyWithIndex
  , GenericUnion
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (from, to), K1 (K1, unK1), M1 (M1, unM1), Rep, U1 (U1), (:+:) (L1, R1))
import Unsafe.Coerce (unsafeCoerce)

import Shrubbery.BranchIndex (BranchIndex, appendTypesToIndex, firstIndexOfType, prependTypesToIndex, splitIndex)
import Shrubbery.Branches (Branches)
import Shrubbery.TypeList (Append, KnownLength)
import Shrubbery.Union (Union (Union), dissectUnion)

{- |
  'genericDissect' provides a suitable implementation of 'dissect' for a sum
  type with a 'Generic' representation of multiple single-argument
  constructors.

  Warning: See the discussion in the module documentation for a reason you
  might not actually want to do this.
-}
genericDissect ::
  (Generic a, GenericUnion (Rep a)) =>
  Branches (GenericBranchTypes (Rep a)) result ->
  a ->
  result
genericDissect branches =
  dissectUnion branches . genericToUnion . from

{- |
  'genericUnifyWithIndex' provides a suitable implementation of
  'unifyWithIndex' for a sum type with a 'Generic' representation of multiple
  single-argument constructors.

  Warning: See the discussion in the module documentation for a reason you
  might not actually want to do this.
-}
genericUnifyWithIndex ::
  (Generic a, GenericUnion (Rep a)) =>
  BranchIndex t (GenericBranchTypes (Rep a)) ->
  t ->
  a
genericUnifyWithIndex index t =
  to (constructorForIndex index t)

{- |
  'genericFromUnion' allows a sum type with a 'Generic' representation of
  multiple single-argument constructors to be created from a 'Union' of its
  'GenericBranchTypes'.

  Warning: See the discussion in the module documentation for a reason you
  might not actually want to do this.
-}
genericFromUnion ::
  (Generic a, GenericUnion (Rep a)) =>
  Union (GenericBranchTypes (Rep a)) ->
  a
genericFromUnion (Union index t) =
  genericUnifyWithIndex index t

{- |
  'GenericBranchTypes' should be as the implementation of the 'BranchTypes' family
  for a type when either 'genericDissect' or 'genericUnifyWithIndex' are used.

  For example:

  @
    type instance BranchTypes MyType = GenericBranchTypes (Rep MyType)
  @
-}
type family GenericBranchTypes (rep :: Type -> Type) :: [Type] where
  GenericBranchTypes (M1 i c f) = GenericBranchTypes f
  GenericBranchTypes (a :+: b) = Append (GenericBranchTypes a) (GenericBranchTypes b)
  GenericBranchTypes (K1 i c) = '[c]
  GenericBranchTypes U1 = '[()]

{- |
  'GenericUnion' provides sum-type analysis for GHC Generics representation of
  sum types. By deriving 'Generic' for your traditional Haskell sum types you
  can the use 'genericUnifyWithIndex', 'genericDissect' and
  'GenericBranchTypes' in your implementations of 'Unification', 'Dissection'
  and 'BranchTypes.

  Warning: See the discussion in the module documentation for a reason you
  might not actually want to do this.
-}
class GenericUnion (rep :: Type -> Type) where
  -- |
  --    'genericToUnion' allows a sum type with a 'Generic' representation of
  --    multiple single-argument constructors to be converted to a 'Union' of its
  --    'GenericBranchTypes'.
  --
  --    Warning: See the discussion in the module documentation for a reason you
  --    might not actually want to do this.
  genericToUnion :: rep p -> Union (GenericBranchTypes rep)

  constructorForIndex :: BranchIndex t (GenericBranchTypes rep) -> t -> rep p

instance GenericUnion f => GenericUnion (M1 i c f) where
  genericToUnion = genericToUnion . unM1
  constructorForIndex index = M1 . constructorForIndex index

instance GenericUnion (K1 i c) where
  genericToUnion k1 =
    -- 'firstIndexOfType' is always 0 here because there is only one type in
    -- the list
    Union firstIndexOfType (unK1 k1)

  {-
    The type in this case is
    @BranchIndex t (GenericBranchTypes (K1 i c)) -> t -> K1 i c p@

    Which expands to
    @BranchIndex t '[c] -> t -> K1 i c p@

    Given that a @BranchIndex@ can only be constructed when the type it
    picks out is actually in the list of types being indexes, we can conclude
    that @t ~ c@, even though GHC cannot conclude this.

    This and the fact that the type of @K1@ is @c -> K1 i c p@ is enough to
    allow us to conclude @K1@ has the required type and @unsafeCoerce@ is, in
    fact, safe here.
  -}
  constructorForIndex _ = unsafeCoerce K1

instance GenericUnion U1 where
  genericToUnion _ =
    -- 'firstIndexOfType' is always 0 here because there is only one type in
    -- the list
    Union firstIndexOfType ()

  {-
    Ideally the implementation could be @\() -> U1@ to enforce that the type of
    the parameter being ignored is @()@ rather than another type that contains
    some data. We cannot do that here without using @unsafeCoerce@ because the
    compiler cannot conclude `t ~ ()` for the same reasons described above for
    the @K1@ implementation of @constructorForIndex@. In this case we can avoid
    using @unsafeCoerce@ by instead ignoring the parameter being passed to the
    constructor entirely.
  -}
  constructorForIndex _ = const U1

instance (GenericUnion a, GenericUnion b, KnownLength (GenericBranchTypes a)) => GenericUnion (a :+: b) where
  genericToUnion genericSum =
    case genericSum of
      L1 left ->
        let
          typesProxy = rightTypesProxy genericSum
        in
          case genericToUnion left of
            Union index param ->
              Union (appendTypesToIndex index typesProxy) param
      R1 right ->
        let
          typesProxy = leftTypesProxy genericSum
        in
          case genericToUnion right of
            Union index param ->
              Union (prependTypesToIndex index typesProxy) param

  constructorForIndex index =
    case splitIndex index of
      Left leftIndex -> L1 . constructorForIndex leftIndex
      Right rightIndex -> R1 . constructorForIndex rightIndex

rightTypesProxy :: (a :+: b) p -> Proxy (GenericBranchTypes b)
rightTypesProxy _ =
  Proxy

leftTypesProxy :: (a :+: b) p -> Proxy (GenericBranchTypes a)
leftTypesProxy _ =
  Proxy
