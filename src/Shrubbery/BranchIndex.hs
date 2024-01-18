{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
  This module provides types and functions for finding and using the index
  of a type within a type-level list.
-}
module Shrubbery.BranchIndex
  ( BranchIndex
  , firstIndexOfType
  , indexOfTypeAt
  , branchIndexToInt
  , appendTypesToIndex
  , prependTypesToIndex
  , splitIndex
  , TypeZipper
  , startZipper
  , moveZipperNext
  , indexOfFocusedType
  , index0
  , index1
  , index2
  , index3
  , index4
  , index5
  , index6
  , index7
  , index8
  , index9
  , index10
  , index11
  , index12
  , index13
  , index14
  , index15
  , index16
  , index17
  , index18
  , index19
  , index20
  , index21
  , index22
  , index23
  , index24
  , index25
  , index26
  , index27
  , index28
  , index29
  , index30
  , index31
  , index32
  , index33
  , index34
  , index35
  , index36
  , index37
  , index38
  , index39
  , index40
  , index41
  , index42
  , index43
  , index44
  , index45
  , index46
  , index47
  , index48
  , index49
  , index50
  , index51
  , index52
  , index53
  , index54
  , index55
  , index56
  , index57
  , index58
  , index59
  , index60
  , index61
  , index62
  , index63
  , index64
  , index65
  , index66
  , index67
  , index68
  , index69
  , index70
  , index71
  , index72
  , index73
  , index74
  , index75
  , index76
  , index77
  , index78
  , index79
  , index80
  , index81
  , index82
  , index83
  , index84
  , index85
  , index86
  , index87
  , index88
  , index89
  , index90
  , index91
  , index92
  , index93
  , index94
  , index95
  , index96
  , index97
  , index98
  , index99
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal)

import Shrubbery.TypeList (Append, FirstIndexOf, KnownLength (lengthOfTypes), TypeAtIndex, ZippedTypes)

{- |
  A 'BranchIndex' is an zero-based index into a list of types for which
  the type at the index is known. This type is used with 'Shrubbery.Branches'
  to perform efficient branching at runtime.
-}
newtype BranchIndex t (types :: [Type])
  = BranchIndex Int

{- |
  Finds the first index of a type in a list of types. The type list and the
  type being searched for must be inferrable based on the usage, or be
  supplied via @TypeApplications@.
-}
firstIndexOfType ::
  ( KnownNat branchIndex
  , branchIndex ~ FirstIndexOf t types
  ) =>
  BranchIndex t types
firstIndexOfType =
  firstIndexOfTypeByProxy Proxy

firstIndexOfTypeByProxy ::
  ( KnownNat branchIndex
  , branchIndex ~ FirstIndexOf t types
  ) =>
  Proxy branchIndex ->
  BranchIndex t types
firstIndexOfTypeByProxy =
  BranchIndex . fromInteger . natVal

{- |
  Builds an index based on an known index value from the type system,
  associating it with the appropriate type from the list. The index is
  specified via a proxy value.
-}
indexOfTypeAt ::
  (KnownNat branchIndex, t ~ TypeAtIndex branchIndex types) =>
  proxy branchIndex ->
  BranchIndex t types
indexOfTypeAt =
  BranchIndex . fromInteger . natVal

{- |
  Retrieves the 'Int' representation of the branch index. It it up to the
  caller to use this integer in a responsible fashion ;)
-}
branchIndexToInt :: BranchIndex t types -> Int
branchIndexToInt (BranchIndex n) =
  n

{- |
  Appends additional types to the end of the list of types for a branch index.
  Because the original types are a prefix of the result types list the index of
  the type being referenced remains the same.
-}
appendTypesToIndex ::
  BranchIndex t types ->
  Proxy moreTypes ->
  BranchIndex t (Append types moreTypes)
appendTypesToIndex (BranchIndex n) _ =
  -- Appending types does not change the index of our type into the list
  BranchIndex n

{- |
  Prepends additional types to the front of the list of types for a branch
  index. The length of the list of types being prepended is added to the index
  so that the type referenced by the index in the resulting list remains the
  same.
-}
prependTypesToIndex ::
  KnownLength moreTypes =>
  BranchIndex t types ->
  Proxy moreTypes ->
  BranchIndex t (Append moreTypes types)
prependTypesToIndex (BranchIndex n) moreTypes =
  -- Prepending types pushes the index out by however many types were added
  BranchIndex (n + lengthOfTypes moreTypes)

{- |
  Examines a branch index to find whether it lands in the in the first or
  second part of an appened list of types. The returned index is adjusted as
  necessary so that points at the correct type in either the first or second
  section of the list.
-}
splitIndex ::
  forall typesA typesB t.
  KnownLength typesA =>
  BranchIndex t (Append typesA typesB) ->
  Either (BranchIndex t typesA) (BranchIndex t typesB)
splitIndex (BranchIndex n) =
  let
    m = lengthOfTypes (Proxy :: Proxy typesA)
  in
    if n < m
      then Left (BranchIndex n)
      else Right (BranchIndex (n - m))

{- |
  'TypeZipper' facilitates interating through a list of known types while
  keeping track of the index as you go. You might need this if you're keeping
  track of a list of operations that use shrubbery functions that require a
  branch index, such as 'Shrubbery.Classes.unifyWithIndex'.

  Note: Types are added to the top of the @front@ list as the are encountered,
  so the list may be reversed from you expected. See the type of 'nextZipper'.
-}
newtype TypeZipper (front :: [Type]) (focus :: Type) (back :: [Type])
  = TypeZipper Int

{- |
  Initializes a 'TypeZipper' at the start of the type list
-}
startZipper :: TypeZipper '[] first back
startZipper =
  TypeZipper 0

{- |
  Moves the zipper to the next type in the list. The currently focused item
  is added to the to the beginning of the @front@ list.
-}
moveZipperNext ::
  TypeZipper front focus (next : back) ->
  TypeZipper (focus : front) next rest
moveZipperNext (TypeZipper n) =
  TypeZipper (n + 1)

{- |
  Builds a 'BranchIndex' for the currently focused type of the zipper. This
  index can then be used with other functions such as
  'Shrubbery.Classes.unifyWithIndex'.
-}
indexOfFocusedType ::
  TypeZipper front focus back ->
  BranchIndex t (ZippedTypes front focus back)
indexOfFocusedType (TypeZipper n) =
  BranchIndex n

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 0)@
index0 :: t ~ TypeAtIndex 0 types => BranchIndex t types
index0 = indexOfTypeAt (Proxy :: Proxy 0)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 1)@
index1 :: t ~ TypeAtIndex 1 types => BranchIndex t types
index1 = indexOfTypeAt (Proxy :: Proxy 1)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 2)@
index2 :: t ~ TypeAtIndex 2 types => BranchIndex t types
index2 = indexOfTypeAt (Proxy :: Proxy 2)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 3)@
index3 :: t ~ TypeAtIndex 3 types => BranchIndex t types
index3 = indexOfTypeAt (Proxy :: Proxy 3)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 4)@
index4 :: t ~ TypeAtIndex 4 types => BranchIndex t types
index4 = indexOfTypeAt (Proxy :: Proxy 4)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 5)@
index5 :: t ~ TypeAtIndex 5 types => BranchIndex t types
index5 = indexOfTypeAt (Proxy :: Proxy 5)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 6)@
index6 :: t ~ TypeAtIndex 6 types => BranchIndex t types
index6 = indexOfTypeAt (Proxy :: Proxy 6)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 7)@
index7 :: t ~ TypeAtIndex 7 types => BranchIndex t types
index7 = indexOfTypeAt (Proxy :: Proxy 7)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 8)@
index8 :: t ~ TypeAtIndex 8 types => BranchIndex t types
index8 = indexOfTypeAt (Proxy :: Proxy 8)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 9)@
index9 :: t ~ TypeAtIndex 9 types => BranchIndex t types
index9 = indexOfTypeAt (Proxy :: Proxy 9)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 10)@
index10 :: t ~ TypeAtIndex 10 types => BranchIndex t types
index10 = indexOfTypeAt (Proxy :: Proxy 10)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 11)@
index11 :: t ~ TypeAtIndex 11 types => BranchIndex t types
index11 = indexOfTypeAt (Proxy :: Proxy 11)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 12)@
index12 :: t ~ TypeAtIndex 12 types => BranchIndex t types
index12 = indexOfTypeAt (Proxy :: Proxy 12)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 13)@
index13 :: t ~ TypeAtIndex 13 types => BranchIndex t types
index13 = indexOfTypeAt (Proxy :: Proxy 13)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 14)@
index14 :: t ~ TypeAtIndex 14 types => BranchIndex t types
index14 = indexOfTypeAt (Proxy :: Proxy 14)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 15)@
index15 :: t ~ TypeAtIndex 15 types => BranchIndex t types
index15 = indexOfTypeAt (Proxy :: Proxy 15)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 16)@
index16 :: t ~ TypeAtIndex 16 types => BranchIndex t types
index16 = indexOfTypeAt (Proxy :: Proxy 16)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 17)@
index17 :: t ~ TypeAtIndex 17 types => BranchIndex t types
index17 = indexOfTypeAt (Proxy :: Proxy 17)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 18)@
index18 :: t ~ TypeAtIndex 18 types => BranchIndex t types
index18 = indexOfTypeAt (Proxy :: Proxy 18)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 19)@
index19 :: t ~ TypeAtIndex 19 types => BranchIndex t types
index19 = indexOfTypeAt (Proxy :: Proxy 19)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 20)@
index20 :: t ~ TypeAtIndex 20 types => BranchIndex t types
index20 = indexOfTypeAt (Proxy :: Proxy 20)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 21)@
index21 :: t ~ TypeAtIndex 21 types => BranchIndex t types
index21 = indexOfTypeAt (Proxy :: Proxy 21)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 22)@
index22 :: t ~ TypeAtIndex 22 types => BranchIndex t types
index22 = indexOfTypeAt (Proxy :: Proxy 22)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 23)@
index23 :: t ~ TypeAtIndex 23 types => BranchIndex t types
index23 = indexOfTypeAt (Proxy :: Proxy 23)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 24)@
index24 :: t ~ TypeAtIndex 24 types => BranchIndex t types
index24 = indexOfTypeAt (Proxy :: Proxy 24)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 25)@
index25 :: t ~ TypeAtIndex 25 types => BranchIndex t types
index25 = indexOfTypeAt (Proxy :: Proxy 25)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 26)@
index26 :: t ~ TypeAtIndex 26 types => BranchIndex t types
index26 = indexOfTypeAt (Proxy :: Proxy 26)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 27)@
index27 :: t ~ TypeAtIndex 27 types => BranchIndex t types
index27 = indexOfTypeAt (Proxy :: Proxy 27)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 28)@
index28 :: t ~ TypeAtIndex 28 types => BranchIndex t types
index28 = indexOfTypeAt (Proxy :: Proxy 28)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 29)@
index29 :: t ~ TypeAtIndex 29 types => BranchIndex t types
index29 = indexOfTypeAt (Proxy :: Proxy 29)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 30)@
index30 :: t ~ TypeAtIndex 30 types => BranchIndex t types
index30 = indexOfTypeAt (Proxy :: Proxy 30)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 31)@
index31 :: t ~ TypeAtIndex 31 types => BranchIndex t types
index31 = indexOfTypeAt (Proxy :: Proxy 31)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 32)@
index32 :: t ~ TypeAtIndex 32 types => BranchIndex t types
index32 = indexOfTypeAt (Proxy :: Proxy 32)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 33)@
index33 :: t ~ TypeAtIndex 33 types => BranchIndex t types
index33 = indexOfTypeAt (Proxy :: Proxy 33)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 34)@
index34 :: t ~ TypeAtIndex 34 types => BranchIndex t types
index34 = indexOfTypeAt (Proxy :: Proxy 34)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 35)@
index35 :: t ~ TypeAtIndex 35 types => BranchIndex t types
index35 = indexOfTypeAt (Proxy :: Proxy 35)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 36)@
index36 :: t ~ TypeAtIndex 36 types => BranchIndex t types
index36 = indexOfTypeAt (Proxy :: Proxy 36)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 37)@
index37 :: t ~ TypeAtIndex 37 types => BranchIndex t types
index37 = indexOfTypeAt (Proxy :: Proxy 37)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 38)@
index38 :: t ~ TypeAtIndex 38 types => BranchIndex t types
index38 = indexOfTypeAt (Proxy :: Proxy 38)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 39)@
index39 :: t ~ TypeAtIndex 39 types => BranchIndex t types
index39 = indexOfTypeAt (Proxy :: Proxy 39)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 40)@
index40 :: t ~ TypeAtIndex 40 types => BranchIndex t types
index40 = indexOfTypeAt (Proxy :: Proxy 40)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 41)@
index41 :: t ~ TypeAtIndex 41 types => BranchIndex t types
index41 = indexOfTypeAt (Proxy :: Proxy 41)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 42)@
index42 :: t ~ TypeAtIndex 42 types => BranchIndex t types
index42 = indexOfTypeAt (Proxy :: Proxy 42)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 43)@
index43 :: t ~ TypeAtIndex 43 types => BranchIndex t types
index43 = indexOfTypeAt (Proxy :: Proxy 43)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 44)@
index44 :: t ~ TypeAtIndex 44 types => BranchIndex t types
index44 = indexOfTypeAt (Proxy :: Proxy 44)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 45)@
index45 :: t ~ TypeAtIndex 45 types => BranchIndex t types
index45 = indexOfTypeAt (Proxy :: Proxy 45)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 46)@
index46 :: t ~ TypeAtIndex 46 types => BranchIndex t types
index46 = indexOfTypeAt (Proxy :: Proxy 46)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 47)@
index47 :: t ~ TypeAtIndex 47 types => BranchIndex t types
index47 = indexOfTypeAt (Proxy :: Proxy 47)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 48)@
index48 :: t ~ TypeAtIndex 48 types => BranchIndex t types
index48 = indexOfTypeAt (Proxy :: Proxy 48)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 49)@
index49 :: t ~ TypeAtIndex 49 types => BranchIndex t types
index49 = indexOfTypeAt (Proxy :: Proxy 49)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 50)@
index50 :: t ~ TypeAtIndex 50 types => BranchIndex t types
index50 = indexOfTypeAt (Proxy :: Proxy 50)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 51)@
index51 :: t ~ TypeAtIndex 51 types => BranchIndex t types
index51 = indexOfTypeAt (Proxy :: Proxy 51)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 52)@
index52 :: t ~ TypeAtIndex 52 types => BranchIndex t types
index52 = indexOfTypeAt (Proxy :: Proxy 52)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 53)@
index53 :: t ~ TypeAtIndex 53 types => BranchIndex t types
index53 = indexOfTypeAt (Proxy :: Proxy 53)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 54)@
index54 :: t ~ TypeAtIndex 54 types => BranchIndex t types
index54 = indexOfTypeAt (Proxy :: Proxy 54)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 55)@
index55 :: t ~ TypeAtIndex 55 types => BranchIndex t types
index55 = indexOfTypeAt (Proxy :: Proxy 55)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 56)@
index56 :: t ~ TypeAtIndex 56 types => BranchIndex t types
index56 = indexOfTypeAt (Proxy :: Proxy 56)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 57)@
index57 :: t ~ TypeAtIndex 57 types => BranchIndex t types
index57 = indexOfTypeAt (Proxy :: Proxy 57)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 58)@
index58 :: t ~ TypeAtIndex 58 types => BranchIndex t types
index58 = indexOfTypeAt (Proxy :: Proxy 58)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 59)@
index59 :: t ~ TypeAtIndex 59 types => BranchIndex t types
index59 = indexOfTypeAt (Proxy :: Proxy 59)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 60)@
index60 :: t ~ TypeAtIndex 60 types => BranchIndex t types
index60 = indexOfTypeAt (Proxy :: Proxy 60)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 61)@
index61 :: t ~ TypeAtIndex 61 types => BranchIndex t types
index61 = indexOfTypeAt (Proxy :: Proxy 61)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 62)@
index62 :: t ~ TypeAtIndex 62 types => BranchIndex t types
index62 = indexOfTypeAt (Proxy :: Proxy 62)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 63)@
index63 :: t ~ TypeAtIndex 63 types => BranchIndex t types
index63 = indexOfTypeAt (Proxy :: Proxy 63)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 64)@
index64 :: t ~ TypeAtIndex 64 types => BranchIndex t types
index64 = indexOfTypeAt (Proxy :: Proxy 64)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 65)@
index65 :: t ~ TypeAtIndex 65 types => BranchIndex t types
index65 = indexOfTypeAt (Proxy :: Proxy 65)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 66)@
index66 :: t ~ TypeAtIndex 66 types => BranchIndex t types
index66 = indexOfTypeAt (Proxy :: Proxy 66)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 67)@
index67 :: t ~ TypeAtIndex 67 types => BranchIndex t types
index67 = indexOfTypeAt (Proxy :: Proxy 67)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 68)@
index68 :: t ~ TypeAtIndex 68 types => BranchIndex t types
index68 = indexOfTypeAt (Proxy :: Proxy 68)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 69)@
index69 :: t ~ TypeAtIndex 69 types => BranchIndex t types
index69 = indexOfTypeAt (Proxy :: Proxy 69)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 70)@
index70 :: t ~ TypeAtIndex 70 types => BranchIndex t types
index70 = indexOfTypeAt (Proxy :: Proxy 70)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 71)@
index71 :: t ~ TypeAtIndex 71 types => BranchIndex t types
index71 = indexOfTypeAt (Proxy :: Proxy 71)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 72)@
index72 :: t ~ TypeAtIndex 72 types => BranchIndex t types
index72 = indexOfTypeAt (Proxy :: Proxy 72)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 73)@
index73 :: t ~ TypeAtIndex 73 types => BranchIndex t types
index73 = indexOfTypeAt (Proxy :: Proxy 73)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 74)@
index74 :: t ~ TypeAtIndex 74 types => BranchIndex t types
index74 = indexOfTypeAt (Proxy :: Proxy 74)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 75)@
index75 :: t ~ TypeAtIndex 75 types => BranchIndex t types
index75 = indexOfTypeAt (Proxy :: Proxy 75)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 76)@
index76 :: t ~ TypeAtIndex 76 types => BranchIndex t types
index76 = indexOfTypeAt (Proxy :: Proxy 76)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 77)@
index77 :: t ~ TypeAtIndex 77 types => BranchIndex t types
index77 = indexOfTypeAt (Proxy :: Proxy 77)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 78)@
index78 :: t ~ TypeAtIndex 78 types => BranchIndex t types
index78 = indexOfTypeAt (Proxy :: Proxy 78)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 79)@
index79 :: t ~ TypeAtIndex 79 types => BranchIndex t types
index79 = indexOfTypeAt (Proxy :: Proxy 79)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 80)@
index80 :: t ~ TypeAtIndex 80 types => BranchIndex t types
index80 = indexOfTypeAt (Proxy :: Proxy 80)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 81)@
index81 :: t ~ TypeAtIndex 81 types => BranchIndex t types
index81 = indexOfTypeAt (Proxy :: Proxy 81)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 82)@
index82 :: t ~ TypeAtIndex 82 types => BranchIndex t types
index82 = indexOfTypeAt (Proxy :: Proxy 82)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 83)@
index83 :: t ~ TypeAtIndex 83 types => BranchIndex t types
index83 = indexOfTypeAt (Proxy :: Proxy 83)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 84)@
index84 :: t ~ TypeAtIndex 84 types => BranchIndex t types
index84 = indexOfTypeAt (Proxy :: Proxy 84)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 85)@
index85 :: t ~ TypeAtIndex 85 types => BranchIndex t types
index85 = indexOfTypeAt (Proxy :: Proxy 85)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 86)@
index86 :: t ~ TypeAtIndex 86 types => BranchIndex t types
index86 = indexOfTypeAt (Proxy :: Proxy 86)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 87)@
index87 :: t ~ TypeAtIndex 87 types => BranchIndex t types
index87 = indexOfTypeAt (Proxy :: Proxy 87)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 88)@
index88 :: t ~ TypeAtIndex 88 types => BranchIndex t types
index88 = indexOfTypeAt (Proxy :: Proxy 88)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 89)@
index89 :: t ~ TypeAtIndex 89 types => BranchIndex t types
index89 = indexOfTypeAt (Proxy :: Proxy 89)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 90)@
index90 :: t ~ TypeAtIndex 90 types => BranchIndex t types
index90 = indexOfTypeAt (Proxy :: Proxy 90)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 91)@
index91 :: t ~ TypeAtIndex 91 types => BranchIndex t types
index91 = indexOfTypeAt (Proxy :: Proxy 91)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 92)@
index92 :: t ~ TypeAtIndex 92 types => BranchIndex t types
index92 = indexOfTypeAt (Proxy :: Proxy 92)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 93)@
index93 :: t ~ TypeAtIndex 93 types => BranchIndex t types
index93 = indexOfTypeAt (Proxy :: Proxy 93)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 94)@
index94 :: t ~ TypeAtIndex 94 types => BranchIndex t types
index94 = indexOfTypeAt (Proxy :: Proxy 94)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 95)@
index95 :: t ~ TypeAtIndex 95 types => BranchIndex t types
index95 = indexOfTypeAt (Proxy :: Proxy 95)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 96)@
index96 :: t ~ TypeAtIndex 96 types => BranchIndex t types
index96 = indexOfTypeAt (Proxy :: Proxy 96)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 97)@
index97 :: t ~ TypeAtIndex 97 types => BranchIndex t types
index97 = indexOfTypeAt (Proxy :: Proxy 97)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 98)@
index98 :: t ~ TypeAtIndex 98 types => BranchIndex t types
index98 = indexOfTypeAt (Proxy :: Proxy 98)

-- | A handy constant for @indexOfTypeAt (Proxy :: Proxy 99)@
index99 :: t ~ TypeAtIndex 99 types => BranchIndex t types
index99 = indexOfTypeAt (Proxy :: Proxy 99)
