{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : MIT

This module provides type families that are useful for working with type-level lists. Usually you
  won't need to use these types directly as while using this package, but they may occasionally come
  in useful if you find yourself writing your own helper functions.

@since 0.1.0.0
-}
module Shrubbery.TypeList
  ( FirstIndexOf
  , FirstIndexOfWithMsg
  , TypeAtIndex
  , TypeAtIndexWithMsg
  , Append
  , KnownLength (..)
  , NotAMemberMsg
  , OutOfBoundsMsg
  , Length
  , ZippedTypes
  , Tag (..)
  , type (@=)
  , TaggedTypes
  , NotAMemberTagMsg
  , LookupTag
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, Symbol, TypeError, natVal, type (+), type (-))

{- | This type finds the first index of a type in the given list as a type-level natural. It is
  implemented as a type synonym around 'FirstIndexOfWithMsg' to provide a nice error message when
  the given type is not found in the list.

@since 0.1.0.0
-}
type FirstIndexOf t types =
  FirstIndexOfWithMsg t types (NotAMemberMsg t types)

{- | Finds the first index of a type in the given list as a type-level natural.

  The last argument of the type family is the error message that will be given if the type is not
  found. Normally you will just use the 'FirstIndexOf' synonym, but you can use this instead if you
  want to provide a customized error message.

@since 0.1.0.0
-}
type family FirstIndexOfWithMsg t (types :: [Type]) (errMsg :: ErrorMessage) where
  FirstIndexOfWithMsg _ '[] errMsg = TypeError errMsg
  FirstIndexOfWithMsg t (t : _) _ = 0
  FirstIndexOfWithMsg t (_ : rest) errMsg = 1 + FirstIndexOfWithMsg t rest errMsg

{- | This type finds the type at the given index in a list of types. It is implemented as a type
  synonym to provide a nice error message when the given type is not found in the list.

@since 0.1.0.0
-}
type TypeAtIndex n types =
  FoundAtIndex n types (TypeAtIndexMaybe n types)

type family TypeAtIndexMaybe (n :: Nat) (types :: [Type]) :: Maybe Type where
  TypeAtIndexMaybe _ '[] = 'Nothing
  TypeAtIndexMaybe 0 (t : _) = 'Just t
  TypeAtIndexMaybe n (_ : rest) = TypeAtIndexMaybe (n - 1) rest

type family FoundAtIndex (n :: Nat) (types :: [Type]) (found :: Maybe Type) :: Type where
  FoundAtIndex n types 'Nothing = TypeError (OutOfBoundsMsg n types)
  FoundAtIndex _ _ ('Just t) = t

{- | Finds the type at the given index in a list of types.

  The last argument of the type family is the error message that will be given if the type is not
  found. Normally you will just use the 'TypeAtIndex' synonym, but you can use this instead if you
  want to provide a customized error message.

@since 0.1.0.0
-}
type family TypeAtIndexWithMsg (n :: Nat) (types :: [Type]) (errMsg :: ErrorMessage) where
  TypeAtIndexWithMsg _ '[] errMsg = TypeError errMsg
  TypeAtIndexWithMsg 0 (t : _) _ = t
  TypeAtIndexWithMsg n (_ : rest) errMsg = TypeAtIndexWithMsg (n - 1) rest errMsg

{- | Appends two type-level lists of form a single list.

@since 0.1.3.0
-}
type family Append (front :: [k]) (back :: [k]) :: [k] where
  Append '[] back = back
  Append (a : rest) back = a : Append rest back

{- | This is the default error message used by 'FirstIndexOf' when the type is not found.

@since 0.1.0.0
-}
type NotAMemberMsg :: forall t. t -> [Type] -> ErrorMessage
type NotAMemberMsg t (types :: [Type]) =
  ( ShowType t
      :<>: Text " is not a member of "
      :<>: ShowType types
  )

{- | This is the default error message used by 'TypeAtIndex' when the index is out of bounds.

@since 0.1.0.0
-}
type OutOfBoundsMsg (index :: Nat) (types :: [Type]) =
  ( Text "Index "
      :<>: ShowType index
      :<>: Text " is out of bounds for the type list of length "
      :<>: ShowType (Length types)
      :<>: Text ": "
      :<>: ShowType types
  )

{- | Similar to 'KnownNat', this class allows length of a type list that is known at compile time to
  be retrieved at runtime. This functionality is exported as a class rather than a family both to
  simplify the type signatures of functions that use this knowledge and for situations where
  consumers would need to turn on @UndecidableInstances@ to use it.

@since 0.1.0.0
-}
class KnownLength (types :: [k]) where
  lengthOfTypes :: proxy types -> Int

{- |

@since 0.1.0.0
-}
instance (KnownNat suppliedLength, suppliedLength ~ Length types) => KnownLength types where
  lengthOfTypes =
    fromInteger . natVal . typesProxyToLengthProxy

typesProxyToLengthProxy :: suppliedLength ~ Length types => proxy types -> Proxy suppliedLength
typesProxyToLengthProxy _ = Proxy

{- | Used by 'KnownLength' to calculate the length of a type-level list.

@since 0.1.0.0
-}
type family Length (types :: [k]) :: Nat where
  Length '[] = 0
  Length (_ : rest) = 1 + Length rest

type family ZippedTypes front focus back :: [k] where
  ZippedTypes '[] focus back = focus : back
  ZippedTypes (a : rest) focus back = ZippedTypes rest a (focus : back)

{- | A 'Symbol' and a 'Type' to be kept with the 'Symbol'.

@since 0.1.3.0
-}
data Tag
  = Tag Symbol Type

type (@=) = 'Tag

{- | Computes the index and type '(Nat, Type)' of the first occurrence of a tag,
  or a 'NotAMemberTagMsg' if the tag is not present in the list.

@since 0.3.0.0
-}
type LookupTag (t :: Symbol) (tags :: [Tag]) =
  FoundTag t tags (LookupTagFrom 0 t tags)

type family LookupTagFrom (index :: Nat) (t :: Symbol) (tags :: [Tag]) :: Maybe (Nat, Type) where
  LookupTagFrom _ _ '[] = 'Nothing
  LookupTagFrom index t ('Tag t typ : _) = 'Just '(index, typ)
  LookupTagFrom index t (_ : rest) = LookupTagFrom (index + 1) t rest

type family FoundTag (t :: Symbol) (tags :: [Tag]) (found :: Maybe (Nat, Type)) :: (Nat, Type) where
  FoundTag t tags 'Nothing = TypeError (NotAMemberTagMsg t tags)
  FoundTag _ _ ('Just indexAndType) = indexAndType

type family TaggedTypes (tags :: [Tag]) :: [Type] where
  TaggedTypes '[] = '[]
  TaggedTypes ('Tag _ typ : rest) = typ : TaggedTypes rest

{- | This is the default error message used by 'FirstIndexOf' when the type is not found.

@since 0.1.3.0
-}
type NotAMemberTagMsg :: forall t. t -> [Tag] -> ErrorMessage
type NotAMemberTagMsg t (tags :: [Tag]) =
  ( ShowType t
      :<>: Text " is not one of the tag symbols in "
      :<>: ShowType tags
  )
