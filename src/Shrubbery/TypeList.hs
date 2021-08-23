{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  This module provides type families that are useful for working with type-level
  lists. Usually you won't need to use these type directly as while using
  this package, but they may occasionally come in useful if you find yourself
  writing your own helper functions.
-}
module Shrubbery.TypeList
  ( FirstIndexOf
  , FirstIndexOfWithMsg
  , TypeAtIndex
  , TypeAtIndexWithMsg
  , KnownLength(..)
  , NotAMemberMsg
  , OutOfBoundsMsg
  , Length
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, Nat, TypeError, ErrorMessage(..), natVal, type (+), type (-))

{-|
  This type finds the first index of a type in the given list as a type-level
  natural. It is implemented as a type-synonym around 'FirstIndexOfWithMsg' to
  provide a nice error message when the given type is not found in the list.
-}
type FirstIndexOf t types =
  FirstIndexOfWithMsg t types (NotAMemberMsg t types)

{-|
  Finds the first index of a type in the given list as a type-level
  natural.

  The last argument of the type family is the error message that will
  be given if the type is not found. Normally you will just use the 'FirstIndexOf'
  synonym, but you can use this instead if you want to provide a customized
  error message.
-}
type family FirstIndexOfWithMsg t (types :: [Type]) (errMsg :: ErrorMessage) where
  FirstIndexOfWithMsg _ '[] errMsg = TypeError errMsg
  FirstIndexOfWithMsg t (t : _) _ = 0
  FirstIndexOfWithMsg t (_ : rest) errMsg = 1 + FirstIndexOfWithMsg t rest errMsg

{-|
  This type finds the type at the given index in a list of types. It is
  implemented as a type-synonym around 'TypeAtIndexWithMsg' to provide a nice
  error message when the given type is not found in the list.
-}
type TypeAtIndex n types =
  TypeAtIndexWithMsg n types (OutOfBoundsMsg n types)

{-|
  Finds the type at the given index in a list of types.

  The last argument of the type family is the error message that will
  be given if the type is not found. Normally you will just use the 'TypeAtIndex'
  synonym, but you can use this instead if you want to provide a customized
  error message.
-}
type family TypeAtIndexWithMsg (n :: Nat) (types :: [Type]) (errMsg :: ErrorMessage) where
  TypeAtIndexWithMsg 0 '[] errMsg = TypeError errMsg
  TypeAtIndexWithMsg 0 (t : _) _ = t
  TypeAtIndexWithMsg n (_ : rest) errMsg = TypeAtIndexWithMsg (n - 1) rest errMsg

{-|
  This is the default error message used by 'FirstIndexOf' when the type is
  not found.
-}
type NotAMemberMsg t (types :: [Type]) =
  (     'ShowType t
  ':<>: 'Text " is not a member of "
  ':<>: 'ShowType types
  )

{-|
  This is the default error message used by 'TypeAtIndex' when the index is
  out of bounds.
-}
type OutOfBoundsMsg (index :: Nat) (types :: [Type]) =
  (     'Text "Index "
  ':<>: 'ShowType index
  ':<>: 'Text " is out of bounds for the type list of length "
  ':<>: 'ShowType (Length types)
  ':<>: 'Text ": "
  ':<>: 'ShowType types
  )

{-|
  Similar to 'KnownNat', this class allows length of a type list that is
  known at compile time to be retrieved at runtime. This functionality is
  experted as class rather than a family both to simply the type signatures
  of functions that use this knowledge and situations where consumers would
  need to turn on @UndecidableInstances@ to use it.
-}
class KnownLength (types :: [Type]) where
  lengthOfTypes :: proxy types -> Int

instance (KnownNat length, length ~ Length types) => KnownLength types where
  lengthOfTypes =
    fromInteger . natVal . typesProxyToLengthProxy

typesProxyToLengthProxy :: length ~ Length types => proxy types -> Proxy length
typesProxyToLengthProxy _ = Proxy

{-|
  Used by 'KnownLength' to calculate the length of a type-level list.
-}
type family Length (types :: [Type]) :: Nat where
  Length '[] = 0
  Length (_ : rest) = 1 + Length rest

