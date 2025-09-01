{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright : Flipstone Technology Partners 2025
License   : BSD3
-}
module Shrubbery.TypeLevel
  ( Case (..)
  , Match (..)
  , Matchable (..)
  , match
  , matchDone
  , matchCall
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import Shrubbery.TypeList (KnownLength (lengthOfTypes))
import Unsafe.Coerce (unsafeCoerce)

-- | @since 1
data Case = Case Symbol Type

-- | @since 1
newtype Match (cases :: [Case]) result
  = Match [Any -> result]

-- | @since 1
class Matchable a where
  type Cases a :: [Case]
  matchDo :: Match (Cases a) b -> a -> b

-- | @since 1
match ::
  forall (s :: Symbol) a b (cases :: [Case]).
  (a -> b) ->
  Match cases b ->
  Match ('Case s a : cases) b
match f (Match cases) =
  Match (unsafeCoerce f : cases)

-- | @since 1
matchDone :: Match '[] result
matchDone =
  Match []

-- | @since 1
matchCall ::
  forall (s :: Symbol) t b (cases :: [Case]).
  Member s t cases =>
  Match cases b ->
  t ->
  b
matchCall (Match cases) a =
  case drop (memberIndex @s @t @cases) (reverse cases) of
    [] -> error "matchCall: index out of bounds"
    (anyF : _rest) -> unsafeCoerce anyF a

class Member (s :: Symbol) (t :: Type) (cases :: [Case]) where
  memberIndex :: Int

instance {-# OVERLAPPABLE #-} KnownLength cases => Member s t ('Case s t : cases) where
  memberIndex = lengthOfTypes (Proxy :: Proxy cases)

instance {-# OVERLAPS #-} Member s t cases => Member s t ('Case s2 t2 : cases) where
  memberIndex = memberIndex @s @t @cases
