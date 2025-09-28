{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin=Shrubbery.Plugin.MyShow -fplugin=MyPlugin -Wno-unused-imports -dlint #-}
-- TODO: Why did this option being in `Main` not seem to run the core
-- plugin? Was I just being dumb?
module FooBarBaz
  ( FooBarBaz(..)
  , dissectFBB
  ) where

import Data.Proxy (Proxy(Proxy))

import Shrubbery.Plugin (DeriveMatchable(..))
import Shrubbery.TypeLevel (Case)
import Shrubbery.TypeList (type (@=))
import Shrubbery.TaggedUnion (TaggedBranches, DissectSum(..), selectTaggedBranchAtProxy)
import MyShow (MyShow(..))

-- {-# ANN type FooBarBaz DeriveMatchable #-}
-- | This is defined in a separate file so that DeriveMatchable can
-- be consumed in the core step before the instance is needed for
-- type checking.
data FooBarBaz
  = Foo Int
  | Bar String
  | Baz ()
  deriving (Eq)

type FooBarBazBranches =
  [ "Foo" @= Int
  , "Bar" @= String
  , "Baz" @= ()
  ]

dissectFBB :: TaggedBranches FooBarBazBranches a -> FooBarBaz -> a
dissectFBB = dissectSum

dissectFBB2 :: TaggedBranches FooBarBazBranches a -> FooBarBaz -> a
dissectFBB2 branches fbb =
  case fbb of
    Foo n -> selectTaggedBranchAtProxy branches (Proxy :: Proxy "Foo") n
    Bar s -> selectTaggedBranchAtProxy branches (Proxy :: Proxy "Bar") s
    Baz u -> selectTaggedBranchAtProxy branches (Proxy :: Proxy "Baz") u

instance Show FooBarBaz where
  show = myShow

