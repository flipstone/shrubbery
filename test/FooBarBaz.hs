{-# OPTIONS_GHC -fplugin=Shrubbery.Plugin #-}
-- TODO: Why did this option being in `Main` not seem to run the core
-- plugin? Was I just being dumb?
module FooBarBaz
  ( FooBarBaz(..)
  ) where

import Shrubbery.Plugin (DeriveMatchable(..))

{-# ANN type FooBarBaz DeriveMatchable #-}
-- | This is defined in a separate file so that DeriveMatchable can
-- be consumed in the core step before the instance is needed for
-- type checking.
data FooBarBaz
  = Foo Int
  | Bar String
  | Baz
  deriving (Show, Eq)

