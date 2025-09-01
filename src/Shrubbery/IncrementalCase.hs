{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Shrubbery.IncrementalCase
Copyright   : Flipstone Technology Partners 2025
License     : BSD3
Maintainer  : you@example.com
Stability   : experimental
Portability : portable
-}
module Shrubbery.IncrementalCase
  ( FooBarBaz (..)
  , normalCase
  , incrementalCase
  ) where

import Shrubbery.TH (deriveMatchable)
import Shrubbery.TypeLevel (Case (..), Cases, Matchable (..), match, matchCall, matchDone)

-- | @since 1
data FooBarBaz
  = Foo Int
  | Bar String
  | Baz

$(deriveMatchable ''FooBarBaz)

-- | @since 1
normalCase :: FooBarBaz -> String
normalCase fooBarBaz =
  case fooBarBaz of
    Foo i -> show i
    Bar s -> s
    Baz -> "Baz"

-- | @since 1
incrementalCase :: FooBarBaz -> String
incrementalCase =
  matchDo @FooBarBaz
    . match @"Foo" show
    . match @"Bar" id
    . match @"Baz" (const "Baz")
    $ matchDone
