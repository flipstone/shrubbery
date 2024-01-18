{- |
  @shrubbery@ provides programmatic branching constructs based on type-level
  lists that can be used to write the equivalent of @case@ statements when the
  number and types of cases are not known ahead of time by the programmer.

  This is useful for writing domain-specific languages that need to do
  case analysis on sum types supplied by the user, which otherwise requires
  the user to provide a function to do the case analysis on behalf of the
  DSL.

  See 'Shrubbery.Union' for a sum type that provides the required braching
  functionality automatically.

  See 'Shrubbery.Classes' for the typeclass interface that allows types
  other than those in 'Shrubbery.Union' to provide branching functionality.

  See 'Shrubbery.Branches' for the types and functions involved in building
  and applying the case analysis itself.
-}
module Shrubbery
  ( module Export
  ) where

import Shrubbery.BranchIndex as Export
import Shrubbery.Branches as Export
import Shrubbery.Classes as Export
import Shrubbery.TaggedUnion as Export
import Shrubbery.TypeList as Export
import Shrubbery.Union as Export
