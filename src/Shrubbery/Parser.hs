{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : BSD3

This module provides facilities for parsing input values into sum types that implement the
'Unification' type classes. A 'Parser' is constructed by giving a handler for each type in the union
that attempts to parse the input value into that type. These handlers must be given in the same
order as the types in the type-list that describes the sum type.

For instance:

@
parser :: Parser Maybe String [Int, Bool, String]
parser =
    parseOption (readMaybe :: String -> Maybe Int)
  $ parseOption (readMaybe :: String -> Maybe Bool)
  $ parseOption (Just :: Strng -> Maybe String)
  $ parseEnd
@

Or, if you prefer to use @TypeApplications@,

@
parser :: Parser Maybe String [Int, Bool, String]
parser =
    parseOption \@Int readMaybe
  $ parseOption \@Bool readMaybe
  $ parseOption \@String Just
  $ parseEnd
@

When a 'Parser' is run via 'parse', it returns the parsing results for all the options, allowing the
caller to choose how to handle them. For instance, using the parser above one might write the
following code to pick the first successful option from the list:

@
case catMaybes (parse parser someInputString) of
  (first:_) ->
    Just first

  [] ->
    Nothing
@

@since 0.1.0.0
-}
module Shrubbery.Parser
  ( Parser
  , parse
  , parseOption
  , parseEnd
  ) where

import Data.Kind (Type)

import Shrubbery.BranchIndex (TypeZipper, indexOfFocusedType, moveZipperNext, startZipper)
import Shrubbery.Classes (BranchTypes, Unification (unifyWithIndex))
import Shrubbery.TypeList (ZippedTypes)

{- | A 'Parser' allows an input value to be examined to see whether one or more underlying types can
  be constructed from it, which are then embedded in a union. The resulting type can be any type
  that provides an instance of 'Unification', such as 'Shrubby.Union.Union'.

@since 0.1.0.0
-}
data Parser f input (types :: [Type]) where
  NilParse :: Parser f input '[]
  ConsParse :: (input -> f a) -> Parser f input rest -> Parser f input (a : rest)

{- | Represents the base case of where no parsing options are present. You can use this as the second
  argument to 'parseOption' when have no other options to specify.

@since 0.1.0.0
-}
parseEnd :: Parser f input '[]
parseEnd = NilParse

{- | Adds an option to the 'Parser'. Every option in the 'Parser' will be given a chance to examine
  the input and possibly produce a value of the type it produces. IF the option cannot produce a
  value its type from the input, it should return whatever the appropriate value for functor @f@
  is. For example, if 'Maybe' is used as @f@, the function would produce @Nothing@ when given input
  it does not want to handle.

  Any values @a@ that are produced by the function will be unified with the other values according
  the 'Unification' sum type that is expected when 'parse' is used.

@since 0.1.0.0
-}
parseOption ::
  forall a f input rest.
  (input -> f a) ->
  Parser f input rest ->
  Parser f input (a : rest)
parseOption = ConsParse

{- | Executes a 'Parser' with a given input value, producing the list of options returned be each
  function given to 'parseOption'. The returned list will always have a number of items equal to the
  number of options added to the 'Parser'.

  The caller must decide how to choose one or more results from the list of options however is
  desired.

@since 0.1.0.0
-}
parse ::
  (Unification sumType, Functor f) =>
  Parser f input (BranchTypes sumType) ->
  input ->
  [f sumType]
parse parser input =
  case parser of
    NilParse ->
      []
    ConsParse tryDiff diffRest ->
      parseZipper tryDiff diffRest startZipper input

{- | This internal function uses a 'TypeZipper' to track the index the options being processed so that
  'unifyWithIndex' can be called appropriately. This ensures that if two options produce the same
  type they are embedded in the union with the appropriate index and can later be differentiated.
-}
parseZipper ::
  ( Unification sumType
  , BranchTypes sumType ~ ZippedTypes front a back
  , Functor f
  ) =>
  (input -> f a) ->
  Parser f input back ->
  TypeZipper front a back ->
  input ->
  [f sumType]
parseZipper f parseRest zipper input =
  let
    item =
      unifyWithIndex (indexOfFocusedType zipper) <$> f input

    rest =
      case parseRest of
        NilParse ->
          []
        ConsParse nextF restOfRest ->
          parseZipper nextF restOfRest (moveZipperNext zipper) input
  in
    item : rest
