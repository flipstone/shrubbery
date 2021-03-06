{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-|
  This modules provides functionality similar to Haskell's built-in case
  statements, but which can be built at run time without losing exhaustivity
  checking. The technique to satisfy both these desires is to use a type-level
  list of types to represent the type of value that is available in a given
  branch, which is analogous to the values you would extract while pattern
  matching a Haskell type with multiple constructors in a case statement.

  This is best understood through an example:

  @
    intStringOrBoolBranches :: Branches [Int, String, Bool] String
    intStringOrBoolBranches =
      branchBuild
      $ branch doSomethingWithInt
      $ branch doSomethingWithString
      $ branch doSomethingWithBool
      $ branchEnd
  @

  Or, if you prefer to use @TypeApplications@ to make this more explicit:

  @
    intStringOrBoolBranches :: Branches [Int, String, Bool] String
    intStringOrBoolBranches =
      branchBuild
      $ branch @Int    doSomethingWithInt
      $ branch @String doSomethingWithString
      $ branch @Bool   doSomethingWithBool
      $ branchEnd
  @


  You can then use the branch values above almost like functions to perform
  case-like analysis based on the type of value, using 'selectBranch'

  @
    >>> selectBranch intStringOrBoolBranches (1 :: Int)
    doSomethingWithInt 1

    >>> selectBranch intStringOrBoolBranches (\"Foo\")
    doSomethingWithString \"Foo\"

    >>> selectBranch intStringOrBoolBranches True
    doSomethingWithBool True
  @
-}
module Shrubbery.Branches
  ( branchBuild
  , branch
  , branchEnd

  , Branches
  , BranchBuilder

  , selectBranch
  , selectBranchAtIndex
  , selectBranchAtProxy
  ) where

import           Control.Monad.ST (ST)
import           Data.Kind (Type)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Primitive.Array as Arr
import           GHC.Exts (Any)
import           GHC.TypeLits (KnownNat)
import           Unsafe.Coerce (unsafeCoerce)

import           Shrubbery.BranchIndex (BranchIndex, branchIndexToInt, firstIndexOfType, indexOfTypeAt)
import           Shrubbery.TypeList (KnownLength(lengthOfTypes), FirstIndexOf,  TypeAtIndex)

{-|
  'Branches' contains an array of functions that have different parameter
  types, but produce the same result. The @paramTypes@ list of types indicates
  the types of the input parameters, in order.
-}
newtype Branches (paramTypes :: [Type]) result =
  Branches (Arr.Array (Any -> result))

{-|
  'BranchBuilder' is an efficient interface for building a 'Branches'.
  Use 'branchBuild' to "execute" the 'BranchBuilder' to make 'Branches'.
-}
newtype BranchBuilder (paramTypes :: [Type]) result =
  BranchBuilder (forall s. Int -> Arr.MutableArray s (Any -> result) -> ST s ())

{-|
  Selects a function out of some 'Branches' to use for a particular value. This
  function picks the first function whose parameter type matches, which is
  usually sufficient as @paramTypes@ will usually contain each type only once.

  If you need to select a particular index, use 'selectBranchAtProxy'.
-}
selectBranch :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf param paramTypes)
             => Branches paramTypes result
             -> param
             -> result
selectBranch =
  selectBranchAtIndex firstIndexOfType

{-|
  Selects the function out of 'Branches' at the given index so that it can be
  used with the correct input parameter type. The index is specified via a proxy
  value, like such:

  @
    selectBranchAtProxy (Proxy :: 1) branches
  @

  Or, with type applications:

  @
    selectBranchAtProxy @1 Proxy branches
  @
-}
selectBranchAtProxy :: (KnownNat branchIndex, param ~ TypeAtIndex branchIndex paramTypes)
                    => proxy branchIndex
                    -> Branches paramTypes result
                    -> param
                    -> result
selectBranchAtProxy proxy =
  selectBranchAtIndex (indexOfTypeAt proxy)

{-|
  Selects the function out of 'Branches' at the given index so that it can be
  used with the correct input parameter type.
-}
selectBranchAtIndex :: BranchIndex param paramTypes
                    -> Branches paramTypes result
                    -> param
                    -> result
selectBranchAtIndex branchIndex (Branches array) =
  unsafeFromBranch $
    Arr.indexArray
      array
      (branchIndexToInt branchIndex)

{-|
  From a lexical code perspective you can think of this as "beginning" a branching
  section (hence the name). It actually finalizes the building of branches to
  optimized the lookup so that branch dispatching can be done in O(1) time.
-}
branchBuild :: KnownLength paramTypes
            => BranchBuilder paramTypes result
            -> Branches paramTypes result
branchBuild builder@(BranchBuilder populateBranches) =
  Branches $
    Arr.runArray $ do
      mutArray <- Arr.newArray (lengthOfTypes $ paramTypesProxy builder) undefined
      populateBranches 0 mutArray
      pure mutArray

{-|
  Specifies how to handle a given position in a list of types. The function
  parameter type is added to the front of the list of types for the branches
  that are being constructed. This means that the branches must be specified
  (from "top" to "bottom") in the same order they are given in the list or else
  you get a compilation error.
-}
branch :: (param -> result)
       -> BranchBuilder paramTypes result
       -> BranchBuilder (param : paramTypes) result
branch branchFunction (BranchBuilder populateBranches) =
  BranchBuilder $ \branchIndex array -> do
    Arr.writeArray array branchIndex (unsafeToBranch branchFunction)
    populateBranches (branchIndex + 1) array

{-|
  Indicates that there are no more branches to specify. This must appear as
  the final entry in a sequence of 'branch' calls to handle the base case of
  an empty type list.
-}
branchEnd :: BranchBuilder '[] result
branchEnd = BranchBuilder $ \_ _ -> pure ()

paramTypesProxy :: BranchBuilder paramTypes result -> Proxy paramTypes
paramTypesProxy _ =
  Proxy

unsafeToBranch :: (a -> result) -> (Any -> result)
unsafeToBranch = unsafeCoerce

unsafeFromBranch :: (Any -> result) -> (a -> result)
unsafeFromBranch = unsafeCoerce
