module MyShow
  ( MyShow(..) 
  , constructorName
  ) where

class MyShow a where
  myShow :: a -> String

constructorName :: String -> String
constructorName s = "Ctor: " ++ s
