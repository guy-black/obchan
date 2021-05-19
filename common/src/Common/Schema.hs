{-# LANGUAGE DeriveGeneric #-}
module Common.Schema where

import Data.Text (Text)
import Database.Id.Class
import GHC.Generics

newtype Post = Post { unPost :: Text }
  deriving Generic

instance HasId Post
