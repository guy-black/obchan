{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Common.Schema (Post(..))
import Database.Id.Class (Id(..))

newtype PostRequest = PostRequest {
                 _postRequest_content :: Maybe Text
                ,_postRequest_image :: Bool
                ,_postRequest_op :: Maybe (Id Post)
                }
  deriving stock Generic deriving newtype (ToJSON, FromJSON)


commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

