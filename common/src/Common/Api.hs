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

data PostRequest = PostRequest {
                 _postRequest_content :: Maybe Text
                ,_postRequest_image :: Bool
                ,_postRequest_op :: Maybe (Id Post)
                }
  deriving stock Generic deriving data (ToJSON, FromJSON)


commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"


data PostResponse = PostResponse {
                  _postResponse_id :: Int64
                 ,_postResponse_image :: Bool
                 ,_postResponse_content :: (Maybe Text)
                 ,_postResponse_op :: (Maybe Int64)
                 ,_postResponse_datetime :: Text}

  deriving stock Generic deriving data (ToJSON, FromJSON) -- todo: figure out what this line means
