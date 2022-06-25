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
import GHC.Int
import Common.Schema (Post(..))
import Database.Id.Class (Id(..), HasId)
import Data.Time as Time

data PostRequest = PostRequest {
                 _postRequest_image :: Text
                ,_postRequest_content :: Text
                ,_postRequest_op :: Maybe (Id PostResponse)
                }
  deriving stock Generic deriving (ToJSON, FromJSON)


data PostFetch = PostFetch {
                  _page :: Int
                 ,_amount :: Int
                 }
  deriving stock Generic deriving (ToJSON, FromJSON)

data PostResponse = PostResponse {
                  _postResponse_id :: Int64
                 ,_postResponse_image :: Text
                 ,_postResponse_content :: Text
                 ,_postResponse_datetime :: Time.LocalTime
                 }
  deriving stock Generic deriving (ToJSON, FromJSON)

instance HasId PostResponse where

data ThreadResponse = ThreadResponse {
                    _threadResponse_op :: PostResponse
                   ,_threadResponse_replies :: [PostResponse]
                   ,_threadResponse_comCount :: Int
                   }
  deriving stock Generic deriving (ToJSON, FromJSON)
