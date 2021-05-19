{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Common.Route as R
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Database.Id.Class (Id(..), unId)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute_, Query, query)
import Gargoyle.PostgreSQL.Connect (withDb)
import Obelisk.Backend (Backend(..), _backend_run, _backend_routeEncoder)
import Obelisk.Route (renderFrontendRoute, pattern (:/))
import qualified Snap.Core as S

maxPostSize :: Word64
maxPostSize = 10000

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS posts\
  \ (id SERIAL PRIMARY KEY, image BOOL, content TEXT, op INTEGER, datetime TIMESTAMP)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> 
  , _backend_routeEncoder = R.fullRouteEncoder
  }
