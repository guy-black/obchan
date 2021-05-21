{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Common.Route as R
import Common.Api
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
maxPostSize = 10000 --in bytes

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS posts\
  \ (id SERIAL PRIMARY KEY, image BOOL, content TEXT, op INTEGER, datetime TIMESTAMP)"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
--_backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  { _backend_run = \serve -> do -- \serve :: (R backendRoute -> Snap ()) -> IO ()
      withDb "db" $ \pool -> do -- \pool :: Pool Connection -> IO a
        --pool is a Pool Connection, dbcon is the Connection
        _ <- withResource pool $ \dbcon -> execute_ dbcon migration
        serve $ \case -- \case::(R backendRoute -> Snap ())
          R.BackendRoute_NewTh :/ () -> do
            Just thread <- A.decode <$> S.readRequestBody maxPasteSize
            --todo, a function to check that content is not null, and image is true
            [[key]] <- liftIO $
              withResource pool $ \dbcon ->
                query dbcon "INSERT INTO posts (image,content,op) VALUES (?,?,?) RETURNING id" ((_postRequest_image thread) :: Bool, (_postRequest_content thread) :: (Maybe Text),Nothing :: (Maybe Int64))
            S.modifyResponse $ S.setResponseStatus 200 "OK"
            S.writeBS $ toStrict $ A.encode (key :: Int)
          R.BackendRoute_NewCom :/ () -> do
            --handle new comment
          R.BackendRoute_ListPosts :/ () -> do
            --return list of posts
          R.BackendRoute_GetPost :/ key -> do
            result <- liftIO $
              withResource pool $ \dbcon ->
                query dbcon "SELECT (image,content,op,datetime) FROM posts WHERE id = ?" [unId key]
            case (result :: (Bool,(Maybe Text),(Maybe Int64),Text)) of
              [(i,c,o,d)] -> S.writeBS $ toStrict $ A.encode $ PostResponse key i c o d   --if the post is real
              _ -> S.modifyResponse $ S.setResponseStatus 404 "Not Found" --if its not found   
          _ -> --else
            S.redirect $
              encodeUtf8 $
                renderFrontendRoute R.checkedFullRouteEncoder $
                  R.FrountedRoute_Main :/ ()
      return () -- -> IO ()
  , _backend_routeEncoder = R.fullRouteEncoder
  }


--when sending a query the values in the question mark are followed in a tuple, or singleton if only one
--results from query come as a list of tuples, or list of singletons if only requesting one column
