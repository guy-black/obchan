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
  \ (id SERIAL PRIMARY KEY, image BOOL NOT NULL, content TEXT, op INTEGER, datetime TIMESTAMP)"

--tfw something goes wrong and you just want to go hom
whoopsie :: Snap ()
whoopsie =
  S.redirect $
    encodeUtf8 $
      renderFrontendRoute R.checkedFullRouteEncoder $
        R.FrountedRoute_Main :/ ()

backend :: Backend BackendRoute FrontendRoute
backend = Backend
--_backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  { _backend_run = \serve -> do -- \serve :: (R backendRoute -> Snap ()) -> IO ()
      withDb "db" $ \pool -> do -- \pool :: Pool Connection -> IO a
        --pool is a Pool Connection, dbcon is the Connection
        _ <- withResource pool $ \dbcon -> execute_ dbcon migration
        serve $ \case -- \case::(R backendRoute -> Snap ())
          R.BackendRoute_NewTh :/ () -> do
            thread <- A.decode <$> S.readRequestBody maxPasteSize
            \case (thread :: Maybe PostRequest) of
              (Just (PostRequest True (Just t) Nothing)) -> -- checks if it has text and picture and no OP
                [[key]] <- liftIO $
                  withResource pool $ \dbcon ->
                    query dbcon "INSERT INTO posts (image,content,op) VALUES (TRUE,?,NULL) RETURNING id" [t :: Text] 
                S.modifyResponse $ S.setResponseStatus 200 "OK"
                S.writeBS $ toStrict $ A.encode (key :: Int)
              _ -> whoopsie           
          R.BackendRoute_NewCom :/ () -> do
            comment <- A.decode <$> S.readRequestBody maxPasteSize
            \case (comment :: Maybe PostRequest) of
              Nothing -> --incase decode failed -- reject
              (Just (PostRequest _ _ Nothing)) -> --in case the comment has no OP value -- reject
              (Just (PostRequest False Nothing _)) -> --in case image is false AND content is blank -- reject
              (Just (PostRequest i c (Just o))) -> --not Nothing, has an Op, image is true and/or content is there -- accept 
          R.BackendRoute_ListPosts :/ () -> do
            postOrder <- A.decode <$> S.readRequestBody maxPostSize --todo: it probably doesnt need a max 10000 bytes but I want to lessen the initial list of things I did wrong, will fix later
            \case (postOrder :: Maybe (Int,Int)) of -- (placeinthreadlist,numberofpoststoshow)
              (Just (p,n)) ->
                postList <- liftIO $
                  withResource pool $ \dbcon ->
                    -- select * from posts Where OP is Null; sort by most recent, skip the first p, return the next n
                    query dbcon "SELECT (id,image,content,datetime) FROM posts WHERE op = NULL\
                      \ ORDER BY id DESC OFFSET ? LIMIT ?" (p :: Int,n :: Int)
                case (postOrder :: [(Int64,Bool,Text,Text)]) of
                  [(id,i,c,d)] -> S.writeBS $ toStrict $ A.encode $ [PostResponse id i c Nothing d]
                  _ -> whoopsie
              _ -> whoopsie
          R.BackendRoute_GetPost :/ key -> do
            result <- liftIO $
              withResource pool $ \dbcon ->
                query dbcon "SELECT (image,content,op,datetime) FROM posts WHERE id = ?" [unId key]
            case (result :: [(Bool,(Maybe Text),(Maybe Int64),Text)]) of
              [(i,c,o,d)] -> S.writeBS $ toStrict $ A.encode $ PostResponse key i c o d   --if the post is real
              _ -> S.modifyResponse $ S.setResponseStatus 404 "Not Found" --if its not found   
          _ -> whoopsie
      return () -- -> IO ()
  , _backend_routeEncoder = R.fullRouteEncoder
  }


--when sending a query the values in the question mark are followed in a tuple, or singleton if only one
--results from query come as a list of tuples, or list of singletons if only requesting one column
