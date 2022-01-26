{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import qualified Common.Route as R
import Common.Api
-- import Common.Schema
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
-- import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Database.Id.Class (Id(..), unId)
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (execute_, Query, query)
import Gargoyle.PostgreSQL.Connect (withDb)
import Obelisk.Backend (Backend(..), _backend_run, _backend_routeEncoder)
import Obelisk.Route (renderFrontendRoute, renderBackendRoute, pattern (:/))
import qualified Snap.Core as S
import Database.PostgreSQL.Simple.Time as Ts -- LocalTimestamp
-- import Data.Time as Time --LocalTime
import GHC.Int

maxPostSize :: Word64
maxPostSize = 10000 --in bytes

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS posts (id SERIAL PRIMARY KEY, image BOOL NOT NULL, \
  \content TEXT NOT NULL, op INTEGER, datetime TIMESTAMP NOT NULL, lastAct TIMESTAMP, comCount INTEGER);\
  \CREATE OR REPLACE FUNCTION getThreadPreviews(page INTEGER, count INTEGER) RETURNS SETOF posts\n\
   \AS\n\
   \$$\n\
   \DECLARE\n\
     \op posts%rowtype;\n\
   \BEGIN\n\
     \FOR op IN\n\
     \SELECT * FROM posts WHERE posts.op is NULL ORDER BY posts.lastact desc OFFSET page LIMIT count\n\
     \LOOP\n\
       \RETURN NEXT op;\n\
       \RETURN QUERY SELECT * FROM posts WHERE posts.op = op.id ORDER BY posts.datetime desc LIMIT 3;\n\
     \END LOOP;\n\
     \RETURN;\n\
   \END;\n\
   \$$\n\
   \LANGUAGE plpgsql"

--tfw something goes wrong and you just want to go hom
whoopsie :: S.Snap ()
whoopsie =
  S.redirect $
    encodeUtf8 $
      renderFrontendRoute R.checkedFullRouteEncoder $
        R.FrontendRoute_Main :/ ()

toPostResponse :: (Int64,Bool,Text,Ts.LocalTimestamp) -> Maybe PostResponse
toPostResponse (pid, i, c, d) = case (d) of
                    Ts.Finite a -> Just (PostResponse pid i c a)
                    _ -> Nothing


-- builds up [ThreadRespone] backwards, remember to reverse at the end
toThreadResponse :: [ThreadResponse] -> [(Int64,Bool,Text,Ts.LocalTimestamp,(Maybe Int))] -> Maybe [ThreadResponse]
toThreadResponse thr prs =
  case prs of
    [] -> Just (reverse thr) -- empty list of db results, base case, return reversed thr
    (pid, i, c, d, cc):ps -> -- thread is real, start processing
      case (toPostResponse (pid, i, c, d)) of
            Nothing -> -- conversion to PostResponse Failed
              Nothing
            Just pr ->
              case cc of
                Just jcc -> -- this is an op, use to inittialize new ThreadResponse with comCount jcc
                  toThreadResponse ((ThreadResponse pr [] jcc) : thr) ps
                Nothing -> -- this is a comment, add to last ThreadResponse
                  case thr of
                    [] -> Nothing -- there should've been a ThreadResponse here
                    (ThreadResponse op rep com):ts -> --the list isn't empty, update first element and recurse
                       toThreadResponse ((ThreadResponse op (pr:rep) com):ts) ps


maybeList2List :: [Maybe a] -> [a]
maybeList2List l = case l of
  [] -> []
  a:as -> case a of
    Nothing -> maybeList2List as
    Just ja -> ja:(maybeList2List as)


backend :: Backend R.BackendRoute R.FrontendRoute
backend = Backend
--_backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  { _backend_run = \serve -> do -- \serve :: (R backendRoute -> Snap ()) -> IO ()
      withDb "db" $ \pool -> do -- \pool :: Pool Connection -> IO a
        --pool is a Pool Connection, dbcon is the Connection
        _ <- withResource pool $ \dbcon -> execute_ dbcon migration
        serve $ \case -- \case::(R backendRoute -> Snap ())
          R.BackendRoute_NewTh :/ () -> do
            thread <- A.decode <$> S.readRequestBody maxPostSize
            case (thread :: Maybe PostRequest) of
              (Just (PostRequest True t Nothing)) -> do -- checks if it has text and picture and no OP
                [[key]] <- liftIO $
                  withResource pool $ \dbcon ->
                    query dbcon "INSERT INTO posts (image,content,op,datetime,lastAct,comCount) VALUES (TRUE,?,NULL,now(),now(),0) RETURNING id" [t :: Text]
                S.modifyResponse $ S.setResponseStatus 200 "OK"
                S.writeBS $ toStrict $ A.encode (key :: Int)
              _ -> whoopsie
          R.BackendRoute_NewCom :/ () -> do
            comment <- A.decode <$> S.readRequestBody maxPostSize
            case (comment :: Maybe PostRequest) of
              Nothing -> whoopsie --incase decode failed -- reject
              (Just (PostRequest _ _ Nothing)) -> whoopsie --in case the comment has no OP value -- reject
              (Just (PostRequest False "" _)) -> whoopsie --in case image is false AND content is blank -- reject
              (Just (PostRequest i c (Just o))) -> do --not Nothing, has an Op, image is true and/or content is there -- accept
                [[thKey]] <- liftIO $
                  withResource pool $ \dbcon ->
                    query dbcon "UPDATE posts SET comCount = comCount + 1, lastAct = now() WHERE id = ?; \
                      \INSERT INTO posts (image,content,op,datetime) VALUES (?,?,?,now()) RETURNING op" (unId o,i :: Bool,c :: Text,unId o)
                S.modifyResponse $ S.setResponseStatus 200 "OK"
                S.writeBS $ toStrict $ A.encode (thKey :: Int)
          R.BackendRoute_ListPosts :/ () -> do
            postOrder <- A.decode <$> S.readRequestBody maxPostSize --todo: it probably doesnt need a max 10000 bytes but I want to lessen the initial list of things I did wrong, will fix later
            case (postOrder :: (Maybe PostFetch)) of -- (placeinthreadlist,numberofpoststoshow)
              (Just (PostFetch p n)) -> do
                postList <- liftIO $
                  withResource pool $ \dbcon ->
                    query dbcon "SELECT id,image,content,datetime,comCount FROM getThreadPreviews(?,?)" (p :: Int,n :: Int)
                case (postList :: [(Int64,Bool,Text,Ts.LocalTimestamp,(Maybe Int))]) of
                  [] -> do
                    S.modifyResponse $ S.setResponseStatus 404 "Not Found"
                    S.modifyResponse $ S.setContentType "text/plain; charset=utf8"
                    S.writeText "oops no threads ¯\\_(ツ)_/¯"
                  _ ->
                    case (toThreadResponse [] postList) of
                      Nothing -> do
                        S.modifyResponse $ S.setResponseStatus 404 "Not Found"
                        S.modifyResponse $ S.setContentType "text/plain; charset=utf8"
                        S.writeText "ThreadResponse machine broke ¯\\_(ツ)_/¯"
                      Just thrLi -> S.writeBS $ toStrict $ A.encode $ thrLi
              _ -> do
                S.modifyResponse $ S.setResponseStatus 404 "Not Found"
                S.modifyResponse $ S.setContentType "text/plain; charset=utf8"
                S.writeText "postFetch didnt get sent successfully ¯\\_(ツ)_/¯"
          R.BackendRoute_GetPost :/ key -> do
            op <- liftIO $
              withResource pool $ \dbcon ->
                query dbcon "SELECT op FROM posts WHERE id = ?" [unId key]
            case (op :: [[Maybe Int64]]) of
              [[Nothing]] -> do --this is the OP, return a list with this post first and all posts who's OP == key OR the post isnt real
                thread <- liftIO $
                  withResource pool $ \dbcon ->
                    query dbcon "SELECT id,image,content,datetime FROM posts WHERE id = ? OR op = ? ORDER BY id ASC" (unId key,unId key)
                case (thread :: [(Int64,Bool,Text,Ts.LocalTimestamp)]) of
                  [] -> do
                    S.modifyResponse $ S.setResponseStatus 404 "Not Found"
                    S.modifyResponse $ S.setContentType "text/plain; charset=utf8"
                    S.writeText "oops no commentss ¯\\_(ツ)_/¯"
                  _ -> S.writeBS $ toStrict $ A.encode $ (maybeList2List (toPostResponse <$> thread))
              [[(Just threadId)]] -> do --this is a comment, redirect to it's op
                thread <- liftIO $
                  withResource pool $ \dbcon ->
                    query dbcon "SELECT id,image,content,datetime FROM posts WHERE id = ? OR op = ? ORDER BY id ASC" (threadId, threadId)
                case (thread :: [(Int64,Bool,Text,Ts.LocalTimestamp)]) of
                  [] -> do
                    S.modifyResponse $ S.setResponseStatus 404 "Not Found"
                    S.modifyResponse $ S.setContentType "text/plain; charset=utf8"
                    S.writeText "oops no commentss ¯\\_(ツ)_/¯"
                  _ -> S.writeBS $ toStrict $ A.encode $ (maybeList2List (toPostResponse <$> thread))
              _ -> whoopsie -- shoudn't happen, just here to to remove compiler warning


          _ -> whoopsie
      return () -- -> IO ()
  , _backend_routeEncoder = R.fullRouteEncoder
  }


--when sending a query the values in the question mark are followed in a tuple, or singleton if only one
--results from query come as a list of tuples, or list of singletons if only requesting one column
