{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes#-}

module Frontend where

import Control.Monad
import GHC.Int
import Database.Id.Class (Id(..))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text as T
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM, eval, liftJSM)
import Data.Time as Time --for timestamp in postresponse
import qualified Snap.Core as S
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Location as Location
import qualified JSDOM.Types as JST


import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Api
import Common.Route
import Common.Schema

type AppWidget js t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PerformEvent t m
  , PostBuild t m
  , Prerender js t m
  , TriggerEvent t m
  )

type WidgetWithJS js t m =
  ( AppWidget js t m
  , HasJSContext (Performable m)
  , MonadJSM (Performable m)
  , MonadJSM m
  )

maybeHead :: [a] -> Maybe a
maybeHead x = case x of
    [] -> Nothing
    (a : as) -> Just a

inputBox :: AppWidget js t m => Event t () -> m (Event t ((Maybe JST.File), Text))
inputBox clr = do
  divClass "input" $ do
    inputEl <- textAreaElement def { _textAreaElementConfig_setValue = Just ("" <$ clr)}
    click <- button "submit"
    inRes <- inputElement $ def & initialAttributes .~ ("type" =: "file")
    let dynFileList = _inputElement_files inRes -- Dynamic t [File]
    let behFil = maybeHead <$> (current dynFileList) -- Behavior t (Maybe File)
    return (attach behFil (tag (current $ _textAreaElement_value inputEl) click))

--shortcuts for the routes were gonnna use
threadRoute :: Text
threadRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewTh :/ ()
commentRoute :: Text
commentRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_NewCom :/ ()
listRoute :: Text
listRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_ListPosts :/ ()
postRoute :: Id PostResponse -> Text
postRoute postId = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_GetPost :/ postId

--routes we postJson to

mapPost :: Maybe (Id PostResponse) -> (Maybe JST.File, Text) -> [M.Map Text (FormValue JST.File)]
mapPost mOp (mf, c) =
  case (mf, mOp) of
    (Nothing, Nothing) ->
      [] -- no pic, no op to comment on, no post
    (Nothing, Just op) -> -- comment without pic
      ("content" =: (FormValue_Text c) <> "op" =: (FormValue_Text ((T.pack . show) op))):[]
    (Just f, Nothing) -> do-- new thread
      ff <- fileToFormValue f
      ("image" =: ff <> "content" =: (FormValue_Text c)):[]
    (Just f, Just op) -> do-- comment with pic
      ff <- fileToFormValue f
      ("image" =: ff <> "content" =: (FormValue_Text c) <> "op" =: (FormValue_Text ((T.pack . show) op))):[]

-- why did putting MonadJSM [] in constraints
postThread :: WidgetWithJS js t m => Event t (Maybe JST.File, Text) -> m (Event t [XhrResponse])
postThread mfc = do
  postForms threadRoute (mapPost Nothing <$> mfc)
postComment :: WidgetWithJS js t m => Id PostResponse -> Event t (Maybe JST.File, Text) -> m (Event t [XhrResponse])
postComment postId mfc = do
  postForms commentRoute ((mapPost (Just postId)) <$> mfc)
fetchThreads :: PostFetch ->  XhrRequest Text 
fetchThreads pf= postJson listRoute pf

fromMaybe :: a -> Maybe a -> a
fromMaybe def m = case m of
  Nothing -> def
  Just j -> j

maybe2list :: Maybe a -> [a]
maybe2list Nothing = []
maybe2list Just a = a:[]

----loading screen
viewPlaceholder :: AppWidget js t m => m ()
viewPlaceholder = text "Loading post..."

viewPlaceholder' :: AppWidget js t m => m (Event t a)
viewPlaceholder' = do
  text "Loading post..."
  return (never)

--render functions
----render a thread preview for them main page
renderOp :: AppWidget js t m => ThreadResponse -> m ()
renderOp (ThreadResponse op rep cc) =
  elAttr "a" ("href" =: ("/p/" <> ((T.pack . show) (_postResponse_id op))) <> "class" =: "link") $
    elAttr "div" ("class" =: "op") $ do
      renderComment op
      text (((T.pack . show) cc) <> " comments")
      recRender renderComment (Prelude.take 3 rep) --ensure only 3 replies are rendered

--  elAttr "a" ("href" =: ("/p/" <> ((T.pack . show) (_postResponse_id po))) <> "class" =: "link") $
--    elAttr "div" ("class" =: "op" <> "id" =: ((T.pack . show) (_postResponse_id po))) $ do
--      elAttr "p" ("class" =: "timestamp") $ display $ (constDyn (_postResponse_datetime po))
--      elAttr "p" ("class" =: "postContent") $ text $ (fromMaybe "" (_postResponse_content po))
--      elAttr "p" ("class" =: "postId") $ display $ (constDyn (_postResponse_id po))


renderComment :: AppWidget js t m => PostResponse -> m ()
renderComment po =
  elAttr "div" ("class" =: "comment" <> "id" =: ((T.pack . show) (_postResponse_id po))) $ do
    elAttr "p" ("class" =: "timestamp") $ display $ (constDyn (_postResponse_datetime po))
    elAttr "p" ("class" =: "postContent") $ (textBlob . T.lines) $ _postResponse_content po
    elAttr "p" ("class" =: "postId") $ display $ (constDyn (_postResponse_id po))

textBlob :: AppWidget js t m => [Text] -> m()
textBlob bl =
  case bl of
    [] -> blank -- shouldn't happen
    (a:[]) -> text a
    (a:as) -> do
      if T.null a then do
        text ""
      --else if (T.isPrefixOf ">" (T.strip a)) then
      --  elAttr "span" ("class" =: "greentext") $ text a
      else do
        text a
      el "br" blank
      textBlob as

recRender :: AppWidget js t m => (a -> m ()) -> [a] -> m ()
recRender rf tl =
  case tl of
    [] -> blank
    [x] -> rf x
    x : xs -> do
      rf x
      recRender rf xs

renderThread :: WidgetWithJS js t m => Dynamic t (Id PostResponse) -> Event t () -> m(Event t (Maybe (Id PostResponse)))
renderThread postId mo = do
  loaded <- getPostBuild
  let evMoreThr = leftmost [loaded, mo]
  evMaLiPr <- getAndDecode $ postRoute <$> tag (current postId) evMoreThr
  let evLiPr = (fmapMaybe id evMaLiPr)
  divClass "content" $ do
    widgetHold_ (text "loading thread") ((recRender renderComment) <$> evLiPr)
  return (firstId <$> evLiPr)

renderMain :: WidgetWithJS js t m => Event t () -> m ()
renderMain mo = do
  loaded <- getPostBuild
  let evMoreThr = leftmost [loaded, mo] --Event t ()
  currPF <-  foldDyn (\_ (PostFetch o c) -> PostFetch (o + 15) c ) (PostFetch 0 15) evMoreThr
  let taggedEvThr = tag (current currPF) evMoreThr --Event t (PostFetch), later use foldDyn to increment pf
  evXhrRes <- performRequestAsync (fetchThreads <$> taggedEvThr)
  let evLiPr = fmapMaybe id (decodeXhrResponse <$> evXhrRes)
  dynLiPr <- foldDyn revCon [] evLiPr
  divClass "content" $ do
    dyn_ ((recRender renderOp) <$> dynLiPr)

revCon :: [a] -> [a] -> [a]
revCon a b = b ++ a

firstId :: [PostResponse] -> Maybe (Id PostResponse)
firstId l =
  case l of
    p:ps -> Just (Id (_postResponse_id p))
    _ -> Nothing


rightRoute :: Int64 -> Int64 -> Text
rightRoute com op =
  ("/p/" <> ((T.pack . show) op) <> "#" <> ((T.pack . show) com))

jsReload :: forall a m t js. (WidgetWithJS js t m, Monad m) => Text -> m ()
jsReload t = prerender_ blank $ liftJSM $ void $ eval t

jsNoOp :: forall m t js. (WidgetWithJS js t m, Monad m) => m ()
jsNoOp = prerender_ blank $ liftJSM $ void $ eval ("" :: T.Text)



app :: (AppWidget js t m, SetRoute t (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
app =
  subRoute_ $ \case
    FrontendRoute_Main -> do
      evPost <- inputBox never -- now Event t (Maybe File, Text)
      evThRes <- prerender
        (return never)
        (postThread evPost)
      rec
        prerender_
          (text "loading")
          (renderMain evMoreThr)
        evMoreThr <- button "see more"
      return ()
      -- setRoute $ (FrontendRoute_ViewPost :/) . Id <$> fmapMaybe id (decodeXhrResponse <$> (switchDyn evThRes))
    FrontendRoute_ViewPost -> do
      postId <- askRoute
      pid <- (sample . current) postId
      rec
        evPost <- inputBox never -- evComPosted
        dynEvComRes <- prerender
          (pure never)
          ((postComment pid) evPost)
      --  let evComPosted = void (switchDyn dynEvComRes)
      --redirect <- prerender -- redirect :: Dynamic t (Event t (Maybe (Id PostRresponse)))
      --  viewPlaceholder' -- loading screen
      --  (renderThread postId evComPosted)
      --setRoute $ (FrontendRoute_Main :/) <$> evComPosted -- if comment posted redirect to home
      --let evRecPostId = fmapMaybe id (switchDyn redirect) -- Event t (Id PostResponse), the Id of Op of thread
      --setRoute $ (FrontendRoute_ViewPost :/) <$> (ffilter (pid /=) evRecPostId) -- redirect to right thread
      return ()

header :: AppWidget js t m => m()
header =
  divClass "header" $ do
    elAttr "a" ("href" =: "/" <> "class" =: "header") $
      text "OBchan"

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "OBchan"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      header
      app
  }
