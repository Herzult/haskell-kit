{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Prismic.Api
        ( api
        , submit
        , search
        , Search (..)
        , Token
        , FormName
        )
    where

import Prismic.Types

import Data.Text (Text, pack, unpack)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map (toList)

import Control.Monad (liftM, join)
import Control.Arrow ((***))

import Network.HTTP.Conduit (parseUrl, requestHeaders, withManager, httpLbs, responseBody)
import Network.HTTP.Types (hAccept)
import Network.URL

api :: Url -> Maybe Token -> IO ApiContext
api url mTk = ApiContext mTk `liftM` get url mTk

submit :: ApiContext -> Search -> IO Response
submit (ApiContext mTk api) srch = do
    let Just form = apiForm api $ searchFormName srch
        ref       = refRef $ fromMaybe (apiMaster api) (searchApiRef srch)
        url       = formAction form `withQueryParams` (searchParams form ++ [("ref", ref)])

    get url mTk

  where
    searchParams form =
        let
            vals = [ ("q", searchQuery srch)
                   , ("page", ((:[]) . pack . show) `liftM` searchPage srch)
                   , ("pageSize", ((:[]) . pack . show) `liftM` searchPage srch)
                   , ("orderings", (:[]) `liftM` searchOrderings srch)
                   ]
            fields = toList $ formFields form
        in
            flatten $ flip map fields $ \(fieldName, field) ->
                (fieldName, merge field (join $ lookup fieldName vals))

    flatten = foldr (\(k, vs) xs -> xs ++ map (k,) vs) []

    merge field mVal =
        let def = maybe [] (:[]) (fieldDefault field)
        in case mVal of
            Nothing  -> def
            Just val -> if fieldMultiple field then def ++ val else val

get :: (FromJSON a) => Url -> Maybe Token -> IO a
get url mTk = do
    let finUrl = authUrl url mTk

    print $ "GET " ++ finUrl

    initReq <- parseUrl finUrl
    let req = initReq
                { requestHeaders = [(hAccept, "application/json")]
                }

    (decodeBody . responseBody) `liftM` withManager (httpLbs req)

  where
    authUrl u mt =
        unpack $ case mt of
            Nothing -> u
            Just tk -> u `withQueryParams` [("token", tk)]

    decodeBody bdy =
        case eitherDecode bdy of
            Left e  -> error $ "Prismic.Api.get: unable to decode JSON body: " ++ e
            Right v -> v

withQueryParams :: Url -> [(Text, Text)] -> Url
withQueryParams u ps =
    let
        url = fromJust $ importURL (unpack u)
        pms = url_params url ++ map (unpack *** unpack) ps
    in
        pack . exportURL $ url { url_params = pms }
