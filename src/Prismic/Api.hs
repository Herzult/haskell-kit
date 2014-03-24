{-# LANGUAGE OverloadedStrings #-}

module Prismic.Api
        ( api
        )
    where

import Prismic.Types

import Data.Text (Text, pack, unpack)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Maybe (fromJust, fromMaybe)

import Control.Monad (liftM)
import Control.Arrow ((***))

import Network.HTTP.Conduit (parseUrl, requestHeaders, withManager, httpLbs, responseBody)
import Network.HTTP.Types (hAccept)
import Network.URL

type Token = Text

data ApiContext = ApiContext (Maybe Token) Api
    deriving (Show, Eq)

api :: Url -> Maybe Token -> IO ApiContext
api url mTk = ApiContext mTk `liftM` get url mTk

submit :: ApiContext -> Search -> IO Response
submit (ApiContext mTk api) srch = do
    undefined
  where
    form a n =
        case apiForm a n of
            Nothing -> error $ "form not found: " ++ unpack n
            Just f  -> f

    merge field mVal =
        let def = maybe [] (:[]) (formFieldDefault field)
            val = fromMaybe [] mVal
        in if formFieldMultiple field
               then def ++ val
               else if null val
                        then def
                        else val

    searchVals src = [ ("q", searchQuery src)
                     , ("page", ((:[]) . pack . show) `liftM` searchPage src)
                     , ("page_size", ((:[]) . pack . show) `liftM` searchPage src)
                     , ("orderings", (:[]) `liftM` searchOrderings src)
                     ]

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
