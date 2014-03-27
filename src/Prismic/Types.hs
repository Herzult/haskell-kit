{-# LANGUAGE OverloadedStrings #-}

module Prismic.Types
        ( Api (..)
        , ApiContext (..)
        , Token
        , apiForm
        , Ref (..)
        , Form (..)
        , FormName
        , FormField (..)
        , Url
        , Bookmark
        , Type
        , Tag
        , Search (..)
        , search
        , Document (..)
        , DocumentData
        , Response (..)
        )
    where

import Data.Text
import Data.Time
import Data.Map

import qualified Data.Map as Map

import Data.Aeson

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)

type Url = Text
type Bookmark = Text
type Type = Text
type Tag = Text
type FormName = Text
type Token = Text

data ApiContext = ApiContext (Maybe Token) Api
    deriving (Show, Eq)

data Api = Api {
    apiRefs             :: [Ref]
  , apiBookmarks        :: Map Text Bookmark
  , apiTypes            :: Map Text Type
  , apiTags             :: [Tag]
  , apiForms            :: Map FormName Form
  , apiOauthInitiateUrl :: Url
  , apiOauthTokenUrl    :: Url
} deriving (Show, Eq)

instance FromJSON Api where
    parseJSON (Object o) = Api <$> o .: "refs"
                               <*> o .: "bookmarks"
                               <*> o .: "types"
                               <*> o .: "tags"
                               <*> o .: "forms"
                               <*> o .: "oauth_initiate"
                               <*> o .: "oauth_token"
    parseJSON _ = mzero

apiForm :: Api -> Text -> Maybe Form
apiForm a = flip Map.lookup (apiForms a)

data Ref = Ref {
    refRef         :: Text
  , refLabel       :: Text
  , refIsMaster    :: Bool
  , refScheduledAt :: Maybe UTCTime
} deriving (Show, Eq)

instance FromJSON Ref where
    parseJSON (Object o) = Ref <$> o .: "ref"
                               <*> o .: "label"
                               <*> o .: "isMasterRef"
                               <*> o .:? "scheduledAt"
    parseJSON _ = mzero

data Form = Form {
    formName    :: Maybe Text
  , formMethod  :: Text
  , formRel     :: Maybe Text
  , formEnctype :: Text
  , formAction  :: Text
  , formFields  :: Map Text FormField
} deriving (Show, Eq)

instance FromJSON Form where
    parseJSON (Object o) = Form <$> o .:? "name"
                                <*> o .: "method"
                                <*> o .:? "rel"
                                <*> o .: "enctype"
                                <*> o .: "action"
                                <*> o .: "fields"
    parseJSON _ = mzero

data FormField = FormField {
    formFieldType     :: Text
  , formFieldDefault  :: Maybe Text
  , formFieldMultiple :: Bool
} deriving (Show, Eq)

instance FromJSON FormField where
    parseJSON (Object o) = FormField <$> o .: "type"
                                     <*> o .:? "default"
                                     <*> o .: "multiple"
    parseJSON _ = mzero

data Search = Search {
    searchFormName  :: FormName
  , searchApiRef    :: Maybe Ref
  , searchQuery     :: Maybe [Text]
  , searchPage      :: Maybe Integer
  , searchPageSize  :: Maybe Integer
  , searchOrderings :: Maybe Text
} deriving (Show, Eq)

search :: FormName -> Search
search n = Search
    { searchFormName  = n
    , searchApiRef    = Nothing
    , searchQuery     = Nothing
    , searchPage      = Nothing
    , searchPageSize  = Nothing
    , searchOrderings = Nothing
    }

newtype DocumentData = DocumentData Value
    deriving (Show, Eq)

instance FromJSON DocumentData where
    parseJSON v = DocumentData <$> pure v

data Document = Document {
    documentId    :: Text
  , documentType  :: Text
  , documentHref  :: Text
  , documentSlugs :: [Text]
  , documentData  :: DocumentData
} deriving (Show, Eq)

instance FromJSON Document where
    parseJSON (Object o) = Document <$> o .: "id"
                                    <*> o .: "type"
                                    <*> o .: "href"
                                    <*> o .: "slugs"
                                    <*> o .: "data"
    parseJSON _ = mzero

data Response = Response {
    responseResults          :: [Document]
  , responsePage             :: Integer
  , responseResultsPerPage   :: Integer
  , responseTotalResultsSize :: Integer
  , responseTotalPages       :: Integer
  , responseNextPage         :: Maybe Text
  , responsePrevPage         :: Maybe Text
} deriving (Show, Eq)

instance FromJSON Response where
    parseJSON (Object o) = Response <$> o .: "results"
                                    <*> o .: "page"
                                    <*> o .: "results_per_page"
                                    <*> o .: "total_results_size"
                                    <*> o .: "total_pages"
                                    <*> o .:? "next_page"
                                    <*> o .:? "prev_page"
