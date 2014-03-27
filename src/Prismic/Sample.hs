{-# LANGUAGE OverloadedStrings #-}

module Prismic.Sample
        ( main
        )
    where

import Prismic.Api

main :: IO ()
main = do
    a <- api "http://lesbonneschoses.prismic.io/api" Nothing
    r <- submit a $ (search "everything")
                        { searchPage     = Just 1
                        , searchPageSize = Just 10
                        }
    print r
