{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Rundeck.Call
-- Copyright   : (c) 2015 Steven Meunier
--
-- License     : BSD-style
-- Maintainer  : steven@solaryeti.com
-- Stability   : experimental
-- Portability : GHC

module Rundeck.Call
       (
       -- * Types
       -- ** Data Types
         ApiCall(..)
       , Conninfo(..)

       -- ** Type Aliases
       , RundeckResponse
       , Param
       , Params

       -- * Functions
       , get
       , post
       , body
       ) where

import           Rundeck.Urls

import           Control.Applicative  ((<$>))
import           Control.Lens         ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as L
import           Data.Text            (Text)
import qualified Network.Wreq         as W

type RundeckResponse = (W.Response L.ByteString)
type Param = (Text, [Text])
type Params = [Param]

-- | Conninfo contains the connection info the make a connection to Rundeck
data Conninfo = Conninfo
    { host :: String   -- ^ The hostname of the Rundeck server
    , port :: String   -- ^ The port the Rundeck server is listening on
    }

-- | Type representing the possible valid calls that can be made against Rundeck
data ApiCall = SystemInfo
             | Projects
             | ExportJobs
             | Tokens
             | Jobs
             | ExecutionOutput Id
             | JobExecutions Id
             deriving (Show, Eq)

url :: String -> String -> String
url h p = "http://" ++ h ++ ":" ++ p

-- | Return the HTTP response body in a given HTTP response
body :: W.Response a -> a
body r = r ^. W.responseBody

opts :: Params -> W.Options
opts params = W.defaults & paramList params

paramList :: Params -> W.Options -> W.Options
paramList [] = W.param "" .~ []
paramList [(x, xs)] = W.param x .~ xs
paramList ((x, xs):xss) = (W.param x .~ xs) <$> paramList xss

-- | Issue a Get request against the Rundeck API
-- The api endpoint is determined by the type of the 'ApiCall' made.
get :: ApiCall -> Conninfo -> Params -> IO RundeckResponse
get a (Conninfo h p) params = W.getWith (opts params) $ url h p ++ apiurl a
  where apiurl (ExecutionOutput i) = executionOutputUrl i
        apiurl (JobExecutions i)   = jobExecutionsUrl i
        apiurl ExportJobs = exportUrl
        apiurl Jobs       = jobsUrl
        apiurl Projects   = projectsUrl
        apiurl Tokens     = tokensUrl
        apiurl SystemInfo = systemInfoUrl

-- | Issue a Post request against the Rundeck API
-- The api endpoint is determined by the type of the 'ApiCall' made.
post :: ApiCall -> Conninfo -> Params -> IO RundeckResponse
post a (Conninfo h p) params = W.postWith (opts params) (url h p ++ apiurl a) [W.partText "loglevel" "INFO"]
  where apiurl (JobExecutions i) = jobExecutionsUrl i
        apiurl _          = "/"  -- TODO: tidy up, this is a hack just to make it exhaustive. Use Either instead
