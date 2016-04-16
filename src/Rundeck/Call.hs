{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Rundeck.Call
Description : Helper functions for calling Rundeck's API
Copyright   : (c) 2015 Steven Meunier

License     : BSD-style (see the file LICENSE)
Maintainer  : code@solaryeti.com
Stability   : experimental
Portability : GHC
-}

module Rundeck.Call
  ( -- * Types
    -- ** Data Types
    ApiCall(..)
  , Conninfo(..)

    -- ** Type Aliases
  , RundeckResponse
  , Param
  , Params

    -- * Functions
  , body
  , get
  , getWithSession
  , post
  , postWithSession

    -- * Re-exports
  , S.Session
  , S.withAPISession
  ) where

import           Rundeck.Urls

import           Control.Lens         ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as L
import           Data.Text            (Text)
import qualified Network.Wreq         as W
import qualified Network.Wreq.Session as S

type RundeckResponse = (W.Response L.ByteString)
type Param = (Text, [Text])
type Params = [Param]

-- | Conninfo contains the connection info the make a connection to Rundeck
data Conninfo = Conninfo
  { host :: String   -- ^ The hostname of the Rundeck server
  , port :: String   -- ^ The port the Rundeck server is listening on
  }

-- | Type representing the possible valid calls that can be made against Rundeck
data ApiCall
  = SystemInfo
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

apiurl :: ApiCall -> ApiUrl
apiurl (ExecutionOutput i) = executionOutputUrl i
apiurl (JobExecutions i)   = jobExecutionsUrl i
apiurl ExportJobs = exportUrl
apiurl Jobs       = jobsUrl
apiurl Projects   = projectsUrl
apiurl Tokens     = tokensUrl
apiurl SystemInfo = systemInfoUrl

-- | Issue a Get request against the Rundeck API.
-- The api endpoint is determined by the type of the 'ApiCall' made.
get :: ApiCall -> Conninfo -> Params -> IO RundeckResponse
get a (Conninfo h p) params = W.getWith (opts params) (url h p ++ apiurl a)

-- | Issue a Post request against the Rundeck API.
-- The api endpoint is determined by the type of the 'ApiCall' made.
post :: ApiCall -> Conninfo -> Params -> IO RundeckResponse
post a (Conninfo h p) params = W.postWith (opts params) (url h p ++ apiurl a) [W.partText "loglevel" "INFO"]

-- | Issue a Get request against the Rundeck API reusing a session.
-- The api endpoint is determined by the type of the 'ApiCall' made.
getWithSession :: S.Session -> ApiCall -> Conninfo -> Params -> IO RundeckResponse
getWithSession sess a (Conninfo h p) params = S.getWith (opts params) sess (url h p ++ apiurl a)

-- | Issue a Post request against the Rundeck API reusing a given session.
-- The api endpoint is determined by the type of the 'ApiCall' made.
postWithSession :: S.Session -> ApiCall -> Conninfo -> Params -> IO RundeckResponse
postWithSession sess a (Conninfo h p) params = S.postWith (opts params) sess (url h p ++ apiurl a) [W.partText "loglevel" "INFO"]
