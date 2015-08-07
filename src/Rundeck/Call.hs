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
       , apiGet
       , apiPost
       , body
       ) where

import           Rundeck.Urls

import           Control.Applicative  ((<$>))
import           Control.Lens         ((&), (.~), (^.))
import qualified Data.ByteString.Lazy as L
import           Data.Text            (Text)
import           Network.Wreq         hiding (params)

type RundeckResponse = (Response L.ByteString)
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

body :: Response a -> a
body r = r ^. responseBody

opts :: Params -> Options
opts params = defaults & paramList params

paramList :: Params -> Options -> Options
paramList [] = param "" .~ []
paramList [(x, xs)] = param x .~ xs
paramList ((x, xs):xss) = (param x .~ xs) <$> paramList xss

-- | Issue a Get request against the Rundeck API
-- The api endpoint is determined by the type of the 'ApiCall' made.
apiGet :: ApiCall -> Conninfo -> Params -> IO RundeckResponse
apiGet a (Conninfo h p) params = getWith (opts params) $ url h p ++ apiurl a
  where apiurl (ExecutionOutput i) = executionOutputUrl i
        apiurl (JobExecutions i)   = jobExecutionsUrl i
        apiurl ExportJobs = exportUrl
        apiurl Jobs       = jobsUrl
        apiurl Projects   = projectsUrl
        apiurl Tokens     = tokensUrl
        apiurl SystemInfo = systemInfoUrl

apiPost :: ApiCall -> Conninfo -> Params -> IO RundeckResponse
apiPost a (Conninfo h p) params = postWith (opts params) (url h p ++ apiurl a) [partText "loglevel" "INFO"]
  where apiurl (JobExecutions i) = jobExecutionsUrl i
        apiurl _          = "/"  -- TODO: tidy up, this is a hack just to make it exhaustive. Use Either instead
