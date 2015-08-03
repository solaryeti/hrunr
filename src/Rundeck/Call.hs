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
       , Method(..)

       -- ** Type Aliases
       , RundeckResponse
       , Param
       , Params

       -- * Functions
       , apiGet
       , jobExecutions
       , executionOutput
       ) where

import Rundeck.Urls

import           Network.Wreq hiding (params)
import qualified Data.ByteString.Lazy as L
import           Control.Lens ((.~), (&))
import           Data.Text (Text)

-- | HTTP methods
data Method = Get | Post

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
             | ExecutionOutput
             | JobExecutions
             deriving (Show, Eq)

url :: String -> String -> String
url h p = "http://" ++ h ++ ":" ++ p

opts :: Params -> Options
opts params = defaults & paramList params

paramList :: Params -> Options -> Options
paramList [] = param "" .~ []
paramList [(x, xs)] = param x .~ xs
paramList ((x, xs):xss) = fmap (param x .~ xs) $ paramList xss

-- | Issue a Get request against the Rundeck API
-- The api endpoint is determined by the type of the 'ApiCall' made.
apiGet :: ApiCall -> Conninfo -> Params -> IO (Response L.ByteString)
apiGet a (Conninfo h p) params = getWith (opts params) $ url h p ++ apiurl a
  where apiurl SystemInfo = systemInfoUrl
        apiurl Projects = projectsUrl
        apiurl Tokens = tokensUrl
        apiurl ExportJobs = exportUrl
        apiurl Jobs = jobsUrl
        apiurl _ = "/"  -- TODO: tidy up, this is a hack just to make it exhaustive

jobExecutions :: Conninfo -> Params -> Method -> Id -> IO (Response L.ByteString)
jobExecutions (Conninfo h p) params Get i = getWith (opts params) $ url h p ++ jobExecutionsUrl i
jobExecutions (Conninfo h p) params Post i = postWith (opts params) (url h p ++ jobExecutionsUrl i) [partText "loglevel" "INFO"]

-- | Get the execution output for a job from Rundeck
executionOutput ::Conninfo -> Params -> Id -> IO (Response L.ByteString)
executionOutput (Conninfo h p) params i = getWith (opts params) $ url h p ++ executionOutputUrl i
