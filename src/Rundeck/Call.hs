{-# LANGUAGE OverloadedStrings #-}
module Rundeck.Call
       (apiGet
       ,jobExecutions
       ,ApiCall(..)
       ,RundeckResponse
       ) where

import Rundeck.Urls

import           Network.Wreq
import qualified Data.ByteString.Lazy as L
import           Control.Lens ((.~), (&))

data Method = Get | Post | Put | Delete
type RundeckResponse = (Response L.ByteString)

data ApiCall = SystemInfo
             | Projects
             | ExportJobs
             | Tokens
             | Jobs
             deriving (Show, Eq)

url :: String -> String -> String
url host port = "http://" ++ host ++ ":" ++ port

opts :: Options
opts = defaults & param "authtoken"     .~ ["x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e"]
                & param "project"       .~ ["local"]
                & header "Accept"       .~ ["application/json"]
                & header "Content-Type" .~ ["application/json"]

apiGet :: ApiCall -> String -> String -> IO (Response L.ByteString)
apiGet a h p = getWith opts $ (url h p) ++ apiurl a
  where apiurl SystemInfo = systemInfoUrl
        apiurl Projects = projectsUrl
        apiurl Tokens = tokensUrl
        apiurl ExportJobs = exportUrl
        apiurl Jobs = jobsUrl

jobExecutions :: Method -> Id -> IO (Response L.ByteString)
jobExecutions Get i = getWith opts $ jobExecutionsUrl i
jobExecutions Post i = postWith opts (jobExecutionsUrl i) [partText "loglevel" "INFO"]
