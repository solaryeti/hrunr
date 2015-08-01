{-# LANGUAGE OverloadedStrings #-}
module Rundeck.Call
       (exportJobs
       ,jobExecutions
       ,jobs
       ,projects
       ,systemInfo
       ,tokens
       ) where

import Rundeck.Urls

import           Network.Wreq
import qualified Data.ByteString.Lazy as L
import           Control.Lens ((.~), (&))


data Request = Get | Post

opts :: Options
opts = defaults & param "authtoken"     .~ ["x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e"]
                & param "project"       .~ ["local"]
                & header "Accept"       .~ ["application/json"]
                & header "Content-Type" .~ ["application/json"]


exportJobs :: IO (Response L.ByteString)
exportJobs = getWith opts exportUrl

-- importJobs :: IO (Response L.ByteString)
-- importJobs = postWith opts exportUrl

tokens :: IO (Response L.ByteString)
tokens = getWith opts tokensUrl

systemInfo :: IO (Response L.ByteString)
systemInfo = getWith opts systemInfoUrl

projects :: IO (Response L.ByteString)
projects = getWith opts projectsUrl

jobs :: IO (Response L.ByteString)
jobs = getWith opts jobsUrl

jobExecutions :: Request -> Id -> IO (Response L.ByteString)
jobExecutions Get i = getWith opts $ jobExecutionsUrl i
jobExecutions Post i = postWith opts (jobExecutionsUrl i) [partText "loglevel" "INFO"]
