{-# LANGUAGE OverloadedStrings #-}
module Rundeck.Call
       ( apiGet
       , jobExecutions
       , ApiCall(..)
       , RundeckResponse
       , Conninfo(..)
       , Method(..)
       , Params
       ) where

import Rundeck.Urls

import           Network.Wreq hiding (params)
import qualified Data.ByteString.Lazy as L
import           Control.Lens ((.~), (&))
import           Data.Text (Text)

data Method = Get | Post -- | Put | Delete
type RundeckResponse = (Response L.ByteString)

type Param = (Text, [Text])
type Params = [Param]

data Conninfo = Conninfo
    { host :: String
    , port :: String
    }

data ApiCall = SystemInfo
             | Projects
             | ExportJobs
             | Tokens
             | Jobs
             deriving (Show, Eq)

url :: String -> String -> String
url h p = "http://" ++ h ++ ":" ++ p

opts :: Params -> Options
opts params = defaults & paramList params
    -- & param "authtoken"     .~ [pack token]
    -- & param "project"       .~ ["local"]
    -- & header "Accept"       .~ ["application/json"]
    -- & header "Content-Type" .~ ["application/json"]

paramList :: Params -> Options -> Options
paramList [] = param "" .~ []
paramList [(x, xs)] = param x .~ xs
paramList ((x, xs):xss) = fmap (param x .~ xs) $ paramList xss

apiGet :: ApiCall -> Conninfo -> Params -> IO (Response L.ByteString)
apiGet a (Conninfo h p) params = getWith (opts params) $ url h p ++ apiurl a
  where apiurl SystemInfo = systemInfoUrl
        apiurl Projects = projectsUrl
        apiurl Tokens = tokensUrl
        apiurl ExportJobs = exportUrl
        apiurl Jobs = jobsUrl

jobExecutions :: Conninfo -> Params -> Method -> Id -> IO (Response L.ByteString)
jobExecutions (Conninfo h p) params Get i = getWith (opts params) $ url h p ++ jobExecutionsUrl i
jobExecutions (Conninfo h p) params Post i = postWith (opts params) (url h p ++ jobExecutionsUrl i) [partText "loglevel" "INFO"]
