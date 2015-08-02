{-# LANGUAGE OverloadedStrings #-}
module Rundeck.Call
       ( apiGet
       , jobExecutions
       , ApiCall(..)
       , RundeckResponse
       , Conninfo(..)
       , Method(..)
       ) where

import Rundeck.Urls

import           Network.Wreq
import qualified Data.ByteString.Lazy as L
import           Control.Lens ((.~), (&))
import           Data.Text (pack)

data Method = Get | Post -- | Put | Delete
type RundeckResponse = (Response L.ByteString)

data Conninfo = Conninfo
    { host :: String
    , port :: String
    , authtoken :: String
    }

data ApiCall = SystemInfo
             | Projects
             | ExportJobs
             | Tokens
             | Jobs
             deriving (Show, Eq)

url :: String -> String -> String
url h p = "http://" ++ h ++ ":" ++ p

opts :: String -> Options
opts token = defaults
    & param "authtoken"     .~ [pack token]
    & param "project"       .~ ["local"]
    -- & header "Accept"       .~ ["application/json"]
    -- & header "Content-Type" .~ ["application/json"]

apiGet :: ApiCall -> Conninfo -> IO (Response L.ByteString)
apiGet a c = getWith (opts $ authtoken c) $ (url h p) ++ apiurl a
  where h = host c
        p = port c
        apiurl SystemInfo = systemInfoUrl
        apiurl Projects = projectsUrl
        apiurl Tokens = tokensUrl
        apiurl ExportJobs = exportUrl
        apiurl Jobs = jobsUrl

jobExecutions :: Conninfo -> Method -> Id -> IO (Response L.ByteString)
jobExecutions c Get i = getWith (opts $ authtoken c) $ (url (host c) (port c)) ++ jobExecutionsUrl i
jobExecutions c Post i = postWith (opts $ authtoken c) ((url (host c) (port c)) ++ jobExecutionsUrl i) [partText "loglevel" "INFO"]
