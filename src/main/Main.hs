{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
-- :set -XOverloadedStrings
module Main where

import qualified Rundeck.Call as RC
import Rundeck.Call ()  -- import instances

import System.Console.CmdArgs

-- import qualified Data.Text as T
-- import Data.Map as Map
-- import Data.Aeson (Value)
-- import Data.Aeson.Lens
import           Network.Wreq
import           Control.Lens ((^.))
-- import           Data.Text.Lazy.Encoding  (decodeUtf8)
-- import           Text.XML (parseText_, def)
-- import           Text.XML.Cursor (content, element, fromDocument, (&/), (&//), ($/))

-- | Setup our commandline arguments
data Hrunr = Jobs       { host :: String, port :: String, project :: String, authtoken :: String }
           | Projects   { host :: String, port :: String, authtoken :: String }
           | SystemInfo { host :: String, port :: String, authtoken :: String }
           deriving (Data,Typeable,Show,Eq)

hostFlags :: String
hostFlags = "192.168.56.2"

portFlags :: String
portFlags = "4440"

jobs :: Hrunr
jobs = Jobs {host = hostFlags
            ,port = portFlags
            ,authtoken = "x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e"
            ,project = def &= help "Project" &= typ "PROJECT"
            } &= help "List the jobs that exist for a project."

systemInfo :: Hrunr
systemInfo = SystemInfo {host = hostFlags
                        ,port = portFlags
                        ,authtoken = "x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e"
                        } &= help "List the systemInfo that exist for a project."

projects ::Hrunr
projects = Projects {host = hostFlags
                    ,port = portFlags
                    ,authtoken = "x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e"
                    } &= help "List the existing projects on the server."

main :: IO ()
main = run =<< cmdArgsRun mode


mode :: Mode (CmdArgs Hrunr)
mode = cmdArgsMode $ modes [jobs,projects,systemInfo] &= help "Cli tool to connect to Rundeck's API" &= program "hrunr" &= summary "hrunr 0.1.0" &= verbosity

runAction :: Hrunr -> IO (RC.RundeckResponse)
runAction (Jobs h p _ _ ) = RC.apiGet RC.Jobs h p
runAction (Projects h p _) = RC.apiGet RC.Projects h p
runAction (SystemInfo h p _) = RC.apiGet RC.SystemInfo h p

run :: Hrunr -> IO ()
run cmd = do
  r <- runAction cmd
  print $ r ^. responseBody
