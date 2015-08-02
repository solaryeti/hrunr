{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- :set -XOverloadedStrings
module Main where

import qualified Rundeck.Call as RC
import Rundeck.Call ()  -- import instances

import Prelude hiding (id)
import Options
import Control.Applicative
--import System.Console.CmdArgs

-- import qualified Data.Text as T
-- import Data.Map as Map
-- import Data.Aeson (Value)
-- import Data.Aeson.Lens
import qualified Network.Wreq as W
import           Control.Lens ((^.))
-- import           Data.Text.Lazy.Encoding  (decodeUtf8)
-- import           Text.XML (parseText_, def)
-- import           Text.XML.Cursor (content, element, fromDocument, (&/), (&//), ($/))

-- | Setup our commandline arguments

data MainOptions = MainOptions
    { host :: String
    , port :: String
    , authtoken :: String
    }
instance Options MainOptions where
  defineOptions = pure MainOptions
      <*> simpleOption "host" "192.168.56.2" "Rundeck host"
      <*> simpleOption "port" "4440" "Rundeck port"
      <*> simpleOption "authtoken" "x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e" "Authtoken for authentication"

data NoSubOptions = NoSubOptions
instance Options NoSubOptions where
  defineOptions = pure NoSubOptions

data RunJobOptions = RunJobOptions { id :: String }
instance Options RunJobOptions where
  defineOptions = pure RunJobOptions
      <*> simpleOption "id" "" "Job to run"

conninfo :: MainOptions -> RC.Conninfo
conninfo mainOpts = RC.Conninfo (host mainOpts) (port mainOpts) (authtoken mainOpts)

main :: IO ()
main = runSubcommand [ subcommand "system-info" $ simpleRun RC.SystemInfo
                     , subcommand "projects" $ simpleRun RC.Projects
                     , subcommand "tokens" $ simpleRun RC.Tokens
                     , subcommand "jobs" $ simpleRun RC.Jobs
                     , subcommand "export-jobs" $ simpleRun RC.ExportJobs
                     , subcommand "runjob" runjob
                     ]

simpleRun :: RC.ApiCall -> MainOptions -> NoSubOptions -> [String] -> IO ()
simpleRun call mainOpts _ _ = do
  r <- RC.apiGet call $ conninfo mainOpts
  print $ r ^. W.responseBody

runjob :: MainOptions -> RunJobOptions -> [String] -> IO ()
runjob mainOpts opts _ = do
  r <- RC.jobExecutions (conninfo mainOpts) RC.Post (id opts)
  print $ r ^. W.responseBody
