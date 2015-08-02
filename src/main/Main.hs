{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Rundeck.Call as RC
import           Rundeck.Call ()  -- import instances

import           Prelude hiding (id)
import           Options
import           Control.Applicative
import qualified Network.Wreq as W
import           Control.Lens ((^.))
import           Data.Text (pack)

data MainOptions = MainOptions
    { host :: String
    , port :: String
    , authtoken :: String
    , project :: String
    }
instance Options MainOptions where
  defineOptions = pure MainOptions
      <*> simpleOption "host" "192.168.56.2" "Rundeck host"
      <*> simpleOption "port" "4440" "Rundeck port"
      <*> defineOption optionType_string (\o -> o
              { optionLongFlags = ["authtoken"]
              , optionDefault = "x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e"
              , optionGroup = Just $ group "param" "Params" ""
              })
      <*> defineOption optionType_string (\o -> o
              { optionLongFlags = ["project"]
              , optionDefault = "local"
              , optionGroup = Just $ group "param" "Params" ""
              })

data NoSubOptions = NoSubOptions
instance Options NoSubOptions where
  defineOptions = pure NoSubOptions

data RunJobOptions = RunJobOptions { id :: String }
instance Options RunJobOptions where
  defineOptions = pure RunJobOptions
      <*> simpleOption "id" "" "Job to run"

conninfo :: MainOptions -> RC.Conninfo
conninfo mainOpts = RC.Conninfo (host mainOpts) (port mainOpts)

params :: RC.ApiCall -> MainOptions -> RC.Params
params call mainOpts
  | call `elem` [RC.Jobs, RC.ExportJobs] =  [("authtoken", [(pack $ authtoken mainOpts)])
                                            ,("project", [(pack $ project mainOpts)])]
  | otherwise = [("authtoken", [(pack $ authtoken mainOpts)])]

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
  r <- RC.apiGet call (conninfo mainOpts) (params call mainOpts)
  print $ r ^. W.responseBody

runjob :: MainOptions -> RunJobOptions -> [String] -> IO ()
runjob mainOpts opts _ = do
  r <- RC.jobExecutions (conninfo mainOpts) [("authtoken", [(pack $ authtoken mainOpts)])] RC.Post (id opts)
  print $ r ^. W.responseBody
