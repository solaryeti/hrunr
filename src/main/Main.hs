{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Rundeck Libraries
import qualified Rundeck.Call as RC
import           Rundeck.Call ()  -- import instances
import           Rundeck.Xml

-- For CLI options
import           Options
import           Control.Applicative

import qualified Network.Wreq as W
import           Control.Lens ((^.))
import           Data.Text (pack)


-- For JSON processing
-- import           Data.Aeson.Lens (key, _String, _Bool)

import qualified Data.Text as T
import qualified Data.Text.IO as TI (putStr)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as L (fromStrict, ByteString)

import           Control.Concurrent (threadDelay)

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

data RunJobOptions = RunJobOptions { rjId :: String }
instance Options RunJobOptions where
  defineOptions = pure RunJobOptions
      <*> simpleOption "id" "" "Job to run"

data ExecutionOutputOptions = ExecutionOutputOptions { eoId :: String, follow :: Bool }
instance Options ExecutionOutputOptions where
  defineOptions = pure ExecutionOutputOptions
      <*> simpleOption "id" "" "Job to run"
      <*> simpleOption "follow" False "Tail execution output"

type Args = [String]

conninfo :: MainOptions -> RC.Conninfo
conninfo mainOpts = RC.Conninfo (host mainOpts) (port mainOpts)

params :: RC.ApiCall -> MainOptions -> RC.Params
params call mainOpts
  | call `elem` [RC.Jobs, RC.ExportJobs] =  [("authtoken", [(pack $ authtoken mainOpts)])
                                            ,("project", [(pack $ project mainOpts)])]
  -- | call `elem` [RC.ExecutionOutput] = [("authtoken", [(pack $ authtoken mainOpts)])
  --                                      ,("offset", ["0"])]
  | otherwise = [("authtoken", [(pack $ authtoken mainOpts)])]

main :: IO ()
main = print =<< runSubcommand
    [ subcommand "system-info" $ simpleRun RC.SystemInfo
    , subcommand "projects" $ simpleRun RC.Projects
    , subcommand "tokens" $ simpleRun RC.Tokens
    , subcommand "jobs" $ simpleRun RC.Jobs
    , subcommand "export-jobs" $ simpleRun RC.ExportJobs
    , subcommand "runjob" $ runjob RC.ExecutionOutput
    , subcommand "execution-output" $ executionOutput RC.JobExecutions
    ]

simpleRun :: RC.ApiCall -> MainOptions -> NoSubOptions -> Args -> IO L.ByteString
simpleRun call mainOpts _ _ = do
  r <- RC.apiGet call (conninfo mainOpts) (params call mainOpts)
  return $ r ^. W.responseBody

runjob :: RC.ApiCall -> MainOptions -> RunJobOptions -> Args -> IO L.ByteString
runjob call mainOpts opts _ = do
  r <- RC.jobExecutions (conninfo mainOpts) (params call mainOpts) RC.Post (rjId opts)
  return $ r ^. W.responseBody

executionOutput :: RC.ApiCall -> MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
executionOutput call mainOpts opts _ = go "0"
  where go offset = do
          r <- RC.executionOutput (conninfo mainOpts) (("offset", [offset]) : params call mainOpts) (eoId opts)
          let cursor = responseBodyCursor $ r ^. W.responseBody

          -- hacky way to get offset as num: See Data.Text.Read
          -- we have to decrement the offset by one or else rundeck give a fault
          let safeOffset = T.pack . show $ (read . T.unpack $ outputContent cursor "offset" :: Int) - 1

          if (outputContent cursor "completed") == "true"
            then return . L.fromStrict . TE.encodeUtf8 $ outputContent cursor "entries"
            else do
              TI.putStr (outputContent cursor "entries")
              threadDelay $ 2 * 1000000  -- 2 seconds
              go safeOffset
