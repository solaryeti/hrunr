{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Rundeck Libraries
import qualified Rundeck.Call as RC
import           Rundeck.Call ()  -- import instances

-- For CLI options
import           Options
import           Control.Applicative

import qualified Network.Wreq as W
import           Control.Lens ((^.))
import           Data.Text (pack)

-- For XML processing
import           Data.Text.Lazy.Encoding  (decodeUtf8)
import           Text.XML (parseText_, def)
import           Text.XML.Cursor (content, element, fromDocument, (&/), (&//), ($/))

-- For JSON processing
-- import           Data.Aeson.Lens (key, _String, _Bool)

import qualified Data.Text as T
import qualified Data.Text.IO as TI (putStr)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as L

import           Control.Concurrent (threadDelay)

import Data.Aeson.Lens (key)
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

-- executionOutput :: RC.ApiCall -> MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
-- executionOutput call mainOpts opts _ = do
--    r <- RC.executionOutput (conninfo mainOpts) (params call mainOpts) (eoId opts)
--    return $ r ^. W.responseBody

executionOutput :: RC.ApiCall -> MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
executionOutput call mainOpts opts _ = go "0"
    where go offset = do
            -- TI.putStr offset
            r <- RC.executionOutput (conninfo mainOpts) (("offset", [offset]) : params call mainOpts) (eoId opts)
            let body = decodeUtf8 $ r ^. W.responseBody
            let doc = parseText_ def body
            let cursor = fromDocument doc
            if (completed cursor) == "true"
              then return . L.fromStrict . TE.encodeUtf8 $ entries cursor
              else do
                TI.putStr (entries cursor)
                threadDelay $ 2 * 1000000  -- 2 seconds
                -- hacky way to get offset as num: See Data.Text.Read
                go . T.pack $ show ((offsetnum cursor) - 1)

            where
              offsetnum c = read . T.unpack $ dataoffset c :: Int
              dataoffset c = T.concat $ c $/ element "output" &/ element "offset" &// content
              completed c = T.concat $ c $/ element "output" &/ element "completed" &// content
              entries c = T.concat $ c $/ element "output" &/ element "entries" &// content

  -- if format=json (default)
  -- let body = r ^. W.responseBody
  -- return . L.fromStrict . TE.encodeUtf8 $ entries r
  -- where offset r = r ^. W.responseBody . key "offset" . _String
  --       completed r = r ^. W.responseBody . key "completed" . _String
  --       entries r = r ^. W.responseBody . key "entries" . _String

  -- if format=xml (default)
  -- let body = decodeUtf8 $ r ^. W.responseBody
  -- let doc = parseText_ def body
  -- let cursor = fromDocument doc
  -- return . L.fromStrict . TE.encodeUtf8 $ T.concat $ entries cursor
  --   where offset c = c $/ element "output" &/ element "offset" &// content
  --         completed c = c $/ element "output" &/ element "completed" &// content
  --         entries c = c $/ element "output" &/ element "entries" &// content





-- rundeckVersion :: IO T.Text
-- rundeckVersion = do
--   r <- systemInfo
--   let x = decodeUtf8 $ r ^. responseBody
--   let doc = parseText_ def x
--   let cursor = fromDocument doc
--   return $ T.concat $ versionContent cursor
--   where versionContent c = c $/ element "system" &/ element "rundeck" &/ element "version" &// content

{-
<result success='true' apiversion='13'>
    <output>
      <id>13</id>
      <offset>430</offset>
      <completed>true</completed>
      <execCompleted>true</execCompleted>
      <hasFailedNodes>false</hasFailedNodes>
      <execState>succeeded</execState>
      <lastModified>1438538885000</lastModified>
      <execDuration>2551</execDuration>
      <percentLoaded>98.62385321100918</percentLoaded>
      <totalSize>436</totalSize>
      <entries>
        <entry time='18:08:04' absolute_time='2015-08-02T18:08:04Z' level='NORMAL' user='rundeck' command='' stepctx='1' node='localhost'>hello world</entry>
   </entries>
  </output>
</result>
-}



-- for repl
mainopts = MainOptions "192.168.56.2" "4440" "x1XSHLASnToUcVtQRJAQdKTQLMEbFF9e" "local"
eoopts = ExecutionOutputOptions "15" False
-- Main.executionOutput RC.ExecutionOutput mainopts eoopts [""]
-- r <- RC.executionOutput (conninfo mainopts) (params RC.ExecutionOutput mainopts) (eoId eoopts)
