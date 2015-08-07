{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Rundeck Libraries
import           Rundeck.Call         ()
import qualified Rundeck.Call         as RC
import           Rundeck.Xml

-- For CLI options
import           Control.Applicative
import           Options

import           Control.Concurrent   (threadDelay)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, putStr)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as TI (putStr)

data MainOptions = MainOptions
    { host      :: String
    , port      :: String
    , authtoken :: String
    , project   :: String
    }
instance Options MainOptions where
  defineOptions = pure MainOptions
      <*> simpleOption "host" "192.168.56.2" "Rundeck host"
      <*> simpleOption "port" "4440" "Rundeck port"
      <*> defineOption optionType_string (\o -> o
              { optionLongFlags = ["authtoken"]
              , optionDefault = "fCg23CDrtT1uJxQsHYpCWPFoCfMEKSQk"
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

data RunJobOptions = RunJobOptions { rjId :: String, rjFollow :: Bool }
instance Options RunJobOptions where
  defineOptions = pure RunJobOptions
      <*> simpleOption "id" "" "Job to run"
      <*> simpleOption "follow" False "Tail execution output"

data ExecutionOutputOptions = ExecutionOutputOptions { eoId :: String, eoFollow :: Bool }
instance Options ExecutionOutputOptions where
  defineOptions = pure ExecutionOutputOptions
      <*> simpleOption "id" "" "Job to run"
      <*> simpleOption "follow" False "Tail execution output"

type Args = [String]

conninfo :: MainOptions -> RC.Conninfo
conninfo mainOpts = RC.Conninfo (host mainOpts) (port mainOpts)

params :: RC.ApiCall -> MainOptions -> RC.Params
params call mainOpts
  | call `elem` [RC.Jobs, RC.ExportJobs] =  [("authtoken", [T.pack $ authtoken mainOpts])
                                            ,("project", [T.pack $ project mainOpts])]
  | otherwise = [("authtoken", [T.pack $ authtoken mainOpts])]

-- | The predecessor of a positive number
positivePred :: (Num a, Ord a, Enum a) => a -> a
positivePred n
  | n > 0 = pred n
  | otherwise = 0

-- lstrip :: L.ByteString -> L.ByteString
-- lstrip bs = L.dropWhile (\x -> x `elem` wschars) bs
--   where wschars = map (fromIntegral . ord) [' ', '\r', 'n', 't']

main :: IO ()
main = L.putStr =<< runSubcommand
    [ subcommand "system-info" $ get RC.SystemInfo
    , subcommand "projects" $ get RC.Projects
    , subcommand "tokens" $ get RC.Tokens
    , subcommand "jobs" $ get RC.Jobs
    , subcommand "export-jobs" $ get RC.ExportJobs
    , subcommand "runjob" $ runjob
    , subcommand "execution-output" $ executionOutput
    ]


get :: RC.ApiCall -> MainOptions -> NoSubOptions -> Args -> IO L.ByteString
get call mainOpts _ _ = do
  r <- RC.apiGet call (conninfo mainOpts) (params call mainOpts)
  return $ RC.body r


runjob :: MainOptions -> RunJobOptions -> Args -> IO L.ByteString
runjob mainOpts opts _ = do
  r <- RC.apiPost (RC.JobExecutions $ rjId opts) (conninfo mainOpts) (params (RC.JobExecutions $ rjId opts) mainOpts)
  let body = RC.body r
  if rjFollow opts
    then executionOutput mainOpts (ExecutionOutputOptions (T.unpack . executionId $ responseBodyCursor body) True) [""]
    else return body

executionOutput :: MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
executionOutput mainOpts opts _ = go "0"
  where go offset = do
          r <- RC.apiGet (RC.ExecutionOutput (eoId opts)) (conninfo mainOpts) (("offset", [offset]) : params (RC.ExecutionOutput (eoId opts)) mainOpts)
          let output = outputContent . responseBodyCursor $ RC.body r

          -- hacky way to get offset as num: See Data.Text.Read
          -- we have to decrement the offset by one or else rundeck give a fault
          let safeOffset = T.pack . show $ positivePred (read . T.unpack $ output "offset" :: Int)

          if output "completed" == "true"
            then return . L.fromStrict . TE.encodeUtf8 $ output "entries"
            else do
              TI.putStr . T.stripEnd $ output "entries"
              threadDelay $ 2 * 1000000  -- 2 seconds
              go safeOffset


-- mainopts = MainOptions "192.168.56.2" "4440" "fCg23CDrtT1uJxQsHYpCWPFoCfMEKSQk" "local"
-- eoopts = ExecutionOutputOptions "15" False
