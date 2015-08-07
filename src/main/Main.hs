{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Rundeck Libraries
-- import           Rundeck.Call         ()
import           Rundeck.Call         hiding (host, port)
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

conninfo :: MainOptions -> Conninfo
conninfo mainOpts = Conninfo (host mainOpts) (port mainOpts)

params :: ApiCall -> MainOptions -> Params
params call mainOpts
  | call `elem` [Jobs, ExportJobs] =  [("authtoken", [T.pack $ authtoken mainOpts])
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
    [ subcommand "system-info" $ doGet SystemInfo
    , subcommand "projects" $ doGet Projects
    , subcommand "tokens" $ doGet Tokens
    , subcommand "jobs" $ doGet Jobs
    , subcommand "export-jobs" $ doGet ExportJobs
    , subcommand "runjob" $ runjob
    , subcommand "execution-output" $ executionOutput
    ]


doGet :: ApiCall -> MainOptions -> NoSubOptions -> Args -> IO L.ByteString
doGet call mainOpts _ _ = get call (conninfo mainOpts) (params call mainOpts) >>= return . body

runjob :: MainOptions -> RunJobOptions -> Args -> IO L.ByteString
runjob mainOpts opts _ = post call (conninfo mainOpts) (params call mainOpts) >>= \r ->
  if rjFollow opts
    then executionOutput mainOpts (executionOpts $ body r) [""]
    else return $ body r
  where call = JobExecutions $ rjId opts
        executionOpts b = (ExecutionOutputOptions (T.unpack . executionId $ responseBodyCursor b) True)

executionOutput :: MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
executionOutput mainOpts opts _ = go "0"
  where go offset = do
          r <- get call (conninfo mainOpts) $ ("offset", [offset]) : params call mainOpts
          let output = outputContent . responseBodyCursor $ body r

          -- hacky way to get offset as num: See Data.Text.Read
          -- we have to decrement the offset by one or else rundeck give a fault
          let safeOffset = T.pack . show $ positivePred (read . T.unpack $ output "offset" :: Int)

          if output "completed" == "true"
            then return . L.fromStrict . TE.encodeUtf8 $ output "entries"
            else do
              TI.putStr . T.stripEnd $ output "entries"
              threadDelay $ 2 * 1000000  -- 2 seconds
              go safeOffset

        call = ExecutionOutput $ eoId opts


-- mainopts = MainOptions "192.168.56.2" "4440" "fCg23CDrtT1uJxQsHYpCWPFoCfMEKSQk" "local"
-- eoopts = ExecutionOutputOptions "15" False
