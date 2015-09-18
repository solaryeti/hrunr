{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Rundeck Libraries
import           Rundeck.Call hiding (host, port)
import           Rundeck.Xml

-- For CLI options
import           Options

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import           Control.Lens ((^.))
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict)
import           Data.ByteString.Lazy.Char8 as LC (putStrLn, pack, append)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TI (putStr)
import           Network.HTTP.Client (HttpException(..))
import           Network.HTTP.Types.Header (ResponseHeaders, HeaderName)
import           Network.Wreq (statusCode, statusMessage)
import           System.Exit

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

data RunJobOptions = RunJobOptions
  { rjId :: String
  , rjName :: String
  , rjArgString :: String
  , rjFollow :: Bool
  }

instance Options RunJobOptions where
  defineOptions = pure RunJobOptions
    <*> simpleOption "id" "" "Job to run"
    <*> simpleOption "name" "" "Full name (including group) of job to run"
    <*> simpleOption "argstring" "" "Argument string to pass to the job, of the form: -opt value -opt2 value ..."
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
  | call `elem` [Jobs, ExportJobs] = [("authtoken", [T.pack $ authtoken mainOpts])
                                     ,("project", [T.pack $ project mainOpts])]
  | otherwise = [("authtoken", [T.pack $ authtoken mainOpts])]

-- | The predecessor of a positive number
positivePred :: (Num a, Ord a, Enum a) => a -> a
positivePred n
  | n > 0 = pred n
  | otherwise = 0

errorHandler :: HttpException -> L.ByteString
errorHandler (StatusCodeException s r _) =
  LC.pack $ "Error: Rundeck returned status code " ++
    show (s ^. statusCode) ++ " - " ++ show (s ^. statusMessage) ++
    "\nWith a response of: \n" ++
    show (snd <$> selectResponseHeaders "X-Response-Body-Start" r)
errorHandler e =
  LC.pack "Error: encountered an unknown error. Full error output as follows:\n  "
    `LC.append` LC.pack (show e)

selectResponseHeaders :: HeaderName -> ResponseHeaders -> ResponseHeaders
selectResponseHeaders h = filter (\(x,_) -> x == h )

main :: IO ()
main = do
  res <- E.try $ runSubcommand
    [ subcommand "system-info" $ doGet SystemInfo
    , subcommand "projects" $ doGet Projects
    , subcommand "tokens" $ doGet Tokens
    , subcommand "jobs" $ doGet Jobs
    , subcommand "export-jobs" $ doGet ExportJobs
    , subcommand "runjob" runjob
    , subcommand "jobid" getId
    , subcommand "execution-output" initExecutionOutput
    ] :: IO (Either HttpException L.ByteString)
  case res of
    Left e -> LC.putStrLn (errorHandler e) >> exitFailure
    Right r -> LC.putStrLn r

-- | Perform a standard get request to a Rundeck API endpoint.
-- Connections are not reused so multiple calls will each create a
-- separate connection.
doGet :: ApiCall -> MainOptions -> NoSubOptions -> Args -> IO L.ByteString
doGet call mainOpts _ _ = body <$> get call (conninfo mainOpts) (params call mainOpts)

getId :: MainOptions -> RunJobOptions -> Args -> IO L.ByteString
getId mainOpts opts _ = do
  cursor <- responseBodyCursor <$> body <$> get Jobs (conninfo mainOpts) (params Jobs mainOpts)
  case jobId cursor (T.pack $ rjId opts) of
    Nothing -> (return . LC.pack) ("Job not found: " ++ rjId opts)
    Just x  -> (return . L.fromStrict . TE.encodeUtf8) x

-- | Trigger the run of a Rundeck job.
-- Connections are reused so enabling the @follow@ parameter will
-- still result in only a single connection to Rundeck.
runjob :: MainOptions -> RunJobOptions -> Args -> IO L.ByteString
runjob mainOpts opts _ =  withAPISession $ \sess -> do
    id' <- jobid sess
    res <- postWithSession sess (JobExecutions id') (conninfo mainOpts) jobparams
    if rjFollow opts
      then executionOutput sess mainOpts (executionOpts $ body res) [""]
      else return $ body res
  where
    executionOpts b = ExecutionOutputOptions (T.unpack . executionId $ responseBodyCursor b) True
    jobid sess = do
      cursor <- responseBodyCursor <$> body <$> getWithSession sess Jobs (conninfo mainOpts) (params Jobs mainOpts)
      case jobId cursor (T.pack $ rjId opts) of
        Nothing -> return $ rjId opts
        Just x  -> return $ T.unpack x
    jobparams = ("argString", [T.pack $ rjArgString opts]) : params (JobExecutions "") mainOpts

-- | Initiate tailing execution output.
-- 'executionOutput' cannot be called without creating a session. This
-- functions acts as a helper to 'executionOutput' by creating the
-- needed session and then calling 'executionOutput'.
initExecutionOutput :: MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
initExecutionOutput mainOpts opts eopts = withAPISession $ \sess -> executionOutput sess mainOpts opts eopts

-- | Tail the output of a Rundeck job.
executionOutput :: Session -> MainOptions -> ExecutionOutputOptions -> Args -> IO L.ByteString
executionOutput sess mainOpts opts _ = go "0"
  where go offset =
          do
            r <- getWithSession sess call (conninfo mainOpts) $ ("offset", [offset]) : params call mainOpts
            let output = (outputContent . responseBodyCursor . body) r

            -- hacky way to get offset as num: See Data.Text.Read
            -- we have to decrement the offset by one or else rundeck give a fault
            let safeOffset = (T.pack . show . positivePred) ((read . T.unpack . output) "offset" :: Int)

            if output "completed" == "true"
              then (return . L.fromStrict . TE.encodeUtf8 . output) "entries"
              else
                do
                  (TI.putStr . T.stripEnd . output) "entries"
                  threadDelay $ 2 * 1000000  -- 2 seconds
                  go safeOffset

        call = ExecutionOutput $ eoId opts

-- mainopts = MainOptions "192.168.56.2" "4440" "fCg23CDrtT1uJxQsHYpCWPFoCfMEKSQk" "local"
-- let mainopts = MainOptions "172.28.129.5" "8080" "gzNIaePk9jEcBqldLozZrLyreOh6dL5A" "sgm"
-- eoopts = ExecutionOutputOptions "15" False
-- doGet Jobs mainopts NoSubOptions [""]
-- let r = get Jobs (conninfo mainopts) (params Jobs mainopts)
-- cursor <- responseBodyCursor <$> body <$> get Jobs (conninfo mainopts) (params Jobs mainopts)
-- let jobs = cursor $/ element "jobs" &/ element "job"
-- map (\x -> ((x $/ element "group" &// content),(x $/ element "name" &// content), (attribute "id" x))) jobs
