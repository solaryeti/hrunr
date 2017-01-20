{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Rundeck Libraries
import           Rundeck.Call hiding (host, port)
import           Rundeck.Xml

-- For CLI options
import           Hrunr.Options

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
import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = execParseOptions >>= run

run :: Options -> IO ()
run (Options globalopts cmd) = do
    res <- E.try $ runCommand :: IO (Either HttpException L.ByteString)
    case res of
      Left e -> LC.putStrLn (errorHandler e) >> exitFailure
      Right r -> LC.putStrLn r
  where runCommand = case cmd of
                       CSystemInfo             -> doGet SystemInfo globalopts
                       CProjects               -> doGet Projects globalopts
                       CTokens                 -> doGet Tokens globalopts
                       CJobs                   -> doGet Jobs globalopts
                       CExportJobs             -> doGet ExportJobs globalopts
                       CRunJob rjopts          -> runjob globalopts rjopts
                       CJobID rjopts           -> getId globalopts rjopts
                       CExecutionOutput eoopts -> initExecutionOutput globalopts eoopts

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
selectResponseHeaders h = filter (\(x,_) -> x == h)

conninfo :: GlobalOptions -> Conninfo
conninfo globalOpts = Conninfo (host globalOpts) (port globalOpts)

params :: ApiCall -> GlobalOptions -> Params
params call globalOpts
  | call `elem` [Jobs, ExportJobs] = [("authtoken", T.pack $ authtoken globalOpts)
                                     ,("project", T.pack $ project globalOpts)]
  | otherwise = [("authtoken", T.pack $ authtoken globalOpts)]

-- | The predecessor of a positive number
positivePred :: (Num a, Ord a, Enum a) => a -> a
positivePred n
  | n > 0 = pred n
  | otherwise = 0

-- | Perform a standard get request to a Rundeck API endpoint.
-- Connections are not reused so multiple calls will each create a
-- separate connection.
doGet :: ApiCall -> GlobalOptions -> IO L.ByteString
doGet call globalOpts = body <$> get call (conninfo globalOpts) (params call globalOpts)

getId :: GlobalOptions -> JobOptions -> IO L.ByteString
getId globalOpts opts = do
  cursor <- responseBodyCursor <$> body <$> get Jobs (conninfo globalOpts) (params Jobs globalOpts)
  case jobId cursor (T.pack $ rjName opts) of
    Nothing -> (return . LC.pack) ("Job not found: " ++ rjName opts)
    Just x  -> (return . L.fromStrict . TE.encodeUtf8) x

-- | Trigger the run of a Rundeck job.
-- Connections are reused so enabling the @follow@ parameter will
-- still result in only a single connection to Rundeck.
runjob :: GlobalOptions -> JobOptions -> IO L.ByteString
runjob globalOpts rjopts =  withAPISession $ \sess -> do
    id' <- jobid sess
    res <- postWithSession sess (JobExecutions id') (conninfo globalOpts) jobparams
    if rjFollow rjopts
      then do
        -- Don't buffer output when following so that the user can see
        -- the output immediately.
        hSetBuffering stdout NoBuffering
        executionOutput sess globalOpts (executionOpts $ body res)
      else return $ body res
  where
    executionOpts b = ExecutionOutputOptions (T.unpack . executionId $ responseBodyCursor b) True
    jobid sess = do
      cursor <- responseBodyCursor <$> body <$> getWithSession sess Jobs (conninfo globalOpts) (params Jobs globalOpts)
      case jobId cursor (T.pack $ rjName rjopts) of
        Nothing -> return $ rjName rjopts
        Just x  -> return $ T.unpack x
    jobparams = ("argString", T.pack $ rjArgString rjopts) : params (JobExecutions "") globalOpts

-- | Initiate tailing execution output.
-- 'executionOutput' cannot be called without creating a session. This
-- functions acts as a helper to 'executionOutput' by creating the
-- needed session and then calling 'executionOutput'.
initExecutionOutput :: GlobalOptions -> ExecutionOutputOptions -> IO L.ByteString
initExecutionOutput globalOpts opts = withAPISession $ \sess -> executionOutput sess globalOpts opts

-- | Tail the output of a Rundeck job.
executionOutput :: Session -> GlobalOptions -> ExecutionOutputOptions -> IO L.ByteString
executionOutput sess globalOpts opts = go "0"
  where go offset =
          do
            r <- getWithSession sess call (conninfo globalOpts) $ ("offset", offset) : params call globalOpts
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
