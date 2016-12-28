module Hrunr.Options
  ( -- * Types
    Command(..)
  , GlobalOptions(..)
  , Options(..)
  , JobOptions(..)
  , ExecutionOutputOptions(..)

  , execParseOptions
  ) where

import Options.Applicative

data Command =
    CSystemInfo
  | CProjects
  | CTokens
  | CJobs
  | CExportJobs
  | CRunJob JobOptions
  | CJobID JobOptions
  | CExecutionOutput ExecutionOutputOptions
  deriving (Show)

data GlobalOptions = GlobalOptions
  { host      :: String
  , port      :: String
  , authtoken :: String
  , project   :: String }
  deriving (Show)

data Options = Options GlobalOptions Command

data JobOptions = JobOptions
  { rjName :: String
  , rjArgString :: String
  , rjFollow :: Bool }
  deriving (Show)

data ExecutionOutputOptions = ExecutionOutputOptions
  { eoId :: String
  , eoFollow :: Bool }
  deriving (Show)

parseGlobalOptions :: Parser GlobalOptions
parseGlobalOptions = GlobalOptions
  <$> strOption
      ( long "host"
     <> short 'H'
     <> metavar "HOST"
     <> help "Rundeck host"
     <> value "localhost"
     <> showDefault )
  <*> strOption
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Rundeck port"
     <> value "8080"
     <> showDefault )
  <*> strOption
      ( long "authtoken"
     <> short 't'
     <> metavar "TOKEN"
     <> help "Rundeck API token" )
  <*> strOption
      ( long "project"
     <> short 'j'
     <> metavar "PROJECT"
     <> help "Rundeck project"
     <> value "tst")

jobOptions :: Parser JobOptions
jobOptions = JobOptions
  <$> strOption
      ( long "name"
     <> metavar "JOBNAME"
     <> help "Job full name (including group) or ID" )
  <*> strOption
      ( long "argstring"
     <> metavar "ARGSTRING"
     <> help "Argument string to pass to the job, of the form: -opt value -opt2 value ..."
     <> value "" )
  <*> switch
      ( long "follow"
     <> help "Tail execution output" )

parseRunJobOptions :: Parser Command
parseRunJobOptions = CRunJob <$> jobOptions

parseJobIDOptions :: Parser Command
parseJobIDOptions = CJobID <$> jobOptions

parseExecutionOutputOptions :: Parser Command
parseExecutionOutputOptions = fmap CExecutionOutput $ ExecutionOutputOptions
  <$> strOption
      ( long "id"
     <> metavar "ID"
     <> help "Job to run" )
  <*> switch
      ( long "follow"
     <> help "Tail execution output" )

parseCommand :: Parser Command
parseCommand = subparser $
     command "system-info" (info (pure CSystemInfo) (progDesc "Query the system information"))
  <> command "projects" (info (pure CProjects) (progDesc "Query the available projects"))
  <> command "tokens" (info (pure CTokens) (progDesc "Query the available tokens"))
  <> command "jobs" (info (pure CJobs) (progDesc "Query the available jobs"))
  <> command "export-jobs" (info (pure CExportJobs) (progDesc "Export all jobs"))
  <> command "runjob" (info (helper <*> parseRunJobOptions) (progDesc "Run a specified job"))
  <> command "jobid" (info (helper <*> parseJobIDOptions) (progDesc "Get a job id"))
  <> command "execution-output" (info (helper <*> parseExecutionOutputOptions) (progDesc "Query the execution output of a job"))

parseOptions :: Parser Options
parseOptions = Options <$> parseGlobalOptions <*> parseCommand

execParseOptions :: IO Options
execParseOptions = execParser $ info (helper <*> parseOptions)
                           ( fullDesc
                             <> progDesc "cli tool to query Rundeck's API"
                             <> header "hrunr - cli tool for Rundeck" )
