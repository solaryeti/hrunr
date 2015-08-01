{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
-- :set -XOverloadedStrings
module Main where

import qualified Rundeck.Call as RC

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

data Hrunr = Jobs
           | Projects
           deriving (Show, Data, Typeable)

mode :: IO Hrunr
mode = cmdArgs $ modes [jobs, projects]

run :: Hrunr -> IO ()
run Jobs = do
  r <- RC.jobs
  print $ r ^. responseBody

run Projects = do
  r <- RC.projects
  print $ r ^. responseBody

main :: IO ()
main = run =<< mode

jobs :: Hrunr
jobs = Jobs

projects ::Hrunr
projects = Projects

-- jobs = do
--   r <- RC.jobs
--   print $ r ^. responseBody

-- projects = do
--   r <- RC.projects
--   print $ r ^. responseBody

-- rundeckVersion :: IO T.Text
-- rundeckVersion = do
--   r <- systemInfo
--   let x = decodeUtf8 $ r ^. responseBody
--   let doc = parseText_ def x
--   let cursor = fromDocument doc
--   return $ T.concat $ versionContent cursor
--   where versionContent c = c $/ element "system" &/ element "rundeck" &/ element "version" &// content
