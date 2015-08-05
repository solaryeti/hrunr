-- {-# LANGUAGE TypeSynonymInstances #-}

-- :set -XOverloadedStrings

--import System.Console.CmdArgs

-- import qualified Data.Text as T
-- import Data.Map as Map
-- import Data.Aeson (Value)
-- import Data.Aeson.Lens

-- this is an orphan instance. Maybe we should just pack all our params?
-- instance SimpleOptionType Text where
--         simpleOptionType = optionType "text" "" (Right . pack) (show . unpack)


-- data Id = I Integer
--         | S String
--         | UUID String

-- instance Show Id where
--   show (I n) = show n
--   show (S s) = s
--   show (UUID s) = s

-- type Resp = Response (Map String Value)

-- query action = do
--   case getUrl action of
--     Nothing -> return Nothing
--     Just u -> do
--       r <- getWith opts u
--       return $ Just r

-- -- getUrl :: forall a. (Data.String.IsString a, Eq a) => a -> ApiUrl
-- getUrl "projects" = Just projectsUrl
-- getUrl "jobs" = Just jobsUrl
-- getUrl _ = Nothing




-- Working with json traversal
-- r <- getWith opts $ url ++ projects
-- r ^. responseBody ^.. values . key "name" . _String


-- xml
-- import           Data.Text.Lazy.Encoding  (decodeUtf8)
-- import           Text.XML (parseText_, def)
-- import           Text.XML.Cursor (content, element, fromDocument, (&/), (&//), ($/))

-- rundeckVersion :: IO T.Text
-- rundeckVersion = do
--   r <- systemInfo
--   let x = decodeUtf8 $ r ^. responseBody
--   let doc = parseText_ def x
--   let cursor = fromDocument doc
--   return $ T.concat $ versionContent cursor
--   where versionContent c = c $/ element "system" &/ element "rundeck" &/ element "version" &// content






    -- & param "authtoken"     .~ [pack token]
    -- & param "project"       .~ ["local"]
    -- & header "Accept"       .~ ["application/json"]
    -- & header "Content-Type" .~ ["application/json"]



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
-- mainopts = MainOptions "192.168.56.2" "4440" "fCg23CDrtT1uJxQsHYpCWPFoCfMEKSQk" "local"
-- eoopts = ExecutionOutputOptions "15" False
-- Main.executionOutput RC.ExecutionOutput mainopts eoopts [""]
-- r <- RC.executionOutput (conninfo mainopts) (params RC.ExecutionOutput mainopts) (eoId eoopts)
