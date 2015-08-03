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
