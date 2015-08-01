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



-- rundeckVersion :: IO T.Text
-- rundeckVersion = do
--   r <- systemInfo
--   let x = decodeUtf8 $ r ^. responseBody
--   let doc = parseText_ def x
--   let cursor = fromDocument doc
--   return $ T.concat $ versionContent cursor
--   where versionContent c = c $/ element "system" &/ element "rundeck" &/ element "version" &// content
