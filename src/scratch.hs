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
