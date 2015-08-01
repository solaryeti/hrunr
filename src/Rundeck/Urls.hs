module Rundeck.Urls
       (exportUrl
       ,importUrl
       ,tokenUrl
       ,tokensUrl
       ,projectsUrl
       ,jobsUrl
       ,projectJobsUrl
       ,systemInfoUrl
       ,jobExecutionsUrl
       , Id
       ) where

data Rundeck = Rundeck { host :: String
                       , port :: String
                       }

type ApiUrl = String
type Id = String

rundeck :: Rundeck
rundeck = Rundeck { host = "http://192.168.56.2", port = "4440" }

url :: String
url = host rundeck ++ ":" ++ port rundeck

exportUrl :: ApiUrl
exportUrl = url ++ "/api/1/jobs/export"

importUrl :: ApiUrl
importUrl = url ++ "/api/1/jobs/import"

tokenUrl :: Id -> ApiUrl
tokenUrl i = url ++ "/api/11/token/" ++ i

tokensUrl :: ApiUrl
tokensUrl = url ++ "/api/11/tokens"

projectsUrl :: ApiUrl
projectsUrl = url ++ "/api/1/projects"

jobsUrl :: ApiUrl
jobsUrl = url ++ "/api/1/jobs"

projectJobsUrl :: String -> ApiUrl
projectJobsUrl name = url ++ "/api/2/project" ++ name ++ "/jobs"

systemInfoUrl :: ApiUrl
systemInfoUrl = url ++ "/api/2/system/info"

jobExecutionsUrl :: Id -> ApiUrl
jobExecutionsUrl i = url ++ "/api/12/job/" ++ i ++ "/executions"
