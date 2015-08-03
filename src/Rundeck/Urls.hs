module Rundeck.Urls
       ( exportUrl
       , importUrl
       , tokenUrl
       , tokensUrl
       , projectsUrl
       , jobsUrl
       , projectJobsUrl
       , systemInfoUrl
       , jobExecutionsUrl
       , executionOutputUrl
       , Id
       ) where

type ApiUrl = String
type Id = String

exportUrl :: ApiUrl
exportUrl = "/api/1/jobs/export"

importUrl :: ApiUrl
importUrl = "/api/1/jobs/import"

tokenUrl :: Id -> ApiUrl
tokenUrl i = "/api/11/token/" ++ i

tokensUrl :: ApiUrl
tokensUrl = "/api/11/tokens"

projectsUrl :: ApiUrl
projectsUrl = "/api/1/projects"

jobsUrl :: ApiUrl
jobsUrl = "/api/1/jobs"

projectJobsUrl :: String -> ApiUrl
projectJobsUrl name = "/api/2/project" ++ name ++ "/jobs"

systemInfoUrl :: ApiUrl
systemInfoUrl = "/api/2/system/info"

jobExecutionsUrl :: Id -> ApiUrl
jobExecutionsUrl i = "/api/12/job/" ++ i ++ "/executions"

executionOutputUrl :: Id -> ApiUrl
executionOutputUrl i = "/api/5/execution/" ++ i ++ "/output"
