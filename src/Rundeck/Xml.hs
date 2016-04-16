{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : $Header$
Description : Processing of Rundeck's XML output
Copyright   : (c) 2016 Steven Meunier

License     : BSD-style (see the file LICENSE)
Maintainer  : code@solaryeti.com
Stability   : experimental
Portability : GHC
-}

module Rundeck.Xml
  ( executionId
  , jobId
  , outputContent
  , responseBodyCursor
  ) where

import qualified Data.ByteString.Lazy    as L
import qualified Data.Text               as T

-- For XML processing
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.XML.Types          (Name)
import           Text.XML                (Node, def, parseText_)
import           Text.XML.Cursor         (Cursor, attribute, content, element,
                                          fromDocument, ($/), (&/), (&//),
                                          (>=>))
import           Text.XML.Cursor.Generic (Cursor)

outputContent :: Text.XML.Cursor.Generic.Cursor Text.XML.Node -> Data.XML.Types.Name -> T.Text
outputContent cursor e = T.concat $ cursor $/ element "output" &/ element e &// content

executionId :: Text.XML.Cursor.Generic.Cursor Text.XML.Node -> T.Text
executionId cursor = T.concat $ cursor $/ element "execution" >=> attribute "id"

responseBodyCursor :: L.ByteString -> Text.XML.Cursor.Cursor
responseBodyCursor = fromDocument . parseText_ def . decodeUtf8

-- | Get the job id of a specified job. Input is expected to be the
-- response body of a 'Jobs' get.
jobId :: Text.XML.Cursor.Generic.Cursor Text.XML.Node -> T.Text -> Maybe T.Text
jobId cursor fullname = case ids of
    [x] -> Just x
    _   -> Nothing
  where
    ids = jobid <$> filter (\x -> join x == fullname || join x == T.append "/" fullname) jobs
    jobs = cursor $/ element "jobs" &/ element "job"
    join x = T.intercalate "/" [jobElement "group" x, jobElement "name" x]
    jobElement e x = T.concat $ x $/ element e &// content
    jobid = T.concat . attribute "id"
