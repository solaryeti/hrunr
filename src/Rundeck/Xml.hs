{-# LANGUAGE OverloadedStrings #-}
module Rundeck.Xml
       ( executionId
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
