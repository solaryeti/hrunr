{-# LANGUAGE OverloadedStrings #-}
module Rundeck.Xml
       ( outputContent
       , responseBodyCursor
       ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

-- For XML processing
import           Data.Text.Lazy.Encoding  (decodeUtf8)
import           Text.XML (parseText_, def, Node)
import           Text.XML.Cursor (content, element, fromDocument, (&/), (&//), ($/), Cursor)
import           Text.XML.Cursor.Generic (Cursor)
import           Data.XML.Types (Name)

outputContent :: Text.XML.Cursor.Generic.Cursor Text.XML.Node -> Data.XML.Types.Name -> T.Text
outputContent cursor e = T.concat $ cursor $/ element "output" &/ element e &// content

responseBodyCursor :: L.ByteString -> Text.XML.Cursor.Cursor
responseBodyCursor = fromDocument . parseText_ def . decodeUtf8
