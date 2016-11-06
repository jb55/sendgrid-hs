{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Email.SendGrid.Data.Attachment where

import Data.Aeson
import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.Lazy as Base64

data Attachment = Attachment {
      _attachmentContent     :: BL.ByteString
    , _attachmentType        :: Maybe Text
    , _attachmentFilename    :: Text
    , _attachmentDisposition :: Maybe Text
    , _attachmentContentId   :: Maybe Text
    }
    deriving (Eq, Show)

makeLenses ''Attachment

instance ToJSON Attachment where
  toJSON a = object [
      "content"     .= decodeUtf8 (Base64.encode (a^.attachmentContent))
    , "type"        .= (a^.attachmentType)
    , "filename"    .= (a^.attachmentFilename)
    , "disposition" .= (a^.attachmentDisposition)
    , "content_id"  .= (a^.attachmentContentId)
    ]
