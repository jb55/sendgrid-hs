{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Email.SendGrid.Data.Attachment where

import Data.Aeson
import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Base64.Lazy as Base64

data Attachment = Attachment {
      _attachmentContent     :: BL.ByteString
    , _attachmentType        :: Maybe Text -- ^ mime type
    , _attachmentFilename    :: Text
    , _attachmentDisposition :: Maybe Text
    , _attachmentContentId   :: Maybe Text
    }
    deriving (Show, Eq)

makeLenses ''Attachment

encodeAttachment :: BL.ByteString -> TL.Text
encodeAttachment dat = decodeUtf8 (Base64.encode dat)

makeAttachment :: BL.ByteString -> Text -> Attachment
makeAttachment dat fileName =
  Attachment {
    _attachmentContent     = dat
  , _attachmentFilename    = fileName
  , _attachmentDisposition = Nothing -- default is attachment
  , _attachmentContentId   = Nothing
  , _attachmentType        = Nothing
  }


instance ToJSON Attachment where
  toJSON a = object [
      "content"     .= encodeAttachment (a^.attachmentContent)
    , "type"        .= (a^.attachmentType)
    , "filename"    .= (a^.attachmentFilename)
    , "disposition" .= (a^.attachmentDisposition)
    , "content_id"  .= (a^.attachmentContentId)
    ]
