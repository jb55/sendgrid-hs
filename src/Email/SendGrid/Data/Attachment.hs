
module Email.SendGrid.Data.Attachment where

import Data.Aeson
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BL

data Attachment = Attachment {
      _attachmentContent     :: BL.ByteString
    , _attachmentType        :: Maybe Text
    , _attachmentFilename    :: Text
    , _attachmentDisposition :: Maybe Text
    , _attachmentContentId   :: Maybe Text
    }
    deriving (Eq, Show)

instance ToJSON Attachment where
  toJSON a = object [
      "content"     .= Base64.encode (a^.attachmentContent)
    , "type"        .= (a^.attachmentType)
    , "filename"    .= (a^.attachmentFilename)
    , "disposition" .= (a^.attachmentDisposition)
    , "content_id"  .= (a^.attachmentContentId)
    ]
