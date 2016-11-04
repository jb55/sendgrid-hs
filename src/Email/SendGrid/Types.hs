
module Email.SendGrid.Types where

import Control.Lens
import Data.Aeson

newtype TextBody = TextBody { _getTextBody :: Text }
  deriving (Eq, Show)

newtype HtmlBody = HtmlBody { _getHtmlBody :: Text }
  deriving (Eq, Show)

data EmailBody = HtmlAndText TextBody HtmlBody
               | JustText    TextBody
               | JustHtml    HtmlBody
               deriving (Eq, Show)

newtype EmailAddress = EmailAddress { _getEmailAddress :: Text }
  deriving (Eq, Show)

newtype Category = Category { _getCategory :: Text }
  deriving (Eq, Show)

data Recipient = Recipient {
    _recipientEmail :: EmailAddress
  , _recipientName  :: Maybe Text
  }
  deriving (Eq, Show)

data Attachment = Attachment {
      _attachmentContent     :: BL.ByteString
    , _attachmentType        :: Maybe Text
    , _attachmentFilename    :: Text
    , _attachmentDisposition :: Maybe Text
    , _attachmentContentId   :: Maybe Text
    }
    deriving (Eq, Show)

-- TODO: mail_settings
-- TODO: asm
data Email = Email {
    _emailToAddresses :: (Recipient, [Recipient])
  , _emailCc          :: [Recipient]
  , _emailBcc         :: [Recipient]
  , _emailReplyTo     :: [Recipient]
  , _emailFromAddress :: EmailAddress
  , _emailSenderName  :: Text
  , _emailSendAt      :: Maybe UTCTime
  , _emailBatchId     :: Maybe Text
  , _emailCustomArgs  :: Maybe Value
  , _emailBody        :: EmailBody
  , _emailSubject     :: Maybe Text
  , _emailCategories  :: [Category]
  , _emailAttachments :: [Attachment]
  }
  deriving (Eq, Show)

instance ToJSON EmailAddress where
  toJSON (EmailAddress e) = toJSON e

instance ToJSON Recipient where
  toJSON r = object [
      "email" .= toJSON (r^.)
    ]

instance ToJSON Attachment where
  toJSON a = object [
      "content"     .= Base64.encode (a^.attachmentContent)
    , "type"        .= (a^.attachmentType)
    , "filename"    .= (a^.attachmentFilename)
    , "disposition" .= (a^.attachmentDisposition)
    , "content_id"  .= (a^.attachmentContentId)
    ]

instance ToJSON Email where
  toJSON email = object [
      "to"          .= (r : rs)
    , "cc"          .= (email^.emailCc)
    , "bcc"         .= (email^.emailBcc)
    , "reply_to"    .= (email^.emailBcc)
    , "subject"     .= (email^.emailSubject)
    , "content"     .= (email^.emailBody)
    , "attachments" .= (email^.emailAttachments)
    , "categories"  .= (email^.emailCategories)
    , "custom_args" .= (email^.emailCustomArgs)
    , "send_at"     .= (email^.emailSendAt)
    , "batch_id"    .= (email^.emailBatchId)
    ]
    where
      (r1, rs) = view emailToAddresses email

makeLenses ''EmailAddress
makeLenses ''Recipient
makeLenses ''Email
makeLenses ''Category
makePrisms ''EmailBody
