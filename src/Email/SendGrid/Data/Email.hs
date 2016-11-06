{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Email.SendGrid.Data.Email where


import Email.SendGrid.Data.EmailAddress
import Email.SendGrid.Data.Attachment

import Data.Time.Clock (UTCTime)
import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.Aeson

newtype TextBody = TextBody { _getTextBody :: Text }
  deriving (Eq, Show)

newtype HtmlBody = HtmlBody { _getHtmlBody :: Text }
  deriving (Eq, Show)

newtype Category = Category { _getCategory :: Text }
  deriving (Eq, Show, ToJSON)

htmlAndTextBody :: TextBody -> HtmlBody -> (EmailBody, [EmailBody])
htmlAndTextBody (TextBody txt) (HtmlBody html) =
  (htmlBody html, [textBody txt])

textBody :: Text -> EmailBody
textBody txt  = EmailBody "text/plain" txt

htmlBody :: Text -> EmailBody
htmlBody html = EmailBody "text/html" html

data EmailBody = EmailBody Text Text
               deriving (Eq, Show)

-- TODO: mail_settings
-- TODO: asm
data Email = Email {
    _emailToAddresses :: (EmailAddress, [EmailAddress])
  , _emailCc          :: [EmailAddress]
  , _emailBcc         :: [EmailAddress]
  , _emailReplyTo     :: [EmailAddress]
  , _emailFromAddress :: EmailAddress
  , _emailSendAt      :: Maybe UTCTime
  , _emailBatchId     :: Maybe Text
  , _emailCustomArgs  :: Maybe Value
  , _emailBody        :: (EmailBody, [EmailBody])
  , _emailSubject     :: Maybe Text
  , _emailCategories  :: [Category]
  , _emailAttachments :: [Attachment]
  }
  deriving (Eq, Show)


initEmail :: EmailAddress -> EmailAddress -> EmailBody -> Email
initEmail toAddr fromAddr body =
  Email {
      _emailToAddresses = (toAddr, [])
    , _emailCc          = []
    , _emailBcc         = []
    , _emailReplyTo     = []
    , _emailFromAddress = fromAddr
    , _emailSendAt      = Nothing
    , _emailBatchId     = Nothing
    , _emailCustomArgs  = Nothing
    , _emailBody        = (body, [])
    , _emailSubject     = Nothing
    , _emailCategories  = []
    , _emailAttachments = []
  }

makeLenses ''Email
makeLenses ''Category
makeLenses ''EmailBody

instance ToJSON EmailBody where
  toJSON (EmailBody typ body) =
    object [ "type" .= typ, "value" .= body ]

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
      (r, rs) = view emailToAddresses email
