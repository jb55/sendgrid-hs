{-# LANGUAGE TemplateHaskell #-}

module Email.SendGrid.Data.Email where

import Control.Lens
import Data.Aeson

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

makeLenses ''Email
