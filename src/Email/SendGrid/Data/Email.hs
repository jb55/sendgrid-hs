{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Email.SendGrid.Data.Email where


import Email.SendGrid.Data.EmailAddress
import Email.SendGrid.Data.Attachment

import Data.Time.Clock (UTCTime)
import Control.Lens hiding ((.=))
import Data.Text (Text)
import Data.Maybe (catMaybes)
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
  , _emailSubject     :: Text
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
    , _emailSubject     = ""
    , _emailCategories  = []
    , _emailAttachments = []
  }

makeLenses ''Email
makeLenses ''Category
makeLenses ''EmailBody

instance ToJSON EmailBody where
  toJSON (EmailBody typ body) =
    object [ "type" .= typ, "value" .= body ]

infixr 5 ?:
(?:) :: Maybe a -> [a] -> [a]
Just x  ?: xs = x : xs
Nothing ?: xs = xs

notEmpty :: [a] -> Maybe [a]
notEmpty xs | null xs   = Nothing
            | otherwise = Just xs

instance ToJSON Email where
  toJSON email = core
    where
      (r, rs) = view emailToAddresses email
      personalizations =
        object $ [
          "to" .= (r : rs)
        ] ++ catMaybes [
            (.=) <$> Just "cc"          <*> notEmpty (email^.emailCc)
          , (.=) <$> Just "bcc"         <*> notEmpty (email^.emailBcc)
          , (.=) <$> Just "custom_args" <*> (email^.emailCustomArgs)
          , (.=) <$> Just "send_at"     <*> (email^.emailSendAt)
        ]
      core =
        object $ [
            "content" .= ((email^.emailBody._1) : (email^.emailBody._2))
          , "from"    .= (email^.emailFromAddress)
          , "subject" .= (email^.emailSubject)
          , "personalizations" .= [ personalizations ]
        ]
        ++ catMaybes [
            (.=) <$> Just "reply_to"    <*> notEmpty (email^.emailReplyTo)
          , (.=) <$> Just "attachments" <*> notEmpty (email^.emailAttachments)
          , (.=) <$> Just "categories"  <*> notEmpty (email^.emailCategories)
          , (.=) <$> Just "batch_id"    <*> (email^.emailBatchId)
        ]
