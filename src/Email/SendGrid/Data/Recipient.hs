{-# LANGUAGE TemplateHaskell #-}

module Email.SendGrid.Data.Recipient where

import Control.Lens
import Data.Text (Text)

data Recipient = Recipient {
    _recipientEmail :: EmailAddress
  , _recipientName  :: Maybe Text
  }
  deriving (Eq, Show)

instance ToJSON Recipient where
  toJSON r = object [
      "email" .= toJSON (r^.)
    ]

makeLenses ''Recipient
