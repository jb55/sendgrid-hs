{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Email.SendGrid.Data.EmailAddress where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text)

data EmailAddress = EmailAddress {
    _recipientEmail :: Text
  , _recipientName  :: Maybe Text
  }
  deriving (Eq, Show)

makeLenses ''EmailAddress

instance ToJSON EmailAddress where
  toJSON r = object [
        "email" .= (r^.recipientEmail)
      , "name"  .= (r^.recipientName)
    ]
