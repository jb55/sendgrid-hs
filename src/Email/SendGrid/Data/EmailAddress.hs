{-# LANGUAGE TemplateHaskell #-}

module Email.SendGrid.Data.EmailAddress where

import Data.Aeson

newtype EmailAddress = EmailAddress { _getEmailAddress :: Text }
  deriving (Eq, Show)

instance ToJSON EmailAddress where
  toJSON (EmailAddress e) = toJSON e

makeLenses ''EmailAddress
