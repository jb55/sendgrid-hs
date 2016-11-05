{-# LANGUAGE TemplateHaskell #-}

module Email.SendGrid.Data
  ( module Email.SendGrid.Data.Attachment
  , module Email.SendGrid.Data.Email
  , module Email.SendGrid.Data.Recipient
  ) where

import Control.Lens
import Data.Aeson
import Data.Text (Text)

newtype TextBody = TextBody { _getTextBody :: Text }
  deriving (Eq, Show)

newtype HtmlBody = HtmlBody { _getHtmlBody :: Text }
  deriving (Eq, Show)

data EmailBody = HtmlAndText TextBody HtmlBody
               | JustText    TextBody
               | JustHtml    HtmlBody
               deriving (Eq, Show)

newtype Category = Category { _getCategory :: Text }
  deriving (Eq, Show)

makeLenses ''Category
makePrisms ''EmailBody
