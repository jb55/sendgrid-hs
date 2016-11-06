{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 

module Email.SendGrid.Client
    ( sendEmail
    , ApiKey(..)
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Lens
import Data.Monoid ((<>))
import Data.CaseInsensitive
import Data.ByteString.Lazy (ByteString)
import Email.SendGrid.Data.Email
import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS

newtype ApiKey = ApiKey { _getApiKey :: BS.ByteString }
               deriving (Show, Eq)

makeLenses ''ApiKey

sendEmail :: ApiKey -> Email -> IO (Response ByteString)
sendEmail (ApiKey key) email = do
  manager <- newManager tlsManagerSettings
  req     <- parseRequest "https://api.sendgrid.com/v3/mail/send"
  let body    = Aeson.encode email
      headers = [
            (mk "authorization", "bearer " <> key)
          , (mk "content-type", "application/json")
        ]
      request = req { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = headers
                    }
  httpLbs request manager
