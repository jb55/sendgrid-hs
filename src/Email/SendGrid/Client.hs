{-# LANGUAGE OverloadedStrings #-}

module Email.SendGrid.Client
    ( sendEmail
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.ByteString.Lazy (ByteString)
import Email.SendGrid.Data.Email

import qualified Data.Aeson as Aeson

sendEmail :: Email -> IO (Response ByteString)
sendEmail email = do
  manager <- newManager tlsManagerSettings
  req     <- parseRequest "https://api.sendgrid.com/v3/mail/send"
  let body    = Aeson.encode email
      request = req { method = "POST"
                    , requestBody = RequestBodyLBS body
                    }
  httpLbs request manager
