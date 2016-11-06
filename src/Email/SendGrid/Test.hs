{-# LANGUAGE OverloadedStrings #-}

module Email.SendGrid.Test where

import Control.Lens
import Email.SendGrid.Data.Email
import Email.SendGrid.Data.EmailAddress

testEmail =
  initEmail toAddr fromAddr body
    & emailSubject ?~ "Test email"
    & emailToAddresses._2 %~ cons (EmailAddress "bill@casarin.me" Nothing)
  where
    toAddr   = EmailAddress "jackbox55@gmail.com" (Just "William")
    fromAddr = EmailAddress "jackbox55@gmail.com" (Just "William")
    body     = textBody "hello, world"

testSend = 
