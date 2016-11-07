
# sendgrid-hs

Lens-based SendGrid v3 library for Haskell

WIP

## Example

```haskell
import Control.Lens
import Data.Aeson.Lens (key, values)
import Network.HTTP.Client (responseBody)
import Email.SendGrid

testEmail :: Email
testEmail =
  initEmail toAddr fromAddr body
    & emailSubject        .~ "Test email"
    & emailToAddresses._2 %~ cons (EmailAddress "bill@casarin.me" Nothing)
    & emailAttachments    .~ [ attachment ]
  where
    attachment = makeAttachment "hello" "hello.txt"
    toAddr     = EmailAddress "jackbox55@gmail.com" (Just "William")
    fromAddr   = EmailAddress "test@monad." (Just "Robot")
    body       = textBody "hello, world"

testSend :: IO ()
testSend = do
  res <- sendEmail (ApiKey "key") testEmail
  traverseOf_ (to responseBody . key "errors" . values) print res
```
