
# sendgrid-hs

Lens-based SendGrid v3 library for Haskell

WIP

## Example

```haskell
testEmail =
  initEmail toAddr fromAddr body
    & emailSubject        .~ "Test email"
    & emailToAddresses._2 %~ cons (EmailAddress "bill@casarin.me" Nothing)
  where
    toAddr   = EmailAddress "jackbox55@gmail.com" (Just "William")
    fromAddr = EmailAddress "test@local." (Just "Robot")
    body     = textBody "hello, world"

testSend = do
  res <- sendEmail (ApiKey "key") testEmail
  traverseOf_ (to responseBody . key "errors" . values) print res
```
