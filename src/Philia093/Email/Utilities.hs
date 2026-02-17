module Philia093.Email.Utilities where

import Control.Lens
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (CI (original))
import Data.IMF
import Data.MIME (MIMEMessage, mime)
import Data.MIME.Charset (defaultCharsets)
import Data.MIME.EncodedWord (decodeEncodedWords)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.LocalTime
import Philia093.Email.EmailTypes qualified as ET

-- Purebred's Mailbox is the Address type of us, so it may seem a bit confusing
purebredMailboxToAddress :: Mailbox -> ET.Address
purebredMailboxToAddress (Mailbox name addrSpec) =
  ET.Address name (decodeUtf8Lenient $ renderAddressSpec addrSpec)

-- Purebred's Address may contain a name and multiple email addresses, but we
-- only care about the email addresses, so we can convert it to a list of our
-- Address type.
purebredAddressToList :: Address -> [ET.Address]
purebredAddressToList (Single mb) = [purebredMailboxToAddress mb]
purebredAddressToList (Group _ mbs) = map purebredMailboxToAddress mbs

purebredHeaderListsToEmailHeaders :: [Header] -> [(Text, Text)]
purebredHeaderListsToEmailHeaders =
  map
    ( \(name, value) ->
        ( decodeUtf8Lenient $ original name,
          decodeEncodedWords defaultCharsets value
        )
    )

-- Parse a MIMEMessage into our Email type
parseMIMEMessage :: MIMEMessage -> ET.Email
parseMIMEMessage mimeMsg =
  ET.Email
    { emailId = ET.EmailId _id, -- TODO: Not sure if this is the same as IMAP ID
      from = concatMap purebredAddressToList _from,
      to = concatMap purebredAddressToList _to,
      cc = concatMap purebredAddressToList _cc,
      bcc = concatMap purebredAddressToList _bcc,
      subject = _subject,
      bodyText = Nothing,
      bodyHtml = Nothing,
      date = _date,
      attachments = [],
      headers = _headers
    }
  where
    _id = maybe "0" (decodeUtf8Lenient . renderMessageID) $ mimeMsg ^. headerMessageID
    _from = mimeMsg ^. headerFrom defaultCharsets
    _to = mimeMsg ^. headerTo defaultCharsets
    _cc = mimeMsg ^. headerCC defaultCharsets
    _bcc = mimeMsg ^. headerBCC defaultCharsets
    _subject = fromMaybe "No Subject" $ mimeMsg ^. headerSubject defaultCharsets
    _date = zonedTimeToUTC <$> mimeMsg ^. headerDate
    _headers = purebredHeaderListsToEmailHeaders $ mimeMsg ^. headerList

parseEmail :: BL.ByteString -> Maybe ET.Email
parseEmail bs = do
  let parseResult = parse (message mime) bs
  case parseResult of
    Left _ -> Nothing
    Right msg -> Just $ parseMIMEMessage msg
