#ByteString
---
## use `Char8` with caution
- `Char8` siliently truncate the 4 bytes `Char` to 1 byte `Char8`, it's only sensiable when you use to store ASCII characters.
- `unpack . pack` for `Char8` is therefore not guaranteed when handling Non-ASCII encodings.

## Conversion
- String -> ByteString
    - Data.ByteString.UTF8.	fromString	:: String -> ByteString
- Text -> ByteString
    - Data.Text.Encoding decodeUtf8 :: ByteString -> Text
