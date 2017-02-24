module Network.Wai.Doppler (
   responseHTML
) where

import Doppler.HTML.Types
import Network.Wai
import Network.HTTP.Types
import Data.Binary.Builder
import Data.Char                   (toLower)
import qualified Doppler.CSS.Types as CSS

responseHTML :: Status -> ResponseHeaders -> Expression -> Response
responseHTML status headers =
     responseBuilder status headers
   . (putStringUtf8 "<!DOCTYPE html>" `append`)
   . toHTMLBuilder

toHTMLBuilder :: Expression -> Builder
toHTMLBuilder (Element name attrs childs) =
   let lowerCaseName = map toLower name
   in          putCharUtf8 '<'
      `append` putStringUtf8 lowerCaseName
      `append` foldr (appendWithSpace . toAttributeBuilder) empty attrs
      `append` putCharUtf8 '>'
      `append` foldr (append . toHTMLBuilder) empty childs
      `append` putStringUtf8 "</"
      `append` putStringUtf8 lowerCaseName
      `append` putCharUtf8 '>'

toHTMLBuilder (Text content) =
   putStringUtf8 content

toHTMLBuilder _ =
   empty

toAttributeBuilder :: Attribute -> Builder
toAttributeBuilder (Attribute (key, collection)) =
            putStringUtf8 key
   `append` putCharUtf8 '='
   `append` putCharUtf8 '"'
   `append` toCollectionBuilder collection
   `append` putCharUtf8 '"'

toCollectionBuilder :: Collection -> Builder
toCollectionBuilder (Values values) =
   foldr (append . toValueBuilder) empty values

toCollectionBuilder _ =
   empty

toValueBuilder :: Value -> Builder
toValueBuilder (StringValue content) =
   putStringUtf8 content

toValueBuilder (CSSValue (CSS.Property (name, values))) =
            putStringUtf8 name
   `append` putCharUtf8 ':'
   `append` foldr (append . toCSSBuilder) empty values
   `append` putCharUtf8 ';'

toValueBuilder _ =
   empty

toCSSBuilder :: CSS.Value -> Builder
toCSSBuilder (CSS.StringValue content) =
   putStringUtf8 content

toCSSBuilder _ =
   empty

appendWithSpace :: Builder -> Builder -> Builder
appendWithSpace lhs rhs =
   putCharUtf8 ' ' `append` lhs `append` rhs
