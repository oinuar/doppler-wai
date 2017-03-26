module Network.Wai.Doppler (
   responseHtml
) where

import Network.Wai
import Doppler.Html.Types
import qualified Doppler.Css.Types as Css
import Network.HTTP.Types
import Data.Binary.Builder
import Data.Char                      (toLower)

responseHtml :: Status -> ResponseHeaders -> Html -> Response
responseHtml status headers =
     responseBuilder status headers
   . (putStringUtf8 "<!DOCTYPE html>" `append`)
   . toHtmlBuilder

toHtmlBuilder :: Html -> Builder
toHtmlBuilder (FullTag name attributes children) =
   let lowerCaseName = map toLower name
   in          putCharUtf8 '<'
      `append` putStringUtf8 lowerCaseName
      `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
      `append` putCharUtf8 '>'
      `append` foldr (append . toHtmlBuilder) empty children
      `append` putStringUtf8 "</"
      `append` putStringUtf8 lowerCaseName
      `append` putCharUtf8 '>'

toHtmlBuilder (Content (Plain content)) =
   putStringUtf8 content

toHtmlBuilder _ =
   empty

toAttributeBuilder :: HtmlAttribute -> Builder
toAttributeBuilder (key, values) =
            putStringUtf8 key
   `append` putCharUtf8 '='
   `append` putCharUtf8 '"'
   `append` toAttributeValueBuilder values
   `append` putCharUtf8 '"'

toAttributeValueBuilder :: [HtmlAttributeValue] -> Builder
toAttributeValueBuilder =
   foldr (append . toValueBuilder) empty

toValueBuilder :: HtmlAttributeValue -> Builder
toValueBuilder (Value content) =
   putStringUtf8 content

toValueBuilder (StyleValue (name, values)) =
            putStringUtf8 name
   `append` putCharUtf8 ':'
   `append` foldr (append . toCssValueBuilder) empty values
   `append` putCharUtf8 ';'

toValueBuilder _ =
   empty

toCssValueBuilder :: Css.CssPropertyValue -> Builder
toCssValueBuilder (Css.Value content) =
   putStringUtf8 content

toCssValueBuilder _ =
   empty

appendWithSpace :: Builder -> Builder -> Builder
appendWithSpace lhs rhs =
   putCharUtf8 ' ' `append` lhs `append` rhs
