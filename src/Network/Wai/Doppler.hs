module Network.Wai.Doppler (
   responseHtml, responseCss, responseXml
) where

import Network.Wai
import Doppler.Html.Types
import qualified Doppler.Css.Types as Css
import Network.HTTP.Types
import Data.Binary.Builder

responseHtml :: Status -> ResponseHeaders -> Html -> Response
responseHtml status headers =
     responseBuilder status headers
   . (putStringUtf8 "<!DOCTYPE html>" `append`)
   . toHtmlBuilder

responseCss :: Status -> ResponseHeaders -> [Css.Css] -> Response
responseCss status headers =
     responseBuilder status headers
   . foldr (append . toCssBuilder) empty

-- TODO: change Html to Xml when parser and types are ready.
responseXml :: Status -> ResponseHeaders -> Html -> Response
responseXml status headers =
     responseBuilder status headers
   . (putStringUtf8 "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" `append`)
   . toHtmlBuilder

toHtmlBuilder :: Html -> Builder
toHtmlBuilder (FullTag name attributes children) =
            putCharUtf8 '<'
   `append` putStringUtf8 name
   `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
   `append` putCharUtf8 '>'
   `append` toHtmlChildrenBuilder children
   `append` putStringUtf8 "</"
   `append` putStringUtf8 name
   `append` putCharUtf8 '>'

toHtmlBuilder (ShortTag name attributes) =
            putCharUtf8 '<'
   `append` putStringUtf8 name
   `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
   `append` putStringUtf8 " />"

toHtmlBuilder (DanglingTag name attributes) =
            putCharUtf8 '<'
   `append` putStringUtf8 name
   `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
   `append` putCharUtf8 '>'

toHtmlBuilder (Content (Style definitions)) =
   foldr (append . toCssBuilder) empty definitions

toHtmlBuilder (Content (Plain content)) =
   putStringUtf8 content

toHtmlBuilder (Content BreakingSpace) =
   putCharUtf8 ' '

toHtmlBuilder _ =
   empty

toHtmlChildrenBuilder :: [Html] -> Builder
toHtmlChildrenBuilder (a@(Content _) : b@(Content BreakingSpace) : c@(Content _) : xs) =
            toHtmlBuilder a
   `append` toHtmlBuilder b
   `append` toHtmlBuilder c
   `append` toHtmlChildrenBuilder xs

toHtmlChildrenBuilder (a:Content BreakingSpace:b:xs) =
   toHtmlChildrenBuilder (a:b:xs)

toHtmlChildrenBuilder (a@(Content BreakingSpace) : b@(Content _) : xs) =
            toHtmlBuilder a
   `append` toHtmlBuilder b
   `append` toHtmlChildrenBuilder xs

toHtmlChildrenBuilder (a@(Content _) : b@(Content BreakingSpace) : xs) =
            toHtmlBuilder a
   `append` toHtmlBuilder b
   `append` toHtmlChildrenBuilder xs

toHtmlChildrenBuilder (Content BreakingSpace : b : xs) =
            toHtmlBuilder b
   `append` toHtmlChildrenBuilder xs

toHtmlChildrenBuilder (a : Content BreakingSpace : xs) =
            toHtmlBuilder a
   `append` toHtmlChildrenBuilder xs

toHtmlChildrenBuilder (a:b:xs) =
            toHtmlBuilder a
   `append` toHtmlBuilder b
   `append` toHtmlChildrenBuilder xs

toHtmlChildrenBuilder [Content BreakingSpace] =
   empty

toHtmlChildrenBuilder [a] =
   toHtmlBuilder a

toHtmlChildrenBuilder [] =
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

toValueBuilder (StyleValue definition) =
   toCssPropertyBuilder definition

toValueBuilder _ =
   empty

toCssBuilder :: Css.Css -> Builder
toCssBuilder (Css.Block selectors properties) =
            foldr (append . putStringUtf8) empty selectors
   `append` putCharUtf8 '{'
   `append` foldr (append . toCssPropertyBuilder) empty properties
   `append` putCharUtf8 '}'

toCssBuilder (Css.Import url) =
            putStringUtf8 "@import '"
   `append` putStringUtf8 url
   `append` putStringUtf8 "';"

toCssBuilder (Css.MediaBlock selectors definitions) =
            putStringUtf8 "@media "
   `append` foldr (append . putStringUtf8) empty selectors
   `append` putCharUtf8 '{'
   `append` foldr (append . toCssBuilder) empty definitions
   `append` putCharUtf8 '}'

toCssPropertyBuilder :: Css.CssProperty -> Builder
toCssPropertyBuilder (name, values) =
            putStringUtf8 name
   `append` putCharUtf8 ':'
   `append` foldr (append . toCssValueBuilder) empty values
   `append` putCharUtf8 ';'

toCssValueBuilder :: Css.CssPropertyValue -> Builder
toCssValueBuilder (Css.Value content) =
   putStringUtf8 content

toCssValueBuilder _ =
   empty

appendWithSpace :: Builder -> Builder -> Builder
appendWithSpace lhs rhs =
   putCharUtf8 ' ' `append` lhs `append` rhs
