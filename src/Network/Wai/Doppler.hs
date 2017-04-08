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
toHtmlBuilder (Html tag) =
   toHtmlTagBuilder tag

toHtmlBuilder (HtmlSiblings tags) =
   foldr (append . toHtmlBuilder) empty tags

toHtmlTagBuilder :: Tag HtmlAttribute HtmlContent -> Builder
toHtmlTagBuilder (FullTag name attributes children) =
            putCharUtf8 '<'
   `append` putStringUtf8 name
   `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
   `append` putCharUtf8 '>'
   `append` toHtmlChildrenTagBuilder children
   `append` putStringUtf8 "</"
   `append` putStringUtf8 name
   `append` putCharUtf8 '>'

toHtmlTagBuilder (ShortTag name attributes) =
            putCharUtf8 '<'
   `append` putStringUtf8 name
   `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
   `append` putStringUtf8 " />"

toHtmlTagBuilder (DanglingTag name attributes) =
            putCharUtf8 '<'
   `append` putStringUtf8 name
   `append` foldr (appendWithSpace . toAttributeBuilder) empty attributes
   `append` putCharUtf8 '>'

toHtmlTagBuilder (Content (Style definitions)) =
   foldr (append . toCssBuilder) empty definitions

toHtmlTagBuilder (Content (Plain content)) =
   putStringUtf8 content

toHtmlTagBuilder (Content BreakingSpace) =
   putCharUtf8 ' '

toHtmlTagBuilder _ =
   empty

toHtmlChildrenTagBuilder :: [Tag HtmlAttribute HtmlContent] -> Builder
toHtmlChildrenTagBuilder (a@(Content _) : b@(Content BreakingSpace) : c@(Content _) : xs) =
            toHtmlTagBuilder a
   `append` toHtmlTagBuilder b
   `append` toHtmlTagBuilder c
   `append` toHtmlChildrenTagBuilder xs

toHtmlChildrenTagBuilder (a:Content BreakingSpace:b:xs) =
   toHtmlChildrenTagBuilder (a:b:xs)

toHtmlChildrenTagBuilder (a@(Content BreakingSpace) : b@(Content _) : xs) =
            toHtmlTagBuilder a
   `append` toHtmlTagBuilder b
   `append` toHtmlChildrenTagBuilder xs

toHtmlChildrenTagBuilder (a@(Content _) : b@(Content BreakingSpace) : xs) =
            toHtmlTagBuilder a
   `append` toHtmlTagBuilder b
   `append` toHtmlChildrenTagBuilder xs

toHtmlChildrenTagBuilder (Content BreakingSpace : b : xs) =
            toHtmlTagBuilder b
   `append` toHtmlChildrenTagBuilder xs

toHtmlChildrenTagBuilder (a : Content BreakingSpace : xs) =
            toHtmlTagBuilder a
   `append` toHtmlChildrenTagBuilder xs

toHtmlChildrenTagBuilder (a:b:xs) =
            toHtmlTagBuilder a
   `append` toHtmlTagBuilder b
   `append` toHtmlChildrenTagBuilder xs

toHtmlChildrenTagBuilder [Content BreakingSpace] =
   empty

toHtmlChildrenTagBuilder [a] =
   toHtmlTagBuilder a

toHtmlChildrenTagBuilder [] =
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
toCssPropertyBuilder (Css.CssProperty (name, values)) =
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
