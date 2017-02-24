{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Doppler.HTML.Syntax
import Network.Wai.Doppler
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString          (ByteString)

port :: Int
port = 8080

app :: Application
app request respond =
   respond $ case rawPathInfo request of
      "/" -> rootPage
      path -> notFound path

rootPage :: Response
rootPage = responseHTML
   status200
   [("Content-Type", "text/html")]
   [html|
      <html>
         <head>
            <title>Hello World!</title>
         </head>

         <body style="background-color: lightcoral; font-size: 28px;" class="content">
            <p>This is body</p>
            <p>... and Sparta.</p>
         </body>
      </html>
   |]

notFound :: ByteString -> Response
notFound path = responseHTML
   status404
   [("Content-Type", "text/html")]
   [html|
      <html>
         <head>
            <title>Not found (404)</title>
         </head>

         <body>
            <h1>Not found</h1>

            <p>Sorry but resource ${path} does not exist.</p>
         </body>
      </html>
   |]

main :: IO ()
main = do
   putStrLn $ "Web server is running at localhost:" ++ show port
   run port app
