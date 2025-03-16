module Main where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Views.Upload
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        html . renderHtml $ homePage
