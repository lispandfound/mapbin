module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Geospatial
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Views.Map
import Views.Upload
import Web.Scotty

saveGeoJson :: FilePath -> GeoFeatureCollection Float -> IO ()
saveGeoJson path = B.writeFile path . encode

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        html . renderHtml $ homePage
    post "/upload" $ do
        geoJson <- jsonData
        let path = "maps/test.geojson"
        liftIO $ saveGeoJson path geoJson
    get "/map/:id" $ do
        html . renderHtml $ mapViewPage "test" "My Map" "Anonymous" False
