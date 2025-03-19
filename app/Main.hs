module Main where

import CLI
import Data.Aeson (Object, decode, encode)
import qualified Data.ByteString.Lazy as B
import Data.Geospatial
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (unpack)
import Data.Text.Lazy (fromStrict)
import Data.UUID
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static
import Network.Wai.Parse (FileInfo (..))
import System.FilePath
import System.Random (randomIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Views.Map
import Views.Upload
import Web.Scotty

saveGeoJson :: FilePath -> GeoFeatureCollection Object -> IO ()
saveGeoJson path = B.writeFile path . encode

uuid :: IO UUID
uuid = randomIO

main :: IO ()
main = do
    serverConfig <- cli
    scotty (port serverConfig) $ do
        middleware $ staticPolicy (hasPrefix (unpack $ mapPath serverConfig))
        get "/" $ do
            html . renderHtml $ homePage
        post "/upload" $ do
            geoJson <- listToMaybe . mapMaybe (decode . fileContent . snd) <$> files
            mapUUID <- liftIO uuid
            let path = unpack (mapPath serverConfig) </> (show mapUUID <> ".geojson")
                redirectPath = "/map/" <> (fromStrict . toText) mapUUID
            maybe (status status400 >> redirect "/") ((>> redirect redirectPath) . liftIO . saveGeoJson path) geoJson

        get "/map/:id" $ do
            mapId <- pathParam "id"
            html . renderHtml $ mapViewPage (mapPath serverConfig) mapId
