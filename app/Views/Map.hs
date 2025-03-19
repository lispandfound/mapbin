module Views.Map (mapViewPage) where

import Data.Text as T
import System.FilePath
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Views.Structure

mapViewPage :: Text -> Text -> Html
mapViewPage mapPath mapId = mapPastebinLayout headerContent bodyContent
  where
    headerContent =
        script $
            toHtml $
                T.unlines
                    [ "document.addEventListener('DOMContentLoaded', function() {"
                    , "  const map = L.map('map').setView([51.505, -0.09], 13);"
                    , "  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {"
                    , "    attribution: '© OpenStreetMap contributors'"
                    , "  }).addTo(map);"
                    , "  fetch('/" <> pack (unpack mapPath </> (unpack mapId <> ".geojson")) <> "')"
                    , "    .then(response => response.json())"
                    , "    .then(data => {"
                    , "      L.geoJSON(data, {"
                    , "        style: function(feature) {"
                    , "          return feature.properties.style;"
                    , "        },"
                    , "        onEachFeature: function(feature, layer) {"
                    , "          if (feature.properties && feature.properties.tooltip) {"
                    , "            layer.bindTooltip(feature.properties.tooltip);"
                    , "          }"
                    , "        },"
                    , "        pointToLayer: function(feature, latlng) {"
                    , "          if (feature.properties && feature.properties.style) {"
                    , "            return L.circleMarker(latlng, feature.properties.style);"
                    , "          } else {"
                    , "            return L.marker(latlng);"
                    , "          }"
                    , "        }"
                    , "      }).addTo(map);"
                    , "      const bounds = L.geoJSON(data).getBounds();"
                    , "      map.fitBounds(bounds);"
                    , "    });"
                    , "});"
                    ]
    bodyContent = do
        H.div ! class_ "w-screen mx-auto px-4 sm:px-6 lg:px-8 py-8" $ do
            H.div ! class_ "bg-white shadow w-full rounded-lg overflow-hidden" $ do
                H.div ! class_ "border-b border-gray-200 px-6 py-4 flex flex-col justify-between items-center" $ do
                    H.div ! A.id "map" ! class_ "map-container w-full" $ ""
