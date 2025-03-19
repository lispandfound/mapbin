module Views.Structure (mapPastebinLayout) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- Main page layout
mapPastebinLayout :: Html -> Html -> Html
mapPastebinLayout headerContent bodyContent = docTypeHtml $ do
    H.head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        H.title "Mapbin - Share Your Maps Easily"
        link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"
        link ! rel "stylesheet" ! href "https://unpkg.com/leaflet@1.9.3/dist/leaflet.css"
        script ! src "https://unpkg.com/leaflet@1.9.3/dist/leaflet.js" $ ""
        H.style $ "body {min-height: 100vh;} .map-container {height: 70vh;}"
        headerContent
    H.body ! class_ "bg-gray-50 flex flex-col min-h-screen" $ do
        navbar
        bodyContent
        mapFooter

-- Navigation bar
navbar :: Html
navbar = nav ! class_ "bg-white shadow-md" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" $ do
        H.div ! class_ "flex justify-between items-center h-16" $ do
            H.div ! class_ "flex items-center" $ do
                H.span ! class_ "text-xl font-bold text-indigo-600" $ H.a ! A.href "/" $ "Mapbin"

-- Footer
mapFooter :: Html
mapFooter = H.footer ! class_ "bg-white shadow-inner mt-auto py-4" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 text-center text-gray-500 text-sm" $ do
        p $ "© 2025 Mapbin - Share your maps with the world"
