module Views.Upload where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- Main page layout
mapPastebinLayout :: Html -> Html -> Html
mapPastebinLayout headerContent bodyContent = docTypeHtml $ do
    H.head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        H.title "GeoShare - Share Your Maps Easily"
        link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"
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
                H.span ! class_ "text-xl font-bold text-indigo-600" $ "GeoShare"
            H.div ! class_ "flex items-center" $ do
                button ! class_ "bg-indigo-600 hover:bg-indigo-700 text-white font-medium py-2 px-4 rounded-md" $ "Upload New Map"

-- Footer
mapFooter :: Html
mapFooter = H.footer ! class_ "bg-white shadow-inner mt-auto py-4" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 text-center text-gray-500 text-sm" $ do
        p $ "© 2025 GeoShare - Share your maps with the world"
        p ! class_ "mt-2" $ do
            a ! href "/terms" ! class_ "hover:text-indigo-600" $ "Terms"
            H.span ! class_ "mx-2" $ "|"
            a ! href "/privacy" ! class_ "hover:text-indigo-600" $ "Privacy"
            H.span ! class_ "mx-2" $ "|"
            a ! href "/contact" ! class_ "hover:text-indigo-600" $ "Contact"

-- Home page
homePage :: Html
homePage = mapPastebinLayout (return ()) $ do
    heroSection

-- Hero section with upload
heroSection :: Html
heroSection = section ! class_ "py-12 bg-gradient-to-r from-indigo-500 to-purple-600 text-white" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" $ do
        H.div ! class_ "text-center" $ do
            h1 ! class_ "text-4xl font-extrabold tracking-tight sm:text-5xl md:text-6xl" $
                "Share Your Maps With The World"
            p ! class_ "mt-6 max-w-2xl mx-auto text-xl" $
                "Upload GeoJSON files and get a permanent link to share with others"

        H.div ! class_ "mt-10 max-w-xl mx-auto bg-white rounded-lg shadow-md p-6" $ do
            H.form ! action "/upload" ! method "post" ! enctype "multipart/form-data" ! class_ "space-y-4" $ do
                H.div $ do
                    H.label ! for "map-name" ! class_ "block text-gray-700 font-medium mb-1" $ "Map Name"
                    input
                        ! type_ "text"
                        ! A.id "map-name"
                        ! name "mapName"
                        ! class_ "w-full px-3 py-2 text-black border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500"
                        ! placeholder "Enter a name for your map"

                H.div $ do
                    H.label ! class_ "block text-gray-700 font-medium mb-1" $ "GeoJSON File"
                    H.div ! class_ "flex items-center justify-center w-full" $ do
                        H.label ! for "dropzone-file" ! class_ "flex flex-col items-center justify-center w-full h-32 border-2 border-gray-300 border-dashed rounded-lg cursor-pointer bg-gray-50 hover:bg-gray-100" $ do
                            H.div ! class_ "flex flex-col items-center justify-center pt-5 pb-6" $ do
                                p ! class_ "mb-2 text-sm text-gray-500" $ do
                                    H.span ! class_ "font-semibold" $ "Click to upload"
                                    H.span $ " or drag and drop"
                                p ! class_ "text-xs text-gray-500" $ "GeoJSON files only"
                            input ! A.id "dropzone-file" ! type_ "file" ! name "geoJson" ! class_ "hidden" ! accept ".geojson,application/geo+json"

                H.div $ do
                    H.label ! for "description" ! class_ "block text-black text-gray-700 font-medium mb-1" $ "Description (Optional)"
                    textarea ! A.id "description" ! name "description" ! class_ "w-full px-3 py-2 border border-gray-300 rounded-md text-black focus:outline-none focus:ring-2 focus:ring-indigo-500" ! A.rows "3" ! placeholder "Add a description for your map" $ ""
                button ! type_ "submit" ! class_ "w-full bg-indigo-600 hover:bg-indigo-700 text-white font-medium py-2 px-4 rounded-md" $
                    "Upload Map"

-- Features section
featuresSection :: Html
featuresSection = section ! class_ "py-12 bg-white" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" $ do
        H.div ! class_ "text-center" $ do
            h2 ! class_ "text-3xl font-extrabold text-gray-900" $ "Why Use GeoShare?"

        H.div ! class_ "mt-10" $ do
            H.div ! class_ "grid grid-cols-1 gap-8 md:grid-cols-3" $ do
                featureCard "Instant Sharing" "Share your geographic data with colleagues or the public with a simple URL"
                featureCard "Interactive Maps" "Your maps are interactive, allowing users to zoom, pan, and explore your data"
                featureCard "No Account Required" "Quick and easy sharing without needing to create an account"

-- Feature card helper
featureCard :: Html -> Html -> Html
featureCard title description = H.div ! class_ "bg-gray-50 p-6 rounded-lg shadow-sm" $ do
    H.div ! class_ "h-12 w-12 mx-auto rounded-md bg-indigo-500 flex items-center justify-center" $ do
        h3 ! class_ "mt-5 text-lg font-medium text-gray-900 text-center" $ title
        p ! class_ "mt-2 text-gray-500 text-center" $ description

-- Recent maps section
recentMapsSection :: Html
recentMapsSection = section ! class_ "py-12 bg-gray-50" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" $ do
        H.div ! class_ "flex justify-between items-center" $ do
            h2 ! class_ "text-3xl font-extrabold text-gray-900" $ "Recent Maps"
            a ! href "/browse" ! class_ "text-indigo-600 hover:text-indigo-700 font-medium" $ "View All →"

        H.div ! class_ "mt-6 grid grid-cols-1 gap-6 sm:grid-cols-2 lg:grid-cols-3" $ do
            mapCard "San Francisco Districts" "john.doe" "3 hours ago" "/map/abc123"
            mapCard "London Bike Lanes" "cycling_enthusiast" "1 day ago" "/map/def456"
            mapCard "Tokyo Train Network" "maps_lover" "2 days ago" "/map/ghi789"

-- Map card helper
mapCard :: Html -> Html -> Html -> AttributeValue -> Html
mapCard title author time href = a ! A.href href ! class_ "block" $ do
    H.div ! class_ "bg-white rounded-lg shadow-md overflow-hidden" $ do
        H.div ! class_ "h-48 bg-gray-200 map-thumbnail" ! dataAttribute "map" "thumbnail" $ ""
        H.div ! class_ "p-4" $ do
            h3 ! class_ "text-lg font-medium text-gray-900" $ title
            H.div ! class_ "mt-2 flex justify-between text-sm text-gray-500" $ do
                H.span $ do
                    "By "
                    H.span ! class_ "text-indigo-600" $ author
                H.span $ time
