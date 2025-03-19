module Views.Upload (homePage) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Views.Structure

-- Home page
homePage :: Html
homePage = mapPastebinLayout (return ()) $ do
    heroSection

-- Hero section with upload
heroSection :: Html
heroSection = section ! class_ "py-12 h-screen  bg-gradient-to-r from-indigo-500 to-purple-600 text-white" $ do
    H.div ! class_ "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" $ do
        H.div ! class_ "text-center" $ do
            h1 ! class_ "text-4xl font-extrabold tracking-tight sm:text-5xl md:text-6xl" $
                "Share Your Maps With The World"
            p ! class_ "mt-6 max-w-2xl mx-auto text-xl" $
                "Upload GeoJSON files and get a permanent link to share with others"

        H.div ! class_ "mt-10 max-w-xl mx-auto bg-white rounded-lg shadow-md p-6" $ do
            H.form ! action "/upload" ! method "post" ! enctype "multipart/form-data" ! class_ "space-y-4" $ do
                H.div $ do
                    H.div ! class_ "flex items-center justify-center w-full" $ do
                        H.label ! for "dropzone-file" ! class_ "flex flex-col items-center justify-center w-full h-32 border-2 border-gray-300 border-dashed rounded-lg cursor-pointer bg-gray-50 hover:bg-gray-100" $ do
                            H.div ! class_ "flex flex-col items-center justify-center pt-5 pb-6" $ do
                                p ! class_ "mb-2 text-sm text-gray-500" $ do
                                    H.span ! class_ "font-semibold" $ "Click to upload"
                                    H.span $ " or drag and drop"
                                p ! class_ "text-xs text-gray-500" $ "GeoJSON files only"
                            input ! A.id "dropzone-file" ! autocomplete "off" ! type_ "file" ! name "geoJson" ! class_ "hidden" ! accept ".geojson,application/geo+json"
                button ! type_ "submit" ! class_ "w-full bg-indigo-600 hover:bg-indigo-700 text-white font-medium py-2 px-4 rounded-md" $
                    "Upload Map"
