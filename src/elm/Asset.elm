module Asset exposing (Image, srcFromString, error, src)

{-| Assets, such as images, videos, and audio. (We only have images for now.)

We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!

-}

import Html exposing (Attribute)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES


error : Image
error =
    image "error.jpg"


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)

concatAsset : String -> String
concatAsset name =
    "/assets/images/" ++ name

-- USING IMAGES


src : Image -> Attribute msg
src (Image url) =
    Attr.src url

srcFromString : String -> Attribute msg
srcFromString myImg =
    Attr.src (concatAsset myImg)
