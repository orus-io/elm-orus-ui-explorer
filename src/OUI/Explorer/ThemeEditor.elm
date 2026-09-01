module OUI.Explorer.ThemeEditor exposing (..)

import Element exposing (Element)
import OUI.Explorer as Explorer
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.Slider as Slider
import OUI.Text as Text


slider :
    Theme themeExt
    -> (Float -> Explorer.BookMsg themeExt msg)
    -> String
    -> ( Float, Float )
    -> Float
    -> Element (Explorer.BookMsg themeExt msg)
slider theme toMsg title ( min, max ) value =
    Element.row [ Element.spacing 30, Element.width Element.fill ]
        [ Text.titleSmall title
            |> Material.text theme
            |> Element.el [ Element.width (Element.px 100) ]
        , Slider.new value
            |> Slider.withStep 1
            |> Slider.withMinMax min max
            |> Slider.onChange toMsg
            |> Material.slider theme
                [ Element.centerY
                , Element.width <| Element.px 250
                ]
            |> Element.el [ Element.width <| Element.px 250 ]
        , String.fromFloat value
            |> Text.bodyLarge
            |> Material.text theme
        ]
