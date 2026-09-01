module OUI.Explorer.ThemeEditor exposing (slider, textSize, textType)

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


textSize :
    Theme themeExt
    -> (Text.Size -> Explorer.BookMsg themeExt msg)
    -> String
    -> Text.Size
    -> Element (Explorer.BookMsg themeExt msg)
textSize theme toMsg title value =
    let
        sizeToFloat : Text.Size -> Float
        sizeToFloat size =
            case size of
                Text.Small ->
                    0

                Text.Medium ->
                    1

                Text.Large ->
                    2

        floatToSize : Float -> Text.Size
        floatToSize float =
            if float <= 0.5 then
                Text.Small

            else if float <= 1.5 then
                Text.Medium

            else
                Text.Large

        sizeToString : Text.Size -> String
        sizeToString size =
            case size of
                Text.Small ->
                    "Small"

                Text.Medium ->
                    "Medium"

                Text.Large ->
                    "Large"
    in
    Element.row [ Element.spacing 30, Element.width Element.fill ]
        [ Text.titleSmall title
            |> Material.text theme
            |> Element.el [ Element.width (Element.px 100) ]
        , Slider.new (sizeToFloat value)
            |> Slider.withDiscreteStep 1
            |> Slider.withMinMax 0 2
            |> Slider.onChange (floatToSize >> toMsg)
            |> Material.slider theme
                [ Element.centerY
                , Element.width <| Element.px 250
                ]
            |> Element.el [ Element.width <| Element.px 250 ]
        , sizeToString value
            |> Text.bodyLarge
            |> Material.text theme
        ]


textType :
    Theme themeExt
    -> (Text.Type -> Explorer.BookMsg themeExt msg)
    -> String
    -> Text.Type
    -> Element (Explorer.BookMsg themeExt msg)
textType theme toMsg title value =
    let
        typeToFloat : Text.Type -> Float
        typeToFloat type_ =
            case type_ of
                Text.Display ->
                    0

                Text.Headline ->
                    1

                Text.Title ->
                    2

                Text.Label ->
                    3

                Text.Body ->
                    4

        floatToType : Float -> Text.Type
        floatToType float =
            if float <= 0.5 then
                Text.Display

            else if float <= 1.5 then
                Text.Headline

            else if float <= 2.5 then
                Text.Title

            else if float <= 3.5 then
                Text.Label

            else
                Text.Body

        typeToString : Text.Type -> String
        typeToString type_ =
            case type_ of
                Text.Display ->
                    "Display"

                Text.Headline ->
                    "Headline"

                Text.Title ->
                    "Title"

                Text.Label ->
                    "Label"

                Text.Body ->
                    "Body"
    in
    Element.row [ Element.spacing 30, Element.width Element.fill ]
        [ Text.titleSmall title
            |> Material.text theme
            |> Element.el [ Element.width (Element.px 100) ]
        , Slider.new (typeToFloat value)
            |> Slider.withDiscreteStep 1
            |> Slider.withMinMax 0 4
            |> Slider.onChange (floatToType >> toMsg)
            |> Material.slider theme
                [ Element.centerY
                , Element.width <| Element.px 250
                ]
            |> Element.el [ Element.width <| Element.px 250 ]
        , typeToString value
            |> Text.bodyLarge
            |> Material.text theme
        ]
