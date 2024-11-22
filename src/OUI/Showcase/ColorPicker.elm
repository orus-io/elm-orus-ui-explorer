module OUI.Showcase.ColorPicker exposing (..)

import Color exposing (Color)
import ColorPicker
import Element exposing (Element)
import OUI.Material
import OUI.Material.Color.Json
import OUI.Material.Theme
import OUI.TextField


type alias State =
    { colorPicker : ColorPicker.State
    , text : String
    , textFocused : Bool
    }


type Msg
    = ColorPickerMsg ColorPicker.Msg
    | TextChange String
    | TextFocus Bool


empty =
    { colorPicker = ColorPicker.empty
    , text = ""
    , textFocused = False
    }


update msg color model =
    case msg of
        ColorPickerMsg subMsg ->
            let
                ( p, nextColor ) =
                    ColorPicker.update subMsg color model.colorPicker
            in
            ( { model | colorPicker = p }, nextColor )

        TextChange value ->
            ( { model | text = value }
            , value
                |> OUI.Material.Color.Json.colorFromHex
                |> Maybe.withDefault color
                |> Just
            )

        TextFocus value ->
            ( { model
                | textFocused = value
                , text =
                    if value then
                        color |> OUI.Material.Color.Json.toHexWithAlpha

                    else
                        model.text
              }
            , if value then
                Just color

              else
                model.text |> OUI.Material.Color.Json.colorFromHex
            )


view : OUI.Material.Theme.Theme themeExt -> Color -> State -> Element Msg
view theme color colorPicker =
    Element.column [ Element.spacing 20 ]
        [ ColorPicker.view color colorPicker.colorPicker
            |> Element.html
            |> Element.el [ Element.centerX ]
            |> Element.map ColorPickerMsg
        , OUI.TextField.new "Hex"
            TextChange
            (if colorPicker.textFocused then
                colorPicker.text

             else
                color
                    |> OUI.Material.Color.Json.toHexWithAlpha
            )
            |> OUI.TextField.withType OUI.TextField.Outlined
            |> OUI.TextField.onFocusBlur (TextFocus True) (TextFocus False)
            |> OUI.TextField.withFocused colorPicker.textFocused
            |> OUI.Material.textField theme []
        ]
