module OUI.Showcase.ColorPicker exposing (..)

import ColorPicker
import Element


view color colorPicker =
    ColorPicker.view color colorPicker
        |> Element.html
