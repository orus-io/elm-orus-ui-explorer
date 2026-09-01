module OUI.Showcase.Dividers exposing (book, commonDividers)

import Element exposing (Element)
import Element.Border
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Material as Material
import OUI.Material.Color
import OUI.Material.Theme as Theme exposing (Theme)
import OUI.Text as Text


book : Explorer.Book themeExt () ()
book =
    Explorer.book "Dividers"
        |> Explorer.withStaticChapter commonDividers
        |> Explorer.withThemeEditor editorChapter


updateDividerTheme :
    (Theme.DividerTheme -> Theme.DividerTheme)
    -> Theme themeExt
    -> Theme themeExt
updateDividerTheme fn theme =
    theme
        |> Theme.withDivider
            (theme
                |> Theme.divider
                |> fn
            )


updateDividerMsg :
    (data -> Theme.DividerTheme -> Theme.DividerTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateDividerMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateDividerTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> () -> Element (Explorer.BookMsg themeExt msg)
editorChapter { theme } _ =
    let
        dividerTheme : Theme.DividerTheme
        dividerTheme =
            Theme.divider theme

        divider : Element (Explorer.BookMsg themeExt msg)
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Divider" |> Material.text theme
        , ThemeEditor.slider theme
            (updateDividerMsg
                (\value d ->
                    { d | thickness = round value }
                )
            )
            "Thickness"
            ( 0, 20, 1 )
            (toFloat dividerTheme.thickness)
        , divider
        ]


commonDividers : Explorer.Shared themeExt -> Element (Explorer.BookMsg themeExt ())
commonDividers { theme } =
    let
        colorscheme : OUI.Material.Color.Scheme
        colorscheme =
            Theme.colorscheme theme
    in
    Element.column
        [ Element.spacing 16
        , Element.width <| Element.px 400
        , Element.Border.solid
        , Element.Border.rounded 8
        , Element.Border.width 1
        , colorscheme.surfaceContainer
            |> OUI.Material.Color.withShade colorscheme.onSurface
                OUI.Material.Color.hoverStateLayerOpacity
            |> OUI.Material.Color.toElementColor
            |> Element.Border.color
        ]
        [ Text.titleMedium "Full width divider"
            |> Material.text theme
            |> Element.el
                [ Element.paddingEach
                    { top = 16
                    , bottom = 0
                    , left = 16
                    , right = 16
                    }
                ]
        , Divider.new
            |> Material.divider theme []
        , Text.titleMedium "Inset divider"
            |> Material.text theme
            |> Element.el [ Element.paddingXY 16 0 ]
        , Divider.new
            |> Material.divider theme []
            |> Element.el [ Element.paddingXY 16 0, Element.width Element.fill ]
        , Element.none
            |> Element.el [ Element.height <| Element.px 40 ]
        ]
