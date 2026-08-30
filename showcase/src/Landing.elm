module Landing exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import OUI
import OUI.Button as Button
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Icon as Icon
import OUI.Material as Material
import OUI.Material.Color
import OUI.Material.Theme as Theme exposing (Theme)
import OUI.Text as Text


view : Explorer.Shared themeExt -> Element (Explorer.BookMsg themeExt ())
view { theme } =
    let
        colorscheme : OUI.Material.Color.Scheme
        colorscheme =
            Theme.colorscheme theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 40, Element.width Element.fill ]
        [ hero theme colorscheme
        , features theme colorscheme
        , exploreStrip theme
        , divider
        , footer theme
        ]


hero : Theme themeExt -> OUI.Material.Color.Scheme -> Element (Explorer.BookMsg themeExt ())
hero theme colorscheme =
    Element.column
        [ Element.width Element.fill
        , Element.padding 40
        , Element.spacing 24
        , Background.color <| OUI.Material.Color.toElementColor colorscheme.primaryContainer
        , Border.rounded 20
        , Border.shadow
            { offset = ( 0, 4 )
            , size = 0
            , blur = 16
            , color = OUI.Material.Color.toElementColor colorscheme.shadow
            }
        ]
        [ Text.headlineLarge "Build beautiful Elm UIs with Material Design 3"
            |> Text.onColor OUI.PrimaryContainer
            |> Material.text theme
        , Element.paragraph [ Element.width Element.fill ]
            [ Text.bodyLarge
                "Elm Orus UI is a toolkit for building user interfaces with an elegant API, fully customizable themes, and a rich component set — all in pure Elm."
                |> Text.onColor OUI.PrimaryContainer
                |> Material.text theme
            ]
        , Element.row [ Element.spacing 16, Element.width Element.fill, Element.paddingXY 0 8 ]
            [ Button.new "Get Started"
                |> Button.withIcon rocketLaunchIcon
                |> Button.link "#/Get_Started"
                |> Button.filledButton
                |> Material.button theme []
            , Button.new "Documentation"
                |> Button.withIcon menuBookIcon
                |> Button.newTabLink "https://package.elm-lang.org/packages/orus-io/elm-orus-ui/latest/"
                |> Button.outlinedButton
                |> Material.button theme []
            , Button.new "GitHub"
                |> Button.withIcon openInNewIcon
                |> Button.newTabLink "https://github.com/orus-io/elm-orus-ui"
                |> Button.textButton
                |> Material.button theme []
            ]
        ]


features : Theme themeExt -> OUI.Material.Color.Scheme -> Element (Explorer.BookMsg themeExt ())
features theme colorscheme =
    let
        card : String -> String -> Icon.Icon -> Element (Explorer.BookMsg themeExt ())
        card title description icon =
            Element.column
                [ Element.spacing 12
                , Element.padding 24
                , Element.width <| Element.px 300
                , Background.color <| OUI.Material.Color.toElementColor colorscheme.surfaceContainerHigh
                , Border.rounded 16
                , Border.width 1
                , Border.color <| OUI.Material.Color.toElementColor colorscheme.outlineVariant
                ]
                [ Material.icon theme
                    [ Element.centerX ]
                    (Icon.withSize 40 icon)
                , Text.titleMedium title
                    |> Material.text theme
                    |> Element.el [ Element.centerX ]
                , Text.bodySmall description
                    |> Material.text theme
                    |> List.singleton
                    |> Element.paragraph [ Element.centerX, Font.center ]
                ]
    in
    Element.wrappedRow [ Element.spacing 20, Element.width Element.fill ]
        [ card "Material Design 3" "Based on the latest Material Design 3 guidelines, with a full color system and typescale." paletteIcon
        , card "Fully Themable" "Every component reads from a central Theme record. Swap colors, shapes, and sizes in one place." tuneIcon
        , card "Type-Safe API" "Leverage Elm's type system — phantom types prevent invalid component states at compile time." verifiedIcon
        , card "Rich Components" "Buttons, text fields, sliders, tabs, menus, dialogs, navigation drawers, and more." widgetsIcon
        , card "Icon Ready" "Works with icidasset/elm-material-icons out of the box, or bring your own SVG icons." extensionIcon
        , card "Light & Dark Mode" "Generate light and dark color schemes from a few key colors, switchable at runtime." darkModeIcon
        ]
        |> Element.el [ Element.paddingXY 0 20 ]


exploreStrip : Theme themeExt -> Element (Explorer.BookMsg themeExt ())
exploreStrip theme =
    Element.column
        [ Element.spacing 16
        , Element.padding 20
        , Element.width Element.fill
        ]
        [ Text.titleLarge "Explore the components" |> Material.text theme
        , Element.wrappedRow [ Element.spacing 12, Element.width Element.fill ]
            [ Button.new "Buttons" |> Button.link "#/Basics/Buttons" |> Button.filledButton |> Material.button theme []
            , Button.new "Text Fields" |> Button.link "#/Basics/Text_Fields" |> Button.outlinedButton |> Material.button theme []
            , Button.new "Tabs" |> Button.link "#/Basics/Tabs" |> Button.outlinedButton |> Material.button theme []
            , Button.new "Colors" |> Button.link "#/Styles/Colors" |> Button.outlinedButton |> Material.button theme []
            , Button.new "Navigation" |> Button.link "#/Complex/Navigation" |> Button.outlinedButton |> Material.button theme []
            ]
        ]


footer : Theme themeExt -> Element (Explorer.BookMsg themeExt ())
footer theme =
    Element.column [ Element.spacing 12, Element.width Element.fill ]
        [ Element.row [ Element.spacing 16, Element.width Element.fill ]
            [ Button.new "Elm Packages" |> Button.newTabLink "https://package.elm-lang.org/packages/orus-io/elm-orus-ui/latest/" |> Button.textButton |> Material.button theme []
            , Button.new "GitHub" |> Button.newTabLink "https://github.com/orus-io/elm-orus-ui" |> Button.textButton |> Material.button theme []
            , Button.new "Material Design 3" |> Button.newTabLink "https://m3.material.io/" |> Button.textButton |> Material.button theme []
            ]
        , Text.bodySmall "Elm Orus UI is open source and free to use."
            |> Material.text theme
        ]



-- Icon definitions


rocketLaunchIcon : Icon.Icon
rocketLaunchIcon =
    Icon.elmMaterialIcons Color Material.Icons.rocket_launch
        |> Icon.withSize 20


menuBookIcon : Icon.Icon
menuBookIcon =
    Icon.elmMaterialIcons Color Material.Icons.menu_book
        |> Icon.withSize 20


openInNewIcon : Icon.Icon
openInNewIcon =
    Icon.elmMaterialIcons Color Material.Icons.open_in_new
        |> Icon.withSize 20


paletteIcon : Icon.Icon
paletteIcon =
    Icon.elmMaterialIcons Color Material.Icons.palette
        |> Icon.withSize 40


tuneIcon : Icon.Icon
tuneIcon =
    Icon.elmMaterialIcons Color Material.Icons.tune
        |> Icon.withSize 40


verifiedIcon : Icon.Icon
verifiedIcon =
    Icon.elmMaterialIcons Color Material.Icons.verified
        |> Icon.withSize 40


widgetsIcon : Icon.Icon
widgetsIcon =
    Icon.elmMaterialIcons Color Material.Icons.widgets
        |> Icon.withSize 40


extensionIcon : Icon.Icon
extensionIcon =
    Icon.elmMaterialIcons Color Material.Icons.extension
        |> Icon.withSize 40


darkModeIcon : Icon.Icon
darkModeIcon =
    Icon.elmMaterialIcons Color Material.Icons.dark_mode
        |> Icon.withSize 40
