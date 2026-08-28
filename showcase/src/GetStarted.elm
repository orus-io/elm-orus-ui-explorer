module GetStarted exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
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
import SyntaxHighlight


view : Explorer.Shared themeExt -> Element (Explorer.BookMsg ())
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
        , steps theme colorscheme
        , exploreStrip theme
        , divider
        , footer theme
        ]


hero : Theme themeExt -> OUI.Material.Color.Scheme -> Element (Explorer.BookMsg ())
hero theme colorscheme =
    Element.column
        [ Element.width Element.fill
        , Element.padding 40
        , Element.spacing 24
        , Background.color <| OUI.Material.Color.toElementColor colorscheme.primaryContainer
        , Border.rounded 20
        ]
        [ Text.headlineLarge "Get Started with Elm Orus UI"
            |> Text.onColor OUI.PrimaryContainer
            |> Material.text theme
        , Element.paragraph [ Element.width Element.fill ]
            [ Text.bodyLarge
                "Install the package, set up a theme, and start building Material Design 3 interfaces in pure Elm."
                |> Text.onColor OUI.PrimaryContainer
                |> Material.text theme
            ]
        , Element.row [ Element.spacing 16, Element.width Element.fill ]
            [ Button.new "Elm Packages"
                |> Button.withIcon menuBookIcon
                |> Button.newTabLink "https://package.elm-lang.org/packages/orus-io/elm-orus-ui/latest/"
                |> Button.filledButton
                |> Material.button theme []
            , Button.new "GitHub"
                |> Button.withIcon openInNewIcon
                |> Button.newTabLink "https://github.com/orus-io/elm-orus-ui"
                |> Button.outlinedButton
                |> Material.button theme []
            ]
        ]


steps : Theme themeExt -> OUI.Material.Color.Scheme -> Element (Explorer.BookMsg ())
steps theme colorscheme =
    Element.column [ Element.spacing 24, Element.width Element.fill ]
        [ step theme
            colorscheme
            "1. Install the package"
            "Add Elm Orus UI to your project with the Elm CLI."
            "bash"
            "elm install orus-io/elm-orus-ui"
        , step theme
            colorscheme
            "2. Define a theme"
            "The theme holds the typescale, colorscheme, and component styles. Start from the default and customize as you go."
            "elm"
            installSample
        , step theme
            colorscheme
            "3. Render components"
            "Each Material renderer takes the theme as its first argument. Compose text, buttons, and icons in a normal Elm view."
            "elm"
            viewSample
        ]


step :
    Theme themeExt
    -> OUI.Material.Color.Scheme
    -> String
    -> String
    -> String
    -> String
    -> Element (Explorer.BookMsg ())
step theme colorscheme title description label code =
    Element.column
        [ Element.spacing 16
        , Element.width Element.fill
        , Element.padding 24
        , Background.color <| OUI.Material.Color.toElementColor colorscheme.surfaceContainerLow
        , Border.rounded 16
        , Border.width 1
        , Border.color <| OUI.Material.Color.toElementColor colorscheme.outlineVariant
        ]
        [ Text.titleLarge title |> Material.text theme
        , Element.paragraph [ Element.width Element.fill ]
            [ Text.bodyMedium description |> Material.text theme ]
        , codeBlock colorscheme label code
        ]


codeBlock : OUI.Material.Color.Scheme -> String -> String -> Element msg
codeBlock colorscheme label code =
    Element.column
        [ Element.width Element.fill
        , Border.rounded 12
        , Border.width 1
        , Border.color <| OUI.Material.Color.toElementColor colorscheme.outlineVariant
        ]
        [ Element.el
            [ Element.width Element.fill
            , Element.paddingXY 16 10
            , Background.color <| OUI.Material.Color.toElementColor colorscheme.surfaceContainerHigh
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <| OUI.Material.Color.toElementColor colorscheme.outlineVariant
            ]
            (Element.row [ Element.width Element.fill ]
                [ Element.row [ Element.spacing 6 ]
                    [ dot <| OUI.Material.Color.toElementColor colorscheme.error
                    , dot <| OUI.Material.Color.toElementColor colorscheme.tertiary
                    , dot <| OUI.Material.Color.toElementColor colorscheme.secondary
                    ]
                , Element.el
                    [ Element.alignRight
                    , Font.family [ Font.monospace ]
                    , Font.size 12
                    , Font.color <| OUI.Material.Color.toElementColor colorscheme.onSurfaceVariant
                    ]
                    (Element.text label)
                ]
            )
        , Element.el
            [ Font.family [ Font.monospace ]
            , Font.color <| OUI.Material.Color.toElementColor colorscheme.onSurface
            , Font.size 13
            , Element.width Element.fill
            ]
            (code
                |> (if label == "elm" then
                        SyntaxHighlight.elm

                    else
                        SyntaxHighlight.noLang
                   )
                |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                |> Result.withDefault
                    (Html.pre [] [ Html.code [] [ Html.text <| "oh no !" ++ code ] ])
                |> Element.html
            )
        ]


codeLine : String -> Element msg
codeLine line =
    Element.el [ Element.width Element.fill ] <|
        Element.text
            (if line == "" then
                " "

             else
                line
            )


dot : Element.Color -> Element msg
dot color =
    Element.el
        [ Element.width (Element.px 10)
        , Element.height (Element.px 10)
        , Border.rounded 5
        , Background.color color
        ]
        Element.none


exploreStrip : Theme themeExt -> Element (Explorer.BookMsg ())
exploreStrip theme =
    Element.column [ Element.spacing 16, Element.width Element.fill ]
        [ Text.titleLarge "Explore the components" |> Material.text theme
        , Element.wrappedRow [ Element.spacing 12, Element.width Element.fill ]
            [ Button.new "Buttons" |> Button.link "#/Basics/Buttons" |> Button.filledButton |> Material.button theme []
            , Button.new "Text Fields" |> Button.link "#/Basics/Text_Fields" |> Button.outlinedButton |> Material.button theme []
            , Button.new "Tabs" |> Button.link "#/Basics/Tabs" |> Button.outlinedButton |> Material.button theme []
            , Button.new "Colors" |> Button.link "#/Styles/Colors" |> Button.outlinedButton |> Material.button theme []
            , Button.new "Typography" |> Button.link "#/Styles/Typography" |> Button.outlinedButton |> Material.button theme []
            ]
        ]


footer : Theme themeExt -> Element (Explorer.BookMsg ())
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


installSample : String
installSample =
    """import OUI.Material.Theme as Theme exposing (Theme)


theme : Theme ()
theme =
    Theme.defaultTheme"""


viewSample : String
viewSample =
    """import Element exposing (Element)
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import OUI.Button as Button
import OUI.Icon as Icon
import OUI.Material as Material
import OUI.Material.Theme as Theme
import OUI.Text as Text


view : Theme themeExt -> Element msg
view theme =
    Element.column [ Element.spacing 16 ]
        [ Text.headlineMedium "Welcome to Elm Orus UI"
            |> Material.text theme
        , Button.new "Click me"
            |> Button.withIcon (Icon.elmMaterialIcons Color Material.Icons.add)
            |> Button.onClick Clicked
            |> Button.filledButton
            |> Material.button theme []
        ]"""



-- Icon definitions


menuBookIcon : Icon.Icon
menuBookIcon =
    Icon.elmMaterialIcons Color Material.Icons.menu_book
        |> Icon.withSize 20


openInNewIcon : Icon.Icon
openInNewIcon =
    Icon.elmMaterialIcons Color Material.Icons.open_in_new
        |> Icon.withSize 20
