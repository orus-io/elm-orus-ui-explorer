module OUI.Showcase.Typography exposing (book)

import Element exposing (Element)
import OUI
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme, Typescale, Typography)
import OUI.Text as Text


book : Explorer.Book themeExt () ()
book =
    Explorer.book "Typography"
        |> Explorer.withMarkdownChapter """
The material typescale

Type styles include: display, headline, title, body, and label

Each type comes in three sizes: large, medium, small


"""
        |> Explorer.withStaticChapter
            (\{ theme } ->
                [ Text.bodyLarge "Different text pieces can be assembled with Element.paragraph, "
                , Text.bodySmall "here with smaller text, "
                , Text.bodyLarge " or here in color." |> Text.withColor OUI.Primary
                ]
                    |> List.map (Material.text theme)
                    |> Element.paragraph []
            )
        |> Explorer.withStaticChapter
            (\{ theme } ->
                Element.column [ Element.spacing 20 ]
                    [ Element.row [ Element.spacing 20 ]
                        [ Element.column
                            [ Element.spacing 10 ]
                            [ Text.displayLarge "Display Large" |> Material.text theme
                            , Text.displayMedium "Display Medium" |> Material.text theme
                            , Text.displaySmall "Display Small" |> Material.text theme
                            ]
                        , Element.column
                            [ Element.spacing 10 ]
                            [ Text.headlineLarge "Headline Large" |> Material.text theme
                            , Text.headlineMedium "Headline Medium" |> Material.text theme
                            , Text.headlineSmall "Headline Small" |> Material.text theme
                            ]
                        ]
                    , Element.row [ Element.spacing 20 ]
                        [ Element.column
                            [ Element.spacing 10 ]
                            [ Text.titleLarge "Title Large" |> Material.text theme
                            , Text.titleMedium "Title Medium" |> Material.text theme
                            , Text.titleSmall "Title Small" |> Material.text theme
                            ]
                        , Element.column
                            [ Element.spacing 10 ]
                            [ Text.labelLarge "Label Large" |> Material.text theme
                            , Text.labelMedium "Label Medium" |> Material.text theme
                            , Text.labelSmall "Label Small" |> Material.text theme
                            ]
                        , Element.column
                            [ Element.spacing 10 ]
                            [ Text.bodyLarge "Body Large" |> Material.text theme
                            , Text.bodyMedium "Body Medium" |> Material.text theme
                            , Text.bodySmall "Body Small" |> Material.text theme
                            ]
                        ]
                    ]
            )
        |> Explorer.withThemeEditor editorChapter


updateTypescaleTheme :
    (Typescale -> Typescale)
    -> Theme themeExt
    -> Theme themeExt
updateTypescaleTheme fn theme =
    theme
        |> OUI.Material.Theme.withTypescale
            (theme
                |> OUI.Material.Theme.typescale
                |> fn
            )


updateTypescaleMsg :
    (Float -> Typescale -> Typescale)
    -> Float
    -> Explorer.BookMsg themeExt msg
updateTypescaleMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateTypescaleTheme (fn value))
        |> Explorer.sharedMsg


typographySection :
    Theme themeExt
    -> String
    -> Typography
    -> ((Typography -> Typography) -> Typescale -> Typescale)
    -> List (Element (Explorer.BookMsg themeExt msg))
typographySection theme title value setter =
    let
        divider : Element (Explorer.BookMsg themeExt msg)
        divider =
            Divider.new |> Material.divider theme []
    in
    [ divider
    , Text.titleLarge title |> Material.text theme
    , ThemeEditor.slider theme
        (updateTypescaleMsg
            (\v -> setter (\typo -> { typo | size = round v }))
        )
        "Size"
        ( 0, 100, 1 )
        (toFloat value.size)
    , ThemeEditor.discreteSlider theme
        (updateTypescaleMsg
            (\v -> setter (\typo -> { typo | weight = round v }))
        )
        "Weight"
        ( 100, 900, 100 )
        (toFloat value.weight)
    ]


editorChapter : Explorer.Shared themeExt -> () -> Element (Explorer.BookMsg themeExt msg)
editorChapter { theme } _ =
    let
        typescale : Typescale
        typescale =
            OUI.Material.Theme.typescale theme

        divider : Element (Explorer.BookMsg themeExt msg)
        divider =
            Divider.new |> Material.divider theme []

        group :
            String
            -> { small : Typography, medium : Typography, large : Typography }
            -> ((Typography -> Typography) -> Typescale -> Typescale)
            -> ((Typography -> Typography) -> Typescale -> Typescale)
            -> ((Typography -> Typography) -> Typescale -> Typescale)
            -> List (Element (Explorer.BookMsg themeExt msg))
        group title groupRecord largeSetter mediumSetter smallSetter =
            typographySection theme (title ++ " Large") groupRecord.large largeSetter
                ++ typographySection theme (title ++ " Medium") groupRecord.medium mediumSetter
                ++ typographySection theme (title ++ " Small") groupRecord.small smallSetter
    in
    Element.column [ Element.spacing 30 ]
        ([ divider
         , Text.titleLarge "Typography" |> Material.text theme
         ]
            ++ group "Display"
                typescale.display
                (\fn ts ->
                    let
                        display : { small : Typography, medium : Typography, large : Typography }
                        display =
                            ts.display

                        large : Typography
                        large =
                            display.large
                    in
                    { ts | display = { display | large = fn large } }
                )
                (\fn ts ->
                    let
                        display : { small : Typography, medium : Typography, large : Typography }
                        display =
                            ts.display

                        medium : Typography
                        medium =
                            display.medium
                    in
                    { ts | display = { display | medium = fn medium } }
                )
                (\fn ts ->
                    let
                        display : { small : Typography, medium : Typography, large : Typography }
                        display =
                            ts.display

                        small : Typography
                        small =
                            display.small
                    in
                    { ts | display = { display | small = fn small } }
                )
            ++ group "Headline"
                typescale.headline
                (\fn ts ->
                    let
                        headline : { small : Typography, medium : Typography, large : Typography }
                        headline =
                            ts.headline

                        large : Typography
                        large =
                            headline.large
                    in
                    { ts | headline = { headline | large = fn large } }
                )
                (\fn ts ->
                    let
                        headline : { small : Typography, medium : Typography, large : Typography }
                        headline =
                            ts.headline

                        medium : Typography
                        medium =
                            headline.medium
                    in
                    { ts | headline = { headline | medium = fn medium } }
                )
                (\fn ts ->
                    let
                        headline : { small : Typography, medium : Typography, large : Typography }
                        headline =
                            ts.headline

                        small : Typography
                        small =
                            headline.small
                    in
                    { ts | headline = { headline | small = fn small } }
                )
            ++ group "Title"
                typescale.title
                (\fn ts ->
                    let
                        title : { small : Typography, medium : Typography, large : Typography }
                        title =
                            ts.title

                        large : Typography
                        large =
                            title.large
                    in
                    { ts | title = { title | large = fn large } }
                )
                (\fn ts ->
                    let
                        title : { small : Typography, medium : Typography, large : Typography }
                        title =
                            ts.title

                        medium : Typography
                        medium =
                            title.medium
                    in
                    { ts | title = { title | medium = fn medium } }
                )
                (\fn ts ->
                    let
                        title : { small : Typography, medium : Typography, large : Typography }
                        title =
                            ts.title

                        small : Typography
                        small =
                            title.small
                    in
                    { ts | title = { title | small = fn small } }
                )
            ++ group "Label"
                typescale.label
                (\fn ts ->
                    let
                        label : { small : Typography, medium : Typography, large : Typography }
                        label =
                            ts.label

                        large : Typography
                        large =
                            label.large
                    in
                    { ts | label = { label | large = fn large } }
                )
                (\fn ts ->
                    let
                        label : { small : Typography, medium : Typography, large : Typography }
                        label =
                            ts.label

                        medium : Typography
                        medium =
                            label.medium
                    in
                    { ts | label = { label | medium = fn medium } }
                )
                (\fn ts ->
                    let
                        label : { small : Typography, medium : Typography, large : Typography }
                        label =
                            ts.label

                        small : Typography
                        small =
                            label.small
                    in
                    { ts | label = { label | small = fn small } }
                )
            ++ group "Body"
                typescale.body
                (\fn ts ->
                    let
                        body : { small : Typography, medium : Typography, large : Typography }
                        body =
                            ts.body

                        large : Typography
                        large =
                            body.large
                    in
                    { ts | body = { body | large = fn large } }
                )
                (\fn ts ->
                    let
                        body : { small : Typography, medium : Typography, large : Typography }
                        body =
                            ts.body

                        medium : Typography
                        medium =
                            body.medium
                    in
                    { ts | body = { body | medium = fn medium } }
                )
                (\fn ts ->
                    let
                        body : { small : Typography, medium : Typography, large : Typography }
                        body =
                            ts.body

                        small : Typography
                        small =
                            body.small
                    in
                    { ts | body = { body | small = fn small } }
                )
            ++ [ divider ]
        )
