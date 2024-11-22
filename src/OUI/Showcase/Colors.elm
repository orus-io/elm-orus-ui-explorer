module OUI.Showcase.Colors exposing (Model, Msg, book)

import Color exposing (Color)
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import File.Download
import Json.Encode
import OUI
import OUI.Button
import OUI.Dialog
import OUI.Element.Modal
import OUI.Explorer as Explorer
import OUI.Material
import OUI.Material.Color exposing (KeyColors)
import OUI.Material.Color.Json
import OUI.Material.Theme
import OUI.Material.Typography
import OUI.Menu
import OUI.MenuButton
import OUI.Showcase.ColorPicker as ColorPicker
import OUI.Showcase.IconsCat
import OUI.Text


colorCell : String -> String -> Color -> Color -> Int -> Element msg
colorCell name colorcode color onColor height =
    Element.row
        [ Background.color (color |> OUI.Material.Color.toElementColor)
        , Font.color (onColor |> OUI.Material.Color.toElementColor)
        , Element.width Element.fill
        , Element.height <| Element.px height
        , Element.padding 15
        ]
        [ Element.el [ Element.alignTop ] <| Element.text name
        , Element.el [ Element.alignBottom, Element.alignRight ] <| Element.text colorcode
        ]


keyColorEdit :
    (KeyColors -> Color)
    -> (Color -> KeyColors -> KeyColors)
    -> KeyColors
    -> Explorer.BookMsg Msg
keyColorEdit getter setter keyColors =
    EditColor
        (getter keyColors)
        (\c -> keyColors |> setter c |> updateKeyColors)
        |> Explorer.bookMsg


keyColorSetPrimary : Color -> KeyColors -> KeyColors
keyColorSetPrimary c kc =
    { kc | primary = c }


keyColorSetSecondary : Color -> KeyColors -> KeyColors
keyColorSetSecondary c kc =
    { kc | secondary = c }


keyColorSetTertiary : Color -> KeyColors -> KeyColors
keyColorSetTertiary c kc =
    { kc | tertiary = c }


keyColorSetError : Color -> KeyColors -> KeyColors
keyColorSetError c kc =
    { kc | error = c }


keyColorSetNeutral : Color -> KeyColors -> KeyColors
keyColorSetNeutral c kc =
    { kc | neutral = c }


keyColorSetNeutralVariant : Color -> KeyColors -> KeyColors
keyColorSetNeutralVariant c kc =
    { kc | neutralVariant = c }


showKeyColor :
    OUI.Material.Theme.Theme themeExt
    -> Bool
    -> String
    -> (KeyColors -> Color)
    -> (Color -> KeyColors -> KeyColors)
    -> KeyColors
    -> Element (Explorer.BookMsg Msg)
showKeyColor theme editable name getter setter keyColors =
    let
        scheme =
            OUI.Material.Theme.colorscheme theme
    in
    Element.column
        (Element.width Element.fill
            :: (if editable then
                    [ Events.onClick (keyColorEdit getter setter keyColors)
                    , Element.pointer
                    , Element.inFront <|
                        Element.el
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.transparent True
                            , Background.color <| OUI.Material.Color.toElementColor scheme.shadow
                            , Element.mouseOver
                                [ Element.transparent False
                                ]
                            ]
                            (OUI.Showcase.IconsCat.edit
                                |> OUI.Material.icon theme
                                    [ Element.centerX
                                    , Element.centerY
                                    ]
                            )
                    ]

                else
                    []
               )
        )
        [ colorCell name "" scheme.surfaceContainer scheme.onSurface 40
        , let
            c =
                getter keyColors
          in
          colorCell ""
            (OUI.Material.Color.Json.toHexWithAlpha c)
            c
            (if (Color.toHsla c |> .lightness) < 0.5 then
                Color.white

             else
                Color.black
            )
            70
        ]


showKeyColors : Explorer.Shared themeExt -> Element (Explorer.BookMsg Msg)
showKeyColors shared =
    let
        theme : OUI.Material.Theme.Theme themeExt
        theme =
            shared.theme

        colorTheme : Explorer.ColorTheme
        colorTheme =
            Explorer.getSelectedColorTheme shared

        scheme : OUI.Material.Color.Scheme
        scheme =
            OUI.Material.Theme.colorscheme theme

        typescale : OUI.Material.Typography.Typescale
        typescale =
            OUI.Material.Theme.typescale theme

        keyColors : OUI.Material.Color.KeyColors
        keyColors =
            colorTheme.theme.keyColors

        editable : Bool
        editable =
            colorTheme.type_ == Explorer.UserColorTheme
    in
    Element.column
        ([ Element.spacing 5
         , Element.width <| Element.px 820
         ]
            ++ OUI.Material.Typography.attrs OUI.Text.Body OUI.Text.Small OUI.Text.NoColor typescale scheme
        )
        [ OUI.Text.titleMedium "Key Colors"
            |> OUI.Text.withColor OUI.Neutral
            |> OUI.Material.Typography.renderWithAttrs
                typescale
                scheme
                [ Element.paddingEach { bottom = 10, top = 0, left = 0, right = 0 }
                ]
        , Element.row [ Element.spacing 5, Element.width Element.fill ]
            [ showKeyColor theme editable "Primary" .primary keyColorSetPrimary keyColors
            , showKeyColor theme editable "Secondary" .secondary keyColorSetSecondary keyColors
            , showKeyColor theme editable "Tertiary" .tertiary keyColorSetTertiary keyColors
            , showKeyColor theme editable "Error" .error keyColorSetError keyColors
            , showKeyColor theme editable "Neutral" .neutral keyColorSetNeutral keyColors
            , showKeyColor theme editable "Neutral Variant" .neutralVariant keyColorSetNeutralVariant keyColors
            ]
        ]
        |> Element.el
            [ Background.color <| OUI.Material.Color.toElementColor scheme.surfaceContainer
            , Element.padding 15
            , Border.rounded 10
            , Border.color <| OUI.Material.Color.toElementColor scheme.outlineVariant
            , Border.width 1
            ]


showColorScheme : String -> OUI.Material.Theme.Theme themeExt -> Element msg
showColorScheme title theme =
    let
        scheme : OUI.Material.Color.Scheme
        scheme =
            OUI.Material.Theme.colorscheme theme

        typescale : OUI.Material.Typography.Typescale
        typescale =
            OUI.Material.Theme.typescale theme
    in
    Element.column
        ([ Element.spacing 5
         , Element.width <| Element.px 820
         ]
            ++ OUI.Material.Typography.attrs OUI.Text.Body OUI.Text.Small OUI.Text.NoColor typescale scheme
        )
        [ OUI.Text.titleMedium title
            |> OUI.Text.withColor OUI.Neutral
            |> OUI.Material.Typography.renderWithAttrs
                typescale
                scheme
                [ Element.paddingEach { bottom = 10, top = 0, left = 0, right = 0 }
                ]
        , Element.row [ Element.spacing 5, Element.width Element.fill ]
            [ Element.column [ Element.spacing 5, Element.width Element.fill ]
                [ Element.column [ Element.width Element.fill ]
                    [ colorCell "Primary" "" scheme.primary scheme.onPrimary 100
                    , colorCell "On Primary" "" scheme.onPrimary scheme.primary 40
                    ]
                , Element.column [ Element.width Element.fill ]
                    [ colorCell "Primary Container" "" scheme.primaryContainer scheme.onPrimaryContainer 100
                    , colorCell "On Primary Container" "" scheme.onPrimaryContainer scheme.primaryContainer 40
                    ]
                ]
            , Element.column [ Element.spacing 5, Element.width Element.fill ]
                [ Element.column [ Element.width Element.fill ]
                    [ colorCell "Secondary" "" scheme.secondary scheme.onSecondary 100
                    , colorCell "On Secondary" "" scheme.onSecondary scheme.secondary 40
                    ]
                , Element.column [ Element.width Element.fill ]
                    [ colorCell "Secondary Container" "" scheme.secondaryContainer scheme.onSecondaryContainer 100
                    , colorCell "On Secondary Container" "" scheme.onSecondaryContainer scheme.secondaryContainer 40
                    ]
                ]
            , Element.column [ Element.spacing 5, Element.width Element.fill ]
                [ Element.column [ Element.width Element.fill ]
                    [ colorCell "Tertiary" "" scheme.tertiary scheme.onTertiary 100
                    , colorCell "On Tertiary" "" scheme.onTertiary scheme.tertiary 40
                    ]
                , Element.column [ Element.width Element.fill ]
                    [ colorCell "Tertiary Container" "" scheme.tertiaryContainer scheme.onTertiaryContainer 100
                    , colorCell "On Tertiary Container" "" scheme.onTertiaryContainer scheme.tertiaryContainer 40
                    ]
                ]
            , Element.column
                [ Element.spacing 5
                , Element.paddingEach { left = 10, top = 0, bottom = 0, right = 0 }
                , Element.width Element.fill
                ]
                [ Element.column [ Element.width Element.fill ]
                    [ colorCell "Error" "" scheme.error scheme.onError 100
                    , colorCell "On Error" "" scheme.onError scheme.error 40
                    ]
                , Element.column [ Element.width Element.fill ]
                    [ colorCell "Error Container" "" scheme.errorContainer scheme.onErrorContainer 100
                    , colorCell "On Error Container" "" scheme.onErrorContainer scheme.errorContainer 40
                    ]
                ]
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.paddingEach
                { top = 10, left = 0, right = 0, bottom = 0 }
            ]
            [ colorCell "Surface Dim" "" scheme.surfaceDim scheme.onSurface 100
            , colorCell "Surface" "" scheme.surface scheme.onSurface 100
            , colorCell "Surface Bright" "" scheme.surfaceBright scheme.onSurface 100
            ]
        , Element.row
            [ Element.width Element.fill
            ]
            [ colorCell "Surface Container Lowest" "" scheme.surfaceContainerLowest scheme.onSurface 100
            , colorCell "Surface Container Low" "" scheme.surfaceContainerLow scheme.onSurface 100
            , colorCell "Surface Container" "" scheme.surfaceContainer scheme.onSurface 100
            , colorCell "Surface Container High" "" scheme.surfaceContainerHigh scheme.onSurface 100
            , colorCell "Surface Container Highest" "" scheme.surfaceContainerHighest scheme.onSurface 100
            ]
        , Element.row
            [ Element.width Element.fill
            ]
            [ colorCell "On Surface" "" scheme.onSurface scheme.surface 40
            , colorCell "On Surface Variant" "" scheme.onSurfaceVariant scheme.surfaceVariant 40
            , colorCell "Outline" "" scheme.outline scheme.surface 40
            , colorCell "Outline Variant" "" scheme.outlineVariant scheme.onSurface 40
            ]
        ]
        |> Element.el
            [ Background.color <| OUI.Material.Color.toElementColor scheme.surfaceContainer
            , Element.padding 15
            , Border.rounded 10
            , Border.color <| OUI.Material.Color.toElementColor scheme.outlineVariant
            , Border.width 1
            ]


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Colors"
        { init = init
        , update = update
        , subscriptions = \_ _ -> Sub.none
        }
        |> Explorer.withDialog
            (\shared model ->
                model.colorSelector
                    |> Maybe.map
                        (\cp ->
                            OUI.Dialog.new "Select a color"
                                |> OUI.Dialog.onAccept "OK" AcceptColor
                                |> OUI.Dialog.onDismiss "Cancel" DismissColor
                                |> OUI.Material.dialogWithContent
                                    shared.theme
                                    []
                                    (ColorPicker.view shared.theme cp.color cp.colorPicker
                                        |> Element.map ColorPickerMsg
                                    )
                                |> OUI.Element.Modal.map Explorer.bookMsg
                        )
            )
        |> Explorer.withChapter
            (\shared model ->
                let
                    currentColorTheme =
                        Explorer.getSelectedColorTheme shared
                in
                Element.row
                    [ Element.spacing 20
                    ]
                    [ "Current color scheme: "
                        |> OUI.Text.bodyLarge
                        |> OUI.Material.text shared.theme
                    , OUI.MenuButton.new ColorThemeButtonMsg
                        (\i ->
                            SelectColorScheme i
                                (Tuple.second shared.selectedColorScheme)
                        )
                        (OUI.Button.new
                            currentColorTheme.theme.name
                        )
                        (OUI.Menu.new
                            (\i ->
                                Explorer.getColorTheme i shared
                                    |> .theme
                                    |> .name
                            )
                            |> OUI.Menu.addItems (List.range 0 (List.length shared.colorThemeList - 1))
                        )
                        |> OUI.MenuButton.alignBottom
                        |> OUI.Material.menuButton shared.theme
                            model.colorThemeButton
                            [ Element.centerX
                            ]
                        |> Element.map Explorer.bookMsg
                    , OUI.Button.new "Copy"
                        |> OUI.Button.onClick CopyColorTheme
                        |> OUI.Material.button shared.theme
                            [ Element.centerX
                            ]
                        |> Element.map Explorer.bookMsg
                    , case currentColorTheme.type_ of
                        Explorer.BuiltinColorTheme ->
                            Element.none

                        Explorer.UserColorTheme ->
                            OUI.Button.new "Delete"
                                |> OUI.Button.onClick
                                    (Tuple.first shared.selectedColorScheme
                                        |> Explorer.deleteColorThemeMsg
                                        |> Explorer.sharedMsg
                                    )
                                |> OUI.Material.button shared.theme
                                    [ Element.centerX
                                    ]
                    , OUI.Button.new "Export"
                        |> OUI.Button.onClick ExportColor
                        |> OUI.Material.button shared.theme
                            [ Element.centerX
                            ]
                        |> Element.map Explorer.bookMsg
                    ]
            )
        |> Explorer.withStaticChapter
            showKeyColors
        |> Explorer.withStaticChapter
            (\shared ->
                shared.theme
                    |> OUI.Material.Theme.withColorscheme
                        (shared.colorThemeList
                            |> List.drop (shared.selectedColorScheme |> Tuple.first)
                            |> List.head
                            |> Maybe.map (.theme >> .schemes >> .light)
                            |> Maybe.withDefault OUI.Material.Color.defaultLightScheme
                        )
                    |> showColorScheme "Light Scheme"
            )
        |> Explorer.withStaticChapter
            (\shared ->
                shared.theme
                    |> OUI.Material.Theme.withColorscheme
                        (shared.colorThemeList
                            |> List.drop (shared.selectedColorScheme |> Tuple.first)
                            |> List.head
                            |> Maybe.map (.theme >> .schemes >> .dark)
                            |> Maybe.withDefault OUI.Material.Color.defaultDarkScheme
                        )
                    |> showColorScheme "Dark Scheme"
            )


type alias ColorSetter =
    Color -> OUI.Material.Color.Theme -> OUI.Material.Color.Theme


type Msg
    = ColorThemeButtonMsg (OUI.MenuButton.Msg Int Msg)
    | SelectColorScheme Int Explorer.ColorSchemeType
    | CopyColorTheme
    | EditColor Color ColorSetter
    | ColorPickerMsg ColorPicker.Msg
    | AcceptColor
    | DismissColor
    | ExportColor


type alias Model =
    { colorThemeButton : OUI.MenuButton.State
    , colorSelector :
        Maybe
            { setter : ColorSetter
            , color : Color
            , colorPicker : ColorPicker.State
            }
    }


init : Explorer.Shared themeExt -> ( Model, Effect Explorer.SharedMsg Msg )
init shared =
    { colorThemeButton = OUI.MenuButton.init "color-page-color-theme-button"
    , colorSelector = Nothing
    }
        |> Effect.withNone


update : Explorer.Shared themeExt -> Msg -> Model -> ( Model, Effect Explorer.SharedMsg Msg )
update shared msg model =
    case msg of
        ColorThemeButtonMsg buttonMsg ->
            let
                ( state, cmd ) =
                    OUI.MenuButton.update buttonMsg model.colorThemeButton
            in
            { model
                | colorThemeButton = state
            }
                |> Effect.withCmd cmd

        SelectColorScheme i t ->
            model
                |> Effect.withShared (Explorer.selectColorScheme i t)

        CopyColorTheme ->
            let
                currentColorTheme =
                    Explorer.getSelectedColorTheme shared |> .theme
            in
            model
                |> Effect.withShared
                    (Explorer.addColorThemeMsg
                        { currentColorTheme | name = currentColorTheme.name ++ " (copy)" }
                    )

        ColorPickerMsg colorPickerMsg ->
            (case model.colorSelector of
                Just colorSelector ->
                    let
                        ( m, color ) =
                            ColorPicker.update colorPickerMsg colorSelector.color colorSelector.colorPicker
                    in
                    { model
                        | colorSelector =
                            Just
                                { colorSelector
                                    | colorPicker = m
                                    , color = color |> Maybe.withDefault colorSelector.color
                                }
                    }

                Nothing ->
                    model
            )
                |> Effect.withNone

        EditColor color setter ->
            { model
                | colorSelector =
                    Just
                        { setter = setter
                        , color = color
                        , colorPicker = ColorPicker.empty
                        }
            }
                |> Effect.withNone

        DismissColor ->
            { model | colorSelector = Nothing }
                |> Effect.withNone

        AcceptColor ->
            case model.colorSelector of
                Just colorSelector ->
                    let
                        currentThemeIndex =
                            shared.selectedColorScheme |> Tuple.first

                        theme =
                            Explorer.getSelectedColorTheme shared
                                |> .theme
                    in
                    { model | colorSelector = Nothing }
                        |> Effect.withShared
                            (Explorer.updateColorThemeMsg
                                currentThemeIndex
                                (theme |> colorSelector.setter colorSelector.color)
                            )

                Nothing ->
                    model
                        |> Effect.withNone

        ExportColor ->
            let
                currentColorTheme =
                    Explorer.getSelectedColorTheme shared |> .theme
            in
            model
                |> Effect.withCmd
                    (currentColorTheme
                        |> OUI.Material.Color.Json.encodeColorTheme
                        |> Json.Encode.encode 2
                        |> File.Download.string (currentColorTheme.name ++ ".json") "application/json"
                    )


updateKeyColors : KeyColors -> OUI.Material.Color.Theme -> OUI.Material.Color.Theme
updateKeyColors keyColors theme =
    -- TODO propagate only the keyColors that were changed
    { theme
        | keyColors = keyColors
        , schemes =
            { light = OUI.Material.Color.lightFromKeyColors keyColors
            , dark = OUI.Material.Color.darkFromKeyColors keyColors
            }
    }
