module OUI.Showcase.Buttons exposing (Model, Msg, book, commonButtonVariants, commonButtons)

import Effect
import Element exposing (Element)
import OUI
import OUI.Button as Button exposing (Button)
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon exposing (clear)
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.Text as Text


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Buttons"
        { init = \_ -> {} |> Effect.withNone
        , update = \_ () m -> m |> Effect.withNone
        , subscriptions = \_ _ -> Sub.none
        }
        |> Explorer.withStaticChapter commonButtons
        |> Explorer.withThemeEditor editorChapter


type alias Model =
    {}


type alias Msg =
    ()


updateButtonTheme :
    (OUI.Material.Theme.ButtonTheme -> OUI.Material.Theme.ButtonTheme)
    -> Theme themeExt
    -> Theme themeExt
updateButtonTheme fn theme =
    theme
        |> OUI.Material.Theme.withButton
            (theme
                |> OUI.Material.Theme.button
                |> fn
            )


updateButtonCommon :
    (OUI.Material.Theme.ButtonLayout -> OUI.Material.Theme.ButtonLayout)
    -> OUI.Material.Theme.Theme themeExt
    -> OUI.Material.Theme.Theme themeExt
updateButtonCommon fn =
    updateButtonTheme
        (\b ->
            { b
                | common = fn b.common
            }
        )


updateButtonCommonMsg :
    (data -> OUI.Material.Theme.ButtonLayout -> OUI.Material.Theme.ButtonLayout)
    -> data
    -> Explorer.BookMsg themeExt msg
updateButtonCommonMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateButtonCommon (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
editorChapter { theme } _ =
    let
        buttonTheme : OUI.Material.Theme.ButtonTheme
        buttonTheme =
            OUI.Material.Theme.button theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Button Layout" |> Material.text theme
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | containerHeight = round value }
                )
            )
            "Container Height"
            ( 0, 200 )
            (toFloat buttonTheme.common.containerHeight)
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | containerRadius = round value }
                )
            )
            "Container Radius"
            ( 0, buttonTheme.common.containerHeight // 2 |> toFloat )
            (toFloat buttonTheme.common.containerRadius)
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | iconSize = round value }
                )
            )
            "Icon Size"
            ( 0, buttonTheme.common.containerHeight |> toFloat )
            (toFloat buttonTheme.common.iconSize)
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | leftRightPadding = round value }
                )
            )
            "Left/right Padding"
            ( 0, buttonTheme.common.containerHeight |> toFloat )
            (toFloat buttonTheme.common.leftRightPadding)
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | leftPaddingWithIcon = round value }
                )
            )
            "Left pad. w. Icon"
            ( 0, buttonTheme.common.containerHeight |> toFloat )
            (toFloat buttonTheme.common.leftPaddingWithIcon)
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | rightPaddingWithIcon = round value }
                )
            )
            "Right pad. w. Icon"
            ( 0, buttonTheme.common.containerHeight |> toFloat )
            (toFloat buttonTheme.common.rightPaddingWithIcon)
        , ThemeEditor.slider theme
            (updateButtonCommonMsg
                (\value layout ->
                    { layout | paddingBetweenElements = round value }
                )
            )
            "Pad. bw. Elements"
            ( 0, buttonTheme.common.containerHeight |> toFloat )
            (toFloat buttonTheme.common.paddingBetweenElements)
        , ThemeEditor.textType theme
            (updateButtonCommonMsg
                (\type_ layout ->
                    { layout | textType = type_ }
                )
            )
            "Text Type"
            buttonTheme.common.textType
        , ThemeEditor.textSize theme
            (updateButtonCommonMsg
                (\size layout ->
                    { layout | textSize = size }
                )
            )
            "Text Size"
            buttonTheme.common.textSize
        , divider
        ]


{-| A common button with/without icon, as a link, disabled
-}
commonButtonVariants :
    OUI.Material.Theme.Theme themeExt
    -> String
    -> (Button { hasNoIcon : (), needOnClickOrDisabled : () } (Explorer.BookMsg themeExt msg) -> Button { hasNoIcon : (), needOnClickOrDisabled : () } (Explorer.BookMsg themeExt msg))
    -> List (Element (Explorer.BookMsg themeExt msg))
commonButtonVariants theme label btnType =
    [ -- no icon, action
      Button.new label
        |> btnType
        |> Button.onClick (Explorer.logEvent <| "Clicked " ++ label)
        |> Material.button theme [ Element.centerX ]
    , -- icon, action
      Button.new "with Icon"
        |> btnType
        |> Button.withIcon clear
        |> Button.onClick (Explorer.logEvent <| "Clicked " ++ label ++ " + icon")
        |> Material.button theme [ Element.centerX ]
    , -- no icon, link
      Button.new "Link"
        |> btnType
        |> Button.link "#/Basics/Buttons"
        |> Material.button theme [ Element.centerX ]
    , -- icon, link
      Button.new "Link Icon"
        |> btnType
        |> Button.withIcon clear
        |> Button.link "#/Basics/Buttons"
        |> Material.button theme [ Element.centerX ]
    , -- icon, disabled
      Button.new "Disabled"
        |> btnType
        |> Button.withIcon clear
        |> Button.disabled
        |> Material.button theme [ Element.centerX ]
    ]


commonButtons : Explorer.Shared themeExt -> Element (Explorer.BookMsg themeExt Msg)
commonButtons { theme } =
    let
        btnRow :
            String
            ->
                (Button
                    { hasNoIcon : ()
                    , needOnClickOrDisabled : ()
                    }
                    (Explorer.BookMsg themeExt msg)
                 ->
                    Button
                        { hasNoIcon : ()
                        , needOnClickOrDisabled : ()
                        }
                        (Explorer.BookMsg themeExt msg)
                )
            -> Element (Explorer.BookMsg themeExt msg)
        btnRow label btnType =
            commonButtonVariants theme label btnType
                |> (::) (Text.titleSmall label |> Material.text theme)
                |> List.map (Element.el [ Element.width <| Element.px 100 ])
                |> Element.row [ Element.spacing 30 ]

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Common buttons" |> Material.text theme
        , Element.column [ Element.spacing 30 ]
            [ btnRow "Elevated" Button.elevatedButton
            , btnRow "Filled" Button.filledButton
            , btnRow "Outlined" Button.outlinedButton
            , btnRow "Text" Button.textButton
            ]
        , divider
        , Text.titleLarge "FAB" |> Material.text theme
        , Element.row [ Element.spacing 30 ]
            (let
                btn : String -> Button { hasIcon : (), needOnClickOrDisabled : () } (Explorer.BookMsg themeExt msg)
                btn s =
                    Button.new (s ++ "FAB")
                        |> Button.withIcon clear

                clickBtn : String -> Button { hasAction : (), hasIcon : () } (Explorer.BookMsg themeExt msg)
                clickBtn s =
                    btn s
                        |> Button.onClick (Explorer.logEvent <| "Clicked " ++ s ++ " FAB")

                linkBtn : String -> Button { hasAction : (), hasIcon : () } (Explorer.BookMsg themeExt msg)
                linkBtn s =
                    btn s
                        |> Button.link "#/Basics/Buttons"
             in
             [ clickBtn "Small"
                |> Button.smallFAB
                |> Button.color OUI.PrimaryContainer
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Small"
                |> Button.smallFAB
                |> Button.color OUI.PrimaryContainer
                |> Material.button theme [ Element.centerX ]
             , clickBtn "Medium"
                |> Button.mediumFAB
                |> Button.color OUI.SecondaryContainer
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Medium"
                |> Button.mediumFAB
                |> Button.color OUI.SecondaryContainer
                |> Material.button theme [ Element.centerX ]
             , clickBtn "Large"
                |> Button.largeFAB
                |> Button.color OUI.TertiaryContainer
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Large"
                |> Button.largeFAB
                |> Button.color OUI.TertiaryContainer
                |> Material.button theme [ Element.centerX ]
             , clickBtn "Extended"
                |> Button.extendedFAB
                |> Button.color OUI.Primary
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Extended"
                |> Button.extendedFAB
                |> Button.color OUI.Primary
                |> Material.button theme [ Element.centerX ]
             ]
            )
        , divider
        , Text.titleLarge "Icon buttons" |> Material.text theme
        , Element.row [ Element.spacing 30 ]
            (let
                btn : String -> Button { hasIcon : (), needOnClickOrDisabled : () } (Explorer.BookMsg themeExt msg)
                btn s =
                    Button.new (s ++ " Icon")
                        |> Button.withIcon clear

                clickBtn : String -> Button { hasAction : (), hasIcon : () } (Explorer.BookMsg themeExt msg)
                clickBtn s =
                    btn s
                        |> Button.onClick (Explorer.logEvent <| "Clicked " ++ s ++ " Icon")

                linkBtn : String -> Button { hasAction : (), hasIcon : () } (Explorer.BookMsg themeExt msg)
                linkBtn s =
                    btn s
                        |> Button.link "#/Basics/Buttons"
             in
             [ clickBtn "Standard"
                |> Button.iconButton
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Standard"
                |> Button.iconButton
                |> Material.button theme [ Element.centerX ]
             , clickBtn "Filled"
                |> Button.filledIconButton
                |> Button.color OUI.Primary
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Filled"
                |> Button.filledIconButton
                |> Button.color OUI.Primary
                |> Material.button theme [ Element.centerX ]
             , clickBtn "Outlined"
                |> Button.outlinedIconButton
                |> Button.color OUI.Primary
                |> Material.button theme [ Element.centerX ]
             , linkBtn "Outlined"
                |> Button.outlinedIconButton
                |> Button.color OUI.Primary
                |> Material.button theme [ Element.centerX ]
             ]
            )
        ]
