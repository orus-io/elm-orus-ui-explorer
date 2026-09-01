module OUI.Showcase.TextFields exposing (InputState, Model, Msg(..), book, inputHasFocus, inputText, newInputState, textfields, update)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import OUI
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon exposing (check, clear)
import OUI.Material as Material
import OUI.Material.Color
import OUI.Material.Theme exposing (Theme)
import OUI.Tabs
import OUI.Text as Text
import OUI.TextField as TextField exposing (TextField)


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Text Fields"
        { init =
            \_ ->
                { inputs = Dict.empty
                , selectedTab = TextField.Text
                }
                    |> Effect.withNone
        , update =
            update
        , subscriptions = \_ _ -> Sub.none
        }
        |> Explorer.withChapter tabs
        |> Explorer.withThemeEditor editorChapter


type alias InputState =
    { text : String
    , hasFocus : Bool
    }


newInputState : InputState
newInputState =
    { text = ""
    , hasFocus = False
    }


type alias Model =
    { inputs : Dict String InputState
    , selectedTab : TextField.Datatype
    }


inputText : String -> Model -> String
inputText name { inputs } =
    Dict.get name inputs
        |> Maybe.withDefault newInputState
        |> .text


inputHasFocus : String -> Model -> Bool
inputHasFocus name { inputs } =
    Dict.get name inputs
        |> Maybe.withDefault newInputState
        |> .hasFocus


type Msg
    = OnChange String String
    | OnFocus String
    | OnLoseFocus String
    | SelectTab TextField.Datatype


update : a -> Msg -> Model -> ( Model, Effect sharedMsg msg )
update _ msg model =
    case msg of
        SelectTab tab ->
            { model
                | selectedTab = tab
            }
                |> Effect.withNone

        OnChange name value ->
            let
                input : InputState
                input =
                    Dict.get name model.inputs
                        |> Maybe.withDefault newInputState
            in
            { model
                | inputs =
                    Dict.insert name
                        { input | text = value }
                        model.inputs
            }
                |> Effect.withNone

        OnFocus name ->
            let
                input : InputState
                input =
                    Dict.get name model.inputs
                        |> Maybe.withDefault newInputState
            in
            { model
                | inputs =
                    Dict.insert name
                        { input | hasFocus = True }
                        model.inputs
            }
                |> Effect.withNone

        OnLoseFocus name ->
            let
                input : InputState
                input =
                    Dict.get name model.inputs
                        |> Maybe.withDefault newInputState
            in
            { model
                | inputs =
                    Dict.insert name
                        { input | hasFocus = False }
                        model.inputs
            }
                |> Effect.withNone


updateTextfieldTheme :
    (OUI.Material.Theme.TextFieldTheme -> OUI.Material.Theme.TextFieldTheme)
    -> Theme themeExt
    -> Theme themeExt
updateTextfieldTheme fn theme =
    theme
        |> OUI.Material.Theme.withTextfield
            (theme
                |> OUI.Material.Theme.textfield
                |> fn
            )


updateTextfieldMsg :
    (data -> OUI.Material.Theme.TextFieldTheme -> OUI.Material.Theme.TextFieldTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateTextfieldMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateTextfieldTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
editorChapter { theme } _ =
    let
        textfieldTheme : OUI.Material.Theme.TextFieldTheme
        textfieldTheme =
            OUI.Material.Theme.textfield theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Layout" |> Material.text theme
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | height = round value }
                )
            )
            "Height"
            ( 0, 150, 1 )
            (toFloat textfieldTheme.height)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | leftRightPaddingWithoutIcon = round value }
                )
            )
            "Left/Right Padding Without Icon"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.leftRightPaddingWithoutIcon)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | leftRightPaddingWithIcon = round value }
                )
            )
            "Left/Right Padding With Icon"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.leftRightPaddingWithIcon)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | paddingBetweenIconAndText = round value }
                )
            )
            "Between Icon And Text"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.paddingBetweenIconAndText)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | supportingTextTopPadding = round value }
                )
            )
            "Supporting Text Top Padding"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.supportingTextTopPadding)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | paddingBetweenSupportingTextAndCharacterCounter = round value }
                )
            )
            "Between Supporting Text And Character Counter"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.paddingBetweenSupportingTextAndCharacterCounter)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | iconSize = round value }
                )
            )
            "Icon Size"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.iconSize)
        , divider
        , Text.titleLarge "Filled" |> Material.text theme
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    { t | filled = { topBottomPadding = round value } }
                )
            )
            "Top/Bottom Padding"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.filled.topBottomPadding)
        , divider
        , Text.titleLarge "Outlined" |> Material.text theme
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    let
                        outlined : { labelLeftRightPadding : Int, labelBottom : Int, shape : Int }
                        outlined =
                            t.outlined
                    in
                    { t | outlined = { outlined | labelLeftRightPadding = round value } }
                )
            )
            "Label Left/Right Padding"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.outlined.labelLeftRightPadding)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    let
                        outlined : { labelLeftRightPadding : Int, labelBottom : Int, shape : Int }
                        outlined =
                            t.outlined
                    in
                    { t | outlined = { outlined | labelBottom = round value } }
                )
            )
            "Label Bottom"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.outlined.labelBottom)
        , ThemeEditor.slider theme
            (updateTextfieldMsg
                (\value t ->
                    let
                        outlined : { labelLeftRightPadding : Int, labelBottom : Int, shape : Int }
                        outlined =
                            t.outlined
                    in
                    { t | outlined = { outlined | shape = round value } }
                )
            )
            "Shape"
            ( 0, 50, 1 )
            (toFloat textfieldTheme.outlined.shape)
        , divider
        ]


tabs : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
tabs shared model =
    Element.column [ Element.width Element.fill, Element.spacing 2 ]
        [ OUI.Tabs.new identity (Explorer.bookMsg << SelectTab)
            |> OUI.Tabs.withItems
                [ ( TextField.Text, "Text" )
                , ( TextField.Search, "Search" )
                , ( TextField.Username, "Username" )
                , ( TextField.Email, "Email" )
                , ( TextField.Password False, "Password" )
                , ( TextField.Password True, "Password (show)" )
                , ( TextField.NewPassword False, "New Password" )
                , ( TextField.NewPassword True, "New Password (show)" )
                , ( TextField.Multiline, "Multiline" )
                ]
            |> OUI.Tabs.secondary
            |> OUI.Tabs.withSelected model.selectedTab
            |> Material.tabs shared.theme [ Element.width Element.fill ]
        , case model.selectedTab of
            TextField.Text ->
                textfields ( "text", identity ) shared model

            TextField.Search ->
                textfields ( "search", TextField.search ) shared model

            TextField.Username ->
                textfields ( "username", TextField.username ) shared model

            TextField.Email ->
                textfields ( "email", TextField.email ) shared model

            TextField.Password show ->
                textfields ( "password", TextField.password show ) shared model

            TextField.NewPassword show ->
                textfields ( "newpassword", TextField.newPassword show ) shared model

            TextField.Multiline ->
                textfields ( "multiline", TextField.multiline True ) shared model
        ]


textfields : ( String, TextField (Explorer.BookMsg themeExt Msg) -> TextField (Explorer.BookMsg themeExt Msg) ) -> Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
textfields ( datatype, setDatatype ) { theme } model =
    let
        colorscheme : OUI.Material.Color.Scheme
        colorscheme =
            OUI.Material.Theme.colorscheme theme

        key : String -> String
        key name =
            name ++ "-" ++ datatype

        textField : String -> String -> TextField (Explorer.BookMsg themeExt Msg)
        textField label name =
            TextField.new label
                (Explorer.bookMsg << (OnChange <| key name))
                (inputText (key name) model)
                |> TextField.onFocusBlur
                    (Explorer.bookMsg <| OnFocus <| key name)
                    (Explorer.bookMsg <| OnLoseFocus <| key name)
                |> TextField.withFocused (inputHasFocus (key name) model)

        render : TextField (Explorer.BookMsg themeExt Msg) -> Element (Explorer.BookMsg themeExt Msg)
        render =
            setDatatype
                >> Material.textField theme
                    [ Element.centerX
                    , Element.centerY
                    , Element.width Element.fill
                    ]
    in
    Element.row
        [ Border.width 1
        , colorscheme.outline
            |> OUI.Material.Color.toElementColor
            |> Border.color
        , colorscheme.surfaceContainer
            |> OUI.Material.Color.toElementColor
            |> Background.color
        ]
        [ Element.column
            [ Element.spacing 20
            , Element.width <| Element.px 500
            , Element.padding 40
            ]
            [ textField "Filled" "filled"
                |> TextField.withSupportingText "A filled text field"
                |> TextField.withType TextField.Filled
                |> render
            , textField "Filled" "filledLeadIcon"
                |> TextField.withSupportingText "A filled text field with leading icon"
                |> TextField.withLeadingIcon check
                |> TextField.withType TextField.Filled
                |> render
            , textField "Filled" "filledTrailIcon"
                |> TextField.withSupportingText "A filled text field with trailing icon"
                |> TextField.withTrailingIcon clear
                |> TextField.withType TextField.Filled
                |> render
            , let
                k : String
                k =
                    key "filledLeadTrailClickIcon"
              in
              TextField.new "Filled"
                (OnChange k
                    >> Explorer.bookMsg
                )
                (inputText k model)
                |> TextField.onFocusBlur
                    (OnFocus k |> Explorer.bookMsg)
                    (OnLoseFocus k |> Explorer.bookMsg)
                |> TextField.withFocused (inputHasFocus k model)
                |> TextField.withSupportingText "A filled text field with clickable trailing icon"
                |> TextField.withLeadingIcon check
                |> TextField.withClickableTrailingIcon (Explorer.logEvent "Clicked !") clear
                |> TextField.withType TextField.Filled
                |> render
            , textField "Filled" "filledError"
                |> TextField.withSupportingText "A filled text field with error"
                |> TextField.withType TextField.Filled
                |> TextField.withColor OUI.Error
                |> render
            , textField "Filled" "filledErrorIcon"
                |> TextField.withSupportingText "A filled text field with a error icon"
                |> TextField.withType TextField.Filled
                |> TextField.withErrorIcon clear
                |> TextField.withColor OUI.Error
                |> render
            ]
        , Element.column
            [ Element.spacing 20
            , Element.width <| Element.px 500
            , Element.padding 40
            ]
            [ textField "Outlined" "outlined"
                |> TextField.withType TextField.Outlined
                |> TextField.withSupportingText "A outlined text field"
                |> render
            , textField "Outlined" "outlinedLeadIcon"
                |> TextField.withSupportingText "A outlined text field with leading icon"
                |> TextField.withLeadingIcon check
                |> TextField.withType TextField.Outlined
                |> render
            , textField "Outlined" "outlinedTrailIcon"
                |> TextField.withSupportingText "A outlined text field with trailing icon"
                |> TextField.withTrailingIcon clear
                |> TextField.withType TextField.Outlined
                |> render
            , let
                k : String
                k =
                    key "outlinedLeadTrailClickIcon"
              in
              TextField.new "Outlined"
                (OnChange k
                    >> Explorer.bookMsg
                )
                (inputText k model)
                |> TextField.onFocusBlur
                    (OnFocus k |> Explorer.bookMsg)
                    (OnLoseFocus k |> Explorer.bookMsg)
                |> TextField.withFocused (inputHasFocus k model)
                |> TextField.withSupportingText "A outlined text field with clickable trailing icon"
                |> TextField.withLeadingIcon check
                |> TextField.withClickableTrailingIcon (Explorer.logEvent "Clicked !") clear
                |> TextField.withType TextField.Outlined
                |> render
            , textField "Outlined" "outlinedError"
                |> TextField.withSupportingText "A outlined text field with error"
                |> TextField.withType TextField.Outlined
                |> TextField.withColor OUI.Error
                |> render
            , textField "Outlined" "outlinedErrorIcon"
                |> TextField.withSupportingText "A outlined text field with a error icon"
                |> TextField.withType TextField.Outlined
                |> TextField.withErrorIcon clear
                |> TextField.withColor OUI.Error
                |> render
            ]
        ]
