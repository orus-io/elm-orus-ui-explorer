module OUI.Showcase.Checkbox exposing (Model, Msg, book)

import Effect exposing (Effect)
import Element exposing (Element)
import OUI
import OUI.Checkbox as Checkbox
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon exposing (clear)
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.Text as Text


type alias Model =
    { basicCheck : Bool
    , basicUncheck : Bool
    , customIconCheck : Bool
    , customIconUncheck : Bool
    , errorCheck : Bool
    , errorUncheck : Bool
    }


type Msg
    = OnClickBasicCheck
    | OnClickBasicUncheck
    | OnClickCustomIconCheck
    | OnClickCustomIconUncheck
    | OnClickErrorCheck
    | OnClickErrorUncheck


init : Model
init =
    { basicCheck = True
    , basicUncheck = False
    , customIconCheck = True
    , customIconUncheck = False
    , errorCheck = True
    , errorUncheck = False
    }


update : Explorer.Shared themeExt -> Msg -> Model -> ( Model, Effect shared msg )
update _ msg model =
    case msg of
        OnClickBasicCheck ->
            { model | basicCheck = not model.basicCheck }
                |> Effect.withNone

        OnClickBasicUncheck ->
            { model | basicUncheck = not model.basicUncheck }
                |> Effect.withNone

        OnClickCustomIconCheck ->
            { model | customIconCheck = not model.customIconCheck }
                |> Effect.withNone

        OnClickCustomIconUncheck ->
            { model | customIconUncheck = not model.customIconUncheck }
                |> Effect.withNone

        OnClickErrorCheck ->
            { model | errorCheck = not model.errorCheck }
                |> Effect.withNone

        OnClickErrorUncheck ->
            { model | errorUncheck = not model.errorUncheck }
                |> Effect.withNone


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Checkbox"
        { init = \_ -> init |> Effect.withNone
        , update = update
        , subscriptions = \_ _ -> Sub.none
        }
        |> Explorer.withChapter checkbox
        |> Explorer.withThemeEditor editorChapter


updateCheckboxTheme :
    (OUI.Material.Theme.CheckboxTheme -> OUI.Material.Theme.CheckboxTheme)
    -> Theme themeExt
    -> Theme themeExt
updateCheckboxTheme fn theme =
    theme
        |> OUI.Material.Theme.withCheckbox
            (theme
                |> OUI.Material.Theme.checkbox
                |> fn
            )


updateCheckboxMsg :
    (data -> OUI.Material.Theme.CheckboxTheme -> OUI.Material.Theme.CheckboxTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateCheckboxMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateCheckboxTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
editorChapter { theme } _ =
    let
        checkboxTheme : OUI.Material.Theme.CheckboxTheme
        checkboxTheme =
            OUI.Material.Theme.checkbox theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Checkbox" |> Material.text theme
        , ThemeEditor.slider theme
            (updateCheckboxMsg
                (\value c ->
                    { c | containerWidth = round value }
                )
            )
            "Container Width"
            ( 0, 100, 1 )
            (toFloat checkboxTheme.containerWidth)
        , ThemeEditor.slider theme
            (updateCheckboxMsg
                (\value c ->
                    { c | containerHeight = round value }
                )
            )
            "Container Height"
            ( 0, 100, 1 )
            (toFloat checkboxTheme.containerHeight)
        , ThemeEditor.slider theme
            (updateCheckboxMsg
                (\value c ->
                    { c | containerShape = round value }
                )
            )
            "Container Shape"
            ( 0, 100, 1 )
            (toFloat checkboxTheme.containerShape)
        , ThemeEditor.slider theme
            (updateCheckboxMsg
                (\value c ->
                    { c | iconSize = round value }
                )
            )
            "Icon Size"
            ( 0, 100, 1 )
            (toFloat checkboxTheme.iconSize)
        , ThemeEditor.slider theme
            (updateCheckboxMsg
                (\value c ->
                    { c | stateLayerSize = round value }
                )
            )
            "State Layer Size"
            ( 0, 100, 1 )
            (toFloat checkboxTheme.stateLayerSize)
        , divider
        ]


checkbox : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
checkbox { theme } model =
    let
        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Element.row [ Element.spacing 30, Element.padding 30 ]
            [ Element.column [ Element.spacing 55, Element.padding 30 ]
                [ Element.none |> Element.el []
                , Text.titleSmall "Basic" |> Material.text theme
                , Text.titleSmall "Disabled" |> Material.text theme
                , Text.titleSmall "Custom Icon" |> Material.text theme
                , Text.titleSmall "Error" |> Material.text theme
                ]
            , Element.column [ Element.spacing 30 ]
                [ Text.titleSmall "Unchecked" |> Material.text theme
                , Checkbox.new
                    |> Checkbox.onChange (\_ -> OnClickBasicUncheck)
                    |> Checkbox.withChecked model.basicUncheck
                    |> Material.checkbox theme []
                , Checkbox.new
                    |> Checkbox.disabled
                    |> Checkbox.withChecked False
                    |> Material.checkbox theme []
                , Checkbox.new
                    |> Checkbox.onChange (\_ -> OnClickCustomIconUncheck)
                    |> Checkbox.withChecked model.customIconUncheck
                    |> Checkbox.withIcon clear
                    |> Material.checkbox theme []
                , Checkbox.new
                    |> Checkbox.onChange (\_ -> OnClickErrorUncheck)
                    |> Checkbox.withChecked model.errorUncheck
                    |> Checkbox.withColor OUI.Error
                    |> Material.checkbox theme []
                ]
            , Element.column [ Element.spacing 30 ]
                [ Text.titleSmall "Checked" |> Material.text theme
                , Checkbox.new
                    |> Checkbox.onChange (\_ -> OnClickBasicCheck)
                    |> Checkbox.withChecked model.basicCheck
                    |> Material.checkbox theme []
                , Checkbox.new
                    |> Checkbox.disabled
                    |> Checkbox.withChecked True
                    |> Material.checkbox theme []
                , Checkbox.new
                    |> Checkbox.onChange (\_ -> OnClickCustomIconCheck)
                    |> Checkbox.withChecked model.customIconCheck
                    |> Checkbox.withIcon clear
                    |> Material.checkbox theme []
                , Checkbox.new
                    |> Checkbox.onChange (\_ -> OnClickErrorCheck)
                    |> Checkbox.withChecked model.errorCheck
                    |> Checkbox.withColor OUI.Error
                    |> Material.checkbox theme []
                ]
            ]
        ]
        |> Element.map Explorer.bookMsg
