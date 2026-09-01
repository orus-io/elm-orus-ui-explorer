module OUI.Showcase.RadioButtons exposing (Model, Msg, book)

import Effect exposing (Effect)
import Element exposing (Element)
import OUI
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.RadioButton as RadioButton
import OUI.Text as Text


type alias Model =
    { basicSelect : Bool
    , basicUnselect : Bool
    , errorSelect : Bool
    , errorUnselect : Bool
    }


type Msg
    = OnClickBasicSelect
    | OnClickBasicUnselect
    | OnClickErrorSelect
    | OnClickErrorUnselect


init : Model
init =
    { basicSelect = True
    , basicUnselect = False
    , errorSelect = True
    , errorUnselect = False
    }


update : Explorer.Shared themeExt -> Msg -> Model -> ( Model, Effect shared msg )
update _ msg model =
    case msg of
        OnClickBasicSelect ->
            { model | basicSelect = not model.basicSelect }
                |> Effect.withNone

        OnClickBasicUnselect ->
            { model | basicUnselect = not model.basicUnselect }
                |> Effect.withNone

        OnClickErrorSelect ->
            { model | errorSelect = not model.errorSelect }
                |> Effect.withNone

        OnClickErrorUnselect ->
            { model | errorUnselect = not model.errorUnselect }
                |> Effect.withNone


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Radio Buttons"
        { init = \_ -> init |> Effect.withNone
        , update = update
        , subscriptions = \_ _ -> Sub.none
        }
        |> Explorer.withChapter radiobuttons
        |> Explorer.withThemeEditor editorChapter


updateRadiobuttonTheme :
    (OUI.Material.Theme.RadioButtonTheme -> OUI.Material.Theme.RadioButtonTheme)
    -> Theme themeExt
    -> Theme themeExt
updateRadiobuttonTheme fn theme =
    theme
        |> OUI.Material.Theme.withRadiobutton
            (theme
                |> OUI.Material.Theme.radiobutton
                |> fn
            )


updateRadiobuttonMsg :
    (data -> OUI.Material.Theme.RadioButtonTheme -> OUI.Material.Theme.RadioButtonTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateRadiobuttonMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateRadiobuttonTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
editorChapter { theme } _ =
    let
        radiobuttonTheme : OUI.Material.Theme.RadioButtonTheme
        radiobuttonTheme =
            OUI.Material.Theme.radiobutton theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Radio Button" |> Material.text theme
        , ThemeEditor.slider theme
            (updateRadiobuttonMsg
                (\value p ->
                    { p | containerWidth = round value }
                )
            )
            "Container Width"
            ( 0, 100 )
            (toFloat radiobuttonTheme.containerWidth)
        , ThemeEditor.slider theme
            (updateRadiobuttonMsg
                (\value p ->
                    { p | containerHeight = round value }
                )
            )
            "Container Height"
            ( 0, 100 )
            (toFloat radiobuttonTheme.containerHeight)
        , ThemeEditor.slider theme
            (updateRadiobuttonMsg
                (\value p ->
                    { p | containerShape = round value }
                )
            )
            "Container Shape"
            ( 0, 100 )
            (toFloat radiobuttonTheme.containerShape)
        , ThemeEditor.slider theme
            (updateRadiobuttonMsg
                (\value p ->
                    { p | contentSize = round value }
                )
            )
            "Content Size"
            ( 0, 100 )
            (toFloat radiobuttonTheme.contentSize)
        , ThemeEditor.slider theme
            (updateRadiobuttonMsg
                (\value p ->
                    { p | stateLayerSize = round value }
                )
            )
            "State Layer Size"
            ( 0, 100 )
            (toFloat radiobuttonTheme.stateLayerSize)
        , ThemeEditor.slider theme
            (updateRadiobuttonMsg
                (\value p ->
                    { p | borderWidth = round value }
                )
            )
            "Border Width"
            ( 0, 100 )
            (toFloat radiobuttonTheme.borderWidth)
        , divider
        ]


radiobuttons : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
radiobuttons { theme } model =
    let
        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Element.row [ Element.spacing 30 ]
            [ Element.column [ Element.spacing 55, Element.padding 30 ]
                [ Element.none |> Element.el []
                , Text.titleSmall "Basic" |> Material.text theme
                , Text.titleSmall "Disabled" |> Material.text theme
                , Text.titleSmall "Error" |> Material.text theme
                ]
            , Element.column [ Element.spacing 30 ]
                [ Text.titleSmall "Unselected" |> Material.text theme
                , RadioButton.new
                    |> RadioButton.onChange (\_ -> OnClickBasicUnselect)
                    |> RadioButton.withSelected model.basicUnselect
                    |> Material.radiobutton theme []
                , RadioButton.new
                    |> RadioButton.disabled
                    |> RadioButton.withSelected False
                    |> Material.radiobutton theme []
                , RadioButton.new
                    |> RadioButton.onChange (\_ -> OnClickErrorUnselect)
                    |> RadioButton.withSelected model.errorUnselect
                    |> RadioButton.withColor OUI.Error
                    |> Material.radiobutton theme []
                ]
            , Element.column [ Element.spacing 30 ]
                [ Text.titleSmall "Selected" |> Material.text theme
                , RadioButton.new
                    |> RadioButton.onChange (\_ -> OnClickBasicSelect)
                    |> RadioButton.withSelected model.basicSelect
                    |> Material.radiobutton theme []
                , RadioButton.new
                    |> RadioButton.disabled
                    |> RadioButton.withSelected True
                    |> Material.radiobutton theme []
                , RadioButton.new
                    |> RadioButton.onChange (\_ -> OnClickErrorSelect)
                    |> RadioButton.withSelected model.errorSelect
                    |> RadioButton.withColor OUI.Error
                    |> Material.radiobutton theme []
                ]
            ]
        ]
        |> Element.map Explorer.bookMsg
