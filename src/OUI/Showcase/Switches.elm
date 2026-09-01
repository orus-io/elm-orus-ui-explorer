module OUI.Showcase.Switches exposing (Model, Msg, book)

import Dict exposing (Dict)
import Effect
import Element exposing (Element)
import OUI
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon exposing (check, clear)
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.Switch as Switch
import OUI.Text as Text


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Switches"
        { init =
            \_ ->
                { switches = Dict.empty }
                    |> Effect.withNone
        , update =
            \_ (SetSwitch name selected) model ->
                { model
                    | switches = Dict.insert name selected model.switches
                }
                    |> Effect.withNone
        , subscriptions = \_ _ -> Sub.none
        }
        |> Explorer.withChapter checkbox
        |> Explorer.withThemeEditor editorChapter


type alias Model =
    { switches : Dict String Bool
    }


type Msg
    = SetSwitch String Bool


updateSwitchTheme :
    (OUI.Material.Theme.SwitchTheme -> OUI.Material.Theme.SwitchTheme)
    -> Theme themeExt
    -> Theme themeExt
updateSwitchTheme fn theme =
    theme
        |> OUI.Material.Theme.withSwitch
            (theme
                |> OUI.Material.Theme.switch
                |> fn
            )


updateSwitchMsg :
    (data -> OUI.Material.Theme.SwitchTheme -> OUI.Material.Theme.SwitchTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateSwitchMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateSwitchTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
editorChapter { theme } _ =
    let
        switchTheme : OUI.Material.Theme.SwitchTheme
        switchTheme =
            OUI.Material.Theme.switch theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Track" |> Material.text theme
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        track : { height : Int, width : Int, outlineWidth : Int, corner : Int }
                        track =
                            sw.track
                    in
                    { sw | track = { track | height = round value } }
                )
            )
            "Height"
            ( 0, 100 )
            (toFloat switchTheme.track.height)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        track : { height : Int, width : Int, outlineWidth : Int, corner : Int }
                        track =
                            sw.track
                    in
                    { sw | track = { track | width = round value } }
                )
            )
            "Width"
            ( 0, 100 )
            (toFloat switchTheme.track.width)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        track : { height : Int, width : Int, outlineWidth : Int, corner : Int }
                        track =
                            sw.track
                    in
                    { sw | track = { track | outlineWidth = round value } }
                )
            )
            "Outline Width"
            ( 0, 20 )
            (toFloat switchTheme.track.outlineWidth)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        track : { height : Int, width : Int, outlineWidth : Int, corner : Int }
                        track =
                            sw.track
                    in
                    { sw | track = { track | corner = round value } }
                )
            )
            "Corner"
            ( 0, 50 )
            (toFloat switchTheme.track.corner)
        , divider
        , Text.titleLarge "Thumb" |> Material.text theme
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        thumb : { size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int } }
                        thumb =
                            sw.thumb

                        size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int }
                        size =
                            thumb.size
                    in
                    { sw | thumb = { thumb | size = { size | unselected = round value } } }
                )
            )
            "Size Unselected"
            ( 0, 50 )
            (toFloat switchTheme.thumb.size.unselected)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        thumb : { size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int } }
                        thumb =
                            sw.thumb

                        size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int }
                        size =
                            thumb.size
                    in
                    { sw | thumb = { thumb | size = { size | withIcon = round value } } }
                )
            )
            "Size With Icon"
            ( 0, 50 )
            (toFloat switchTheme.thumb.size.withIcon)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        thumb : { size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int } }
                        thumb =
                            sw.thumb

                        size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int }
                        size =
                            thumb.size
                    in
                    { sw | thumb = { thumb | size = { size | selected = round value } } }
                )
            )
            "Size Selected"
            ( 0, 50 )
            (toFloat switchTheme.thumb.size.selected)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        thumb : { size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int } }
                        thumb =
                            sw.thumb

                        size : { unselected : Int, withIcon : Int, selected : Int, pressed : Int }
                        size =
                            thumb.size
                    in
                    { sw | thumb = { thumb | size = { size | pressed = round value } } }
                )
            )
            "Size Pressed"
            ( 0, 50 )
            (toFloat switchTheme.thumb.size.pressed)
        , divider
        , Text.titleLarge "State Layer" |> Material.text theme
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    { sw | stateLayer = { size = round value } }
                )
            )
            "Size"
            ( 0, 100 )
            (toFloat switchTheme.stateLayer.size)
        , divider
        , Text.titleLarge "Icon" |> Material.text theme
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        icon : { sizeUnselected : Int, sizeSelected : Int }
                        icon =
                            sw.icon
                    in
                    { sw | icon = { icon | sizeUnselected = round value } }
                )
            )
            "Size Unselected"
            ( 0, 50 )
            (toFloat switchTheme.icon.sizeUnselected)
        , ThemeEditor.slider theme
            (updateSwitchMsg
                (\value sw ->
                    let
                        icon : { sizeUnselected : Int, sizeSelected : Int }
                        icon =
                            sw.icon
                    in
                    { sw | icon = { icon | sizeSelected = round value } }
                )
            )
            "Size Selected"
            ( 0, 50 )
            (toFloat switchTheme.icon.sizeSelected)
        , divider
        ]


checkbox : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
checkbox { theme } { switches } =
    let
        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30, Element.width <| Element.px 300 ]
        [ divider
        , Element.row [ Element.spacing 30, Element.width Element.fill ]
            [ Text.titleMedium "Primary color" |> Material.text theme
            , Switch.new (Dict.get "primary" switches |> Maybe.withDefault True)
                |> Switch.onChange (Explorer.bookMsg << SetSwitch "primary")
                |> Material.switch theme [ Element.alignRight ]
            ]
        , Element.row [ Element.spacing 30, Element.width Element.fill ]
            [ Text.titleMedium "Disabled" |> Material.text theme
            , Switch.new (Dict.get "primary" switches |> Maybe.withDefault True)
                |> Material.switch theme [ Element.alignRight ]
            ]
        , Element.row [ Element.spacing 30, Element.width Element.fill ]
            [ Text.titleMedium "With icon" |> Material.text theme
            , Switch.new (Dict.get "icons" switches |> Maybe.withDefault True)
                |> Switch.onChange (Explorer.bookMsg << SetSwitch "icons")
                |> Switch.withIconUnselected clear
                |> Switch.withIconSelected check
                |> Material.switch theme [ Element.alignRight ]
            ]
        , Element.row [ Element.spacing 30, Element.width Element.fill ]
            [ Text.titleMedium "Secondary color" |> Material.text theme
            , Switch.new (Dict.get "secondary" switches |> Maybe.withDefault True)
                |> Switch.withColor OUI.Secondary
                |> Switch.onChange (Explorer.bookMsg << SetSwitch "secondary")
                |> Material.switch theme [ Element.alignRight ]
            ]
        , Element.row [ Element.spacing 30, Element.width Element.fill ]
            [ Text.titleMedium "Error color" |> Material.text theme
            , Switch.new (Dict.get "error" switches |> Maybe.withDefault True)
                |> Switch.onChange (Explorer.bookMsg << SetSwitch "error")
                |> Switch.withColor OUI.Error
                |> Material.switch theme [ Element.alignRight ]
            ]
        ]
