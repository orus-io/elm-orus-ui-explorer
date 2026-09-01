module OUI.Showcase.Tabs exposing (Model, Msg, book)

import Effect exposing (Effect)
import Element exposing (Element)
import OUI.Badge as Badge exposing (Badge)
import OUI.Divider as Divider
import OUI.Explorer as Explorer exposing (withChapter)
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon as Icon exposing (Icon)
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.Tabs
import OUI.Text as Text


type alias Entry =
    { label : String
    , icon : Maybe Icon
    , badge : Maybe Badge
    }


entries : List ( Int, Entry )
entries =
    [ ( 0, { label = "Hi", icon = Just Icon.clear, badge = Just <| Badge.number 5 } )
    , ( 1, { label = "There", icon = Just Icon.light_mode, badge = Nothing } )
    , ( 2, { label = "Youpi", icon = Nothing, badge = Just Badge.small } )
    , ( 3, { label = "Trala", icon = Just Icon.dark_mode, badge = Just <| Badge.label "la" } )
    ]


type alias Model =
    { primarySelected : Int
    , secondarySelected : Int
    }


type Msg
    = OnClickPrimary Int
    | OnClickSecondary Int


book : Explorer.Book themeExt Model Msg
book =
    Explorer.statefulBook "Tabs"
        { init = \_ -> { primarySelected = 0, secondarySelected = 0 } |> Effect.withNone
        , update = update
        , subscriptions = \_ _ -> Sub.none
        }
        |> withChapter tabs
        |> Explorer.withThemeEditor editorChapter


tabs : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
tabs { theme } model =
    let
        primary : OUI.Tabs.Tabs Int Entry Msg
        primary =
            OUI.Tabs.new .label OnClickPrimary
                |> OUI.Tabs.withItems entries
                |> OUI.Tabs.withIcon .icon
                |> OUI.Tabs.withBadge .badge
                |> OUI.Tabs.withSelected model.primarySelected

        secondary : OUI.Tabs.Tabs Int Entry Msg
        secondary =
            OUI.Tabs.new .label OnClickSecondary
                |> OUI.Tabs.withItems entries
                |> OUI.Tabs.withIcon .icon
                |> OUI.Tabs.withBadge .badge
                |> OUI.Tabs.withSelected model.secondarySelected
                |> OUI.Tabs.secondary
    in
    Element.column [ Element.spacing 30, Element.padding 50 ]
        [ Text.titleLarge "Primary tabs" |> Material.text theme
        , primary
            |> Material.tabs theme [ Element.width <| Element.px 500 ]
        , Text.titleLarge "Secondary tabs" |> Material.text theme
        , secondary
            |> Material.tabs theme [ Element.width <| Element.px 500 ]
        ]
        |> Element.map Explorer.bookMsg


update : Explorer.Shared themeExt -> Msg -> Model -> ( Model, Effect shared msg )
update _ msg model =
    case msg of
        OnClickPrimary key ->
            { model | primarySelected = key }
                |> Effect.withNone

        OnClickSecondary key ->
            { model | secondarySelected = key }
                |> Effect.withNone


updateTabsTheme :
    (OUI.Material.Theme.TabsTheme -> OUI.Material.Theme.TabsTheme)
    -> Theme themeExt
    -> Theme themeExt
updateTabsTheme fn theme =
    theme
        |> OUI.Material.Theme.withTabs
            (theme
                |> OUI.Material.Theme.tabs
                |> fn
            )


updateTabsMsg :
    (data -> OUI.Material.Theme.TabsTheme -> OUI.Material.Theme.TabsTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateTabsMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateTabsTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> Model -> Element (Explorer.BookMsg themeExt Msg)
editorChapter { theme } _ =
    let
        tabsTheme : OUI.Material.Theme.TabsTheme
        tabsTheme =
            OUI.Material.Theme.tabs theme

        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Primary" |> Material.text theme
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    let
                        primary : { containerHeight : Int, activeIndicatorHeight : Int, activeIndicatorWidth : Int }
                        primary =
                            t.primary
                    in
                    { t | primary = { primary | containerHeight = round value } }
                )
            )
            "Container Height"
            ( 0, 150 )
            (toFloat tabsTheme.primary.containerHeight)
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    let
                        primary : { containerHeight : Int, activeIndicatorHeight : Int, activeIndicatorWidth : Int }
                        primary =
                            t.primary
                    in
                    { t | primary = { primary | activeIndicatorHeight = round value } }
                )
            )
            "Active Indicator Height"
            ( 0, 20 )
            (toFloat tabsTheme.primary.activeIndicatorHeight)
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    let
                        primary : { containerHeight : Int, activeIndicatorHeight : Int, activeIndicatorWidth : Int }
                        primary =
                            t.primary
                    in
                    { t | primary = { primary | activeIndicatorWidth = round value } }
                )
            )
            "Active Indicator Width"
            ( 0, 100 )
            (toFloat tabsTheme.primary.activeIndicatorWidth)
        , divider
        , Text.titleLarge "Secondary" |> Material.text theme
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    let
                        secondary : { containerHeight : Int, activeIndicatorHeight : Int }
                        secondary =
                            t.secondary
                    in
                    { t | secondary = { secondary | containerHeight = round value } }
                )
            )
            "Container Height"
            ( 0, 150 )
            (toFloat tabsTheme.secondary.containerHeight)
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    let
                        secondary : { containerHeight : Int, activeIndicatorHeight : Int }
                        secondary =
                            t.secondary
                    in
                    { t | secondary = { secondary | activeIndicatorHeight = round value } }
                )
            )
            "Active Indicator Height"
            ( 0, 20 )
            (toFloat tabsTheme.secondary.activeIndicatorHeight)
        , divider
        , Text.titleLarge "Padding" |> Material.text theme
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    { t | paddingBetweenIconAndText = round value }
                )
            )
            "Between Icon And Text"
            ( 0, 50 )
            (toFloat tabsTheme.paddingBetweenIconAndText)
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    { t | paddingBetweenInlineIconAndText = round value }
                )
            )
            "Between Inline Icon And Text"
            ( 0, 50 )
            (toFloat tabsTheme.paddingBetweenInlineIconAndText)
        , ThemeEditor.slider theme
            (updateTabsMsg
                (\value t ->
                    { t | paddingBetweenInlineTextAndBadge = round value }
                )
            )
            "Between Inline Text And Badge"
            ( 0, 50 )
            (toFloat tabsTheme.paddingBetweenInlineTextAndBadge)
        , divider
        , Text.titleLarge "Text" |> Material.text theme
        , ThemeEditor.textSize theme
            (updateTabsMsg
                (\size t ->
                    let
                        text : { size : Text.Size, type_ : Text.Type }
                        text =
                            t.text
                    in
                    { t | text = { text | size = size } }
                )
            )
            "Text Size"
            tabsTheme.text.size
        , ThemeEditor.textType theme
            (updateTabsMsg
                (\type_ t ->
                    let
                        text : { size : Text.Size, type_ : Text.Type }
                        text =
                            t.text
                    in
                    { t | text = { text | type_ = type_ } }
                )
            )
            "Text Type"
            tabsTheme.text.type_
        , divider
        ]
