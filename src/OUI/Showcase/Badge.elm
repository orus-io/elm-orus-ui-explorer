module OUI.Showcase.Badge exposing (book)

import Element exposing (Element)
import OUI.Badge
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon
import OUI.Material as Material
import OUI.Material.Theme exposing (Theme)
import OUI.Text as Text


book : Explorer.Book themeExt () ()
book =
    Explorer.book "Badge"
        |> Explorer.withStaticChapter badges
        |> Explorer.withThemeEditor editorChapter


updateBadgeTheme :
    (OUI.Material.Theme.BadgeTheme -> OUI.Material.Theme.BadgeTheme)
    -> Theme themeExt
    -> Theme themeExt
updateBadgeTheme fn theme =
    theme
        |> OUI.Material.Theme.withBadge
            (theme
                |> OUI.Material.Theme.badge
                |> fn
            )


updateBadgeSmallMsg :
    (data
     ->
        { shape : Int
        , size : Int
        , pos : ( Int, Int )
        }
     ->
        { shape : Int
        , size : Int
        , pos : ( Int, Int )
        }
    )
    -> data
    -> Explorer.BookMsg themeExt msg
updateBadgeSmallMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateBadgeTheme
            (\b ->
                { b | small = fn value b.small }
            )
        )
        |> Explorer.sharedMsg


updateBadgeLargeMsg :
    (data
     ->
        { shape : Int
        , size : Int
        , padding : Int
        , textSize : Text.Size
        , textType : Text.Type
        , textColor : Text.Color
        , pos : ( Int, Int )
        }
     ->
        { shape : Int
        , size : Int
        , padding : Int
        , textSize : Text.Size
        , textType : Text.Type
        , textColor : Text.Color
        , pos : ( Int, Int )
        }
    )
    -> data
    -> Explorer.BookMsg themeExt msg
updateBadgeLargeMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateBadgeTheme
            (\b ->
                { b | large = fn value b.large }
            )
        )
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> () -> Element (Explorer.BookMsg themeExt msg)
editorChapter { theme } _ =
    let
        badgeTheme : OUI.Material.Theme.BadgeTheme
        badgeTheme =
            OUI.Material.Theme.badge theme

        divider : Element (Explorer.BookMsg themeExt msg)
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Small Badge" |> Material.text theme
        , ThemeEditor.slider theme
            (updateBadgeSmallMsg
                (\value small ->
                    { small | shape = round value }
                )
            )
            "Shape"
            ( 0, 50, 1 )
            (toFloat badgeTheme.small.shape)
        , ThemeEditor.slider theme
            (updateBadgeSmallMsg
                (\value small ->
                    { small | size = round value }
                )
            )
            "Size"
            ( 0, 50, 1 )
            (toFloat badgeTheme.small.size)
        , ThemeEditor.slider theme
            (updateBadgeSmallMsg
                (\value small ->
                    let
                        ( _, y ) =
                            small.pos
                    in
                    { small | pos = ( round value, y ) }
                )
            )
            "Pos X"
            ( 0, 50, 1 )
            (toFloat (Tuple.first badgeTheme.small.pos))
        , ThemeEditor.slider theme
            (updateBadgeSmallMsg
                (\value small ->
                    let
                        ( x, _ ) =
                            small.pos
                    in
                    { small | pos = ( x, round value ) }
                )
            )
            "Pos Y"
            ( 0, 50, 1 )
            (toFloat (Tuple.second badgeTheme.small.pos))
        , divider
        , Text.titleLarge "Large Badge" |> Material.text theme
        , ThemeEditor.slider theme
            (updateBadgeLargeMsg
                (\value large ->
                    { large | shape = round value }
                )
            )
            "Shape"
            ( 0, 50, 1 )
            (toFloat badgeTheme.large.shape)
        , ThemeEditor.slider theme
            (updateBadgeLargeMsg
                (\value large ->
                    { large | size = round value }
                )
            )
            "Size"
            ( 0, 50, 1 )
            (toFloat badgeTheme.large.size)
        , ThemeEditor.slider theme
            (updateBadgeLargeMsg
                (\value large ->
                    { large | padding = round value }
                )
            )
            "Padding"
            ( 0, 50, 1 )
            (toFloat badgeTheme.large.padding)
        , ThemeEditor.textType theme
            (updateBadgeLargeMsg
                (\type_ large ->
                    { large | textType = type_ }
                )
            )
            "Text Type"
            badgeTheme.large.textType
        , ThemeEditor.textSize theme
            (updateBadgeLargeMsg
                (\size large ->
                    { large | textSize = size }
                )
            )
            "Text Size"
            badgeTheme.large.textSize
        , ThemeEditor.slider theme
            (updateBadgeLargeMsg
                (\value large ->
                    let
                        ( _, y ) =
                            large.pos
                    in
                    { large | pos = ( round value, y ) }
                )
            )
            "Pos X"
            ( 0, 50, 1 )
            (toFloat (Tuple.first badgeTheme.large.pos))
        , ThemeEditor.slider theme
            (updateBadgeLargeMsg
                (\value large ->
                    let
                        ( x, _ ) =
                            large.pos
                    in
                    { large | pos = ( x, round value ) }
                )
            )
            "Pos Y"
            ( 0, 50, 1 )
            (toFloat (Tuple.second badgeTheme.large.pos))
        , divider
        ]


badges : Explorer.Shared themeExt -> Element msg
badges { theme } =
    let
        divider : Element msg
        divider =
            Divider.new |> Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Element.table
            [ Element.spacing 20 ]
            { data =
                [ { text = "small"
                  , badge = OUI.Badge.small
                  }
                , { text = "with text"
                  , badge = OUI.Badge.label "new"
                  }
                , { text = "with large text"
                  , badge = OUI.Badge.label "Lorem Ipsum"
                  }
                , { text = "with number"
                  , badge = OUI.Badge.number 10
                  }
                , { text = "with large number"
                  , badge = OUI.Badge.number 12348
                  }
                ]
            , columns =
                [ { header = Element.none
                  , width = Element.shrink
                  , view =
                        .text
                            >> Text.titleSmall
                            >> Material.text theme
                  }
                , { header = Element.none
                  , width = Element.shrink
                  , view =
                        \{ badge } ->
                            OUI.Icon.light_mode
                                |> Material.icon theme
                                    [ badge
                                        |> Material.badge theme []
                                    ]
                                |> Element.el []
                  }
                ]
            }
        ]
