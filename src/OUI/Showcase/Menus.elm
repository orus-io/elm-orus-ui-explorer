module OUI.Showcase.Menus exposing (book, chapter)

import Element exposing (Element)
import OUI.Divider as Divider
import OUI.Explorer as Explorer
import OUI.Explorer.ThemeEditor as ThemeEditor
import OUI.Icon
import OUI.Material
import OUI.Material.Theme exposing (Theme)
import OUI.Menu as Menu
import OUI.Text as Text


book : Explorer.Book themeExt () ()
book =
    Explorer.book "Menu"
        |> Explorer.withChapter chapter
        |> Explorer.withThemeEditor editorChapter


updateMenuTheme :
    (OUI.Material.Theme.MenuTheme -> OUI.Material.Theme.MenuTheme)
    -> Theme themeExt
    -> Theme themeExt
updateMenuTheme fn theme =
    theme
        |> OUI.Material.Theme.withMenu
            (theme
                |> OUI.Material.Theme.menu
                |> fn
            )


updateMenuMsg :
    (data -> OUI.Material.Theme.MenuTheme -> OUI.Material.Theme.MenuTheme)
    -> data
    -> Explorer.BookMsg themeExt msg
updateMenuMsg fn value =
    Explorer.updateCurrentThemeMsg
        (updateMenuTheme (fn value))
        |> Explorer.sharedMsg


editorChapter : Explorer.Shared themeExt -> () -> Element (Explorer.BookMsg themeExt msg)
editorChapter { theme } _ =
    let
        menuTheme : OUI.Material.Theme.MenuTheme
        menuTheme =
            OUI.Material.Theme.menu theme

        divider : Element (Explorer.BookMsg themeExt msg)
        divider =
            Divider.new |> OUI.Material.divider theme []
    in
    Element.column [ Element.spacing 30 ]
        [ divider
        , Text.titleLarge "Menu" |> OUI.Material.text theme
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | radius = round value }
                )
            )
            "Radius"
            ( 0, 50, 1 )
            (toFloat menuTheme.radius)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | topBottomPadding = round value }
                )
            )
            "Top/Bottom Padding"
            ( 0, 50, 1 )
            (toFloat menuTheme.topBottomPadding)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | leftRightPadding = round value }
                )
            )
            "Left/Right Padding"
            ( 0, 50, 1 )
            (toFloat menuTheme.leftRightPadding)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | paddingWithinItem = round value }
                )
            )
            "Padding Within Item"
            ( 0, 50, 1 )
            (toFloat menuTheme.paddingWithinItem)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | itemHeight = round value }
                )
            )
            "Item Height"
            ( 0, 100, 1 )
            (toFloat menuTheme.itemHeight)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | iconSize = round value }
                )
            )
            "Icon Size"
            ( 0, 50, 1 )
            (toFloat menuTheme.iconSize)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | minWidth = round value }
                )
            )
            "Min Width"
            ( 0, 300, 1 )
            (toFloat menuTheme.minWidth)
        , ThemeEditor.slider theme
            (updateMenuMsg
                (\value m ->
                    { m | maxWidth = round value }
                )
            )
            "Max Width"
            ( 0, 500, 1 )
            (toFloat menuTheme.maxWidth)
        , divider
        ]


chapter : Explorer.Shared themeExt -> () -> Element (Explorer.BookMsg themeExt msg)
chapter shared _ =
    Element.wrappedRow [ Element.spacing 50 ]
        [ Menu.new identity
            |> Menu.addItems [ "one", "two", "three" ]
            |> Menu.onClick (\i -> Explorer.logEvent <| "clicked menu1/" ++ i)
            |> OUI.Material.menu shared.theme []
        , Menu.new identity
            |> Menu.onClick (\i -> Explorer.logEvent <| "clicked menu2/" ++ i)
            |> Menu.withIcon
                (\i ->
                    if i == "two" then
                        Just OUI.Icon.clear

                    else
                        Nothing
                )
            |> Menu.withTrailingIcon
                (\i ->
                    if i == "one" then
                        Just OUI.Icon.check

                    else
                        Nothing
                )
            |> Menu.addItems [ "one", "two", "three" ]
            |> Menu.addDivider
            |> Menu.addItems [ "a longer entry" ]
            |> OUI.Material.menu shared.theme []
        ]
