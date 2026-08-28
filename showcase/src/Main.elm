module Main exposing (..)

import Browser
import ColorThemes
import GetStarted
import IcidassetMaterialIcons.Outlined as Outlined
import IcidassetMaterialIcons.Regular as Regular
import IcidassetMaterialIcons.Round as Round
import Landing
import Material.Icons.Types exposing (Coloring(..))
import OUI.Explorer as Explorer
import OUI.Material.Color
import OUI.Material.Theme as Theme exposing (Theme)
import OUI.Material.Typography
import OUI.Showcase as Showcase


main =
    Explorer.explorer
        |> Explorer.setColorTheme OUI.Material.Color.defaultTheme
        |> Explorer.addColorTheme ColorThemes.spring
        |> Explorer.addColorTheme ColorThemes.autumn
        |> Explorer.addColorTheme ColorThemes.summer
        |> Explorer.addColorTheme ColorThemes.sky
        |> Explorer.setTheme theme
        |> Explorer.addBook
            (Explorer.book "Elm Orus UI" |> Explorer.withStaticChapter Landing.view)
        |> Explorer.addBook
            (Explorer.book "Get Started"
                |> Explorer.withStaticChapter GetStarted.view
            )
        |> Showcase.addPages
        |> Explorer.category "Material Icons"
        |> Explorer.addBook Regular.book
        |> Explorer.addBook Outlined.book
        |> Explorer.addBook Round.book
        |> Explorer.finalize
        |> Browser.application


typescale : OUI.Material.Typography.Typescale
typescale =
    let
        base =
            Theme.defaultTypescale

        title =
            base.title

        titleMedium =
            title.medium
    in
    { base
        | title =
            { title
                | medium =
                    { titleMedium
                        | size =
                            --10
                            titleMedium.size
                    }
            }
    }


theme : Theme ()
theme =
    let
        base =
            Theme.defaultTheme

        button =
            Theme.button base

        buttonCommon =
            button.common
    in
    base
        |> Theme.withTypescale typescale
        |> Theme.withButton
            { button
                | common = buttonCommon

                --  | common = { buttonCommon | containerRadius = 8 }
            }
