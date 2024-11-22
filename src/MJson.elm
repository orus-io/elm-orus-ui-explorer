module MJson exposing (decodeColorTheme, encodeColorTheme)

import Bitwise
import Color
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import OUI.Material.Color as Color


encodeColorTheme : Color.Theme -> Json.Encode.Value
encodeColorTheme theme =
    Json.Encode.object
        [ ( "name", Json.Encode.string theme.name )
        , ( "description", Json.Encode.string theme.description )
        , ( "keyColors", encodeKeyColors theme.keyColors )
        , ( "schemes"
          , Json.Encode.object
                [ ( "light", encodeColorScheme theme.schemes.light )
                , ( "dark", encodeColorScheme theme.schemes.dark )
                ]
          )
        ]


decodeColorTheme : Json.Decode.Decoder Color.Theme
decodeColorTheme =
    Json.Decode.map4 Color.Theme
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "keyColors" decodeKeyColors)
        (Json.Decode.field "schemes"
            (Json.Decode.map2
                (\light dark ->
                    { light = light
                    , dark = dark
                    }
                )
                (Json.Decode.field "light" decodeColorScheme)
                (Json.Decode.field "dark" decodeColorScheme)
            )
        )


encodeColor : Color.Color -> Json.Encode.Value
encodeColor color =
    color
        |> toHex
        |> (\{ hex, alpha } ->
                hex
                    ++ (alpha
                            * 255
                            |> round
                            |> int255ToHex
                       )
           )
        |> Json.Encode.string


decodeColor : Json.Decode.Decoder Color.Color
decodeColor =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case colorFromHex s of
                    Nothing ->
                        Json.Decode.fail ("Invalid color" ++ s)

                    Just c ->
                        Json.Decode.succeed c
            )


encodeKeyColors : Color.KeyColors -> Json.Encode.Value
encodeKeyColors keyColors =
    Json.Encode.object
        [ ( "primary", encodeColor keyColors.primary )
        , ( "secondary", encodeColor keyColors.secondary )
        , ( "tertiary", encodeColor keyColors.tertiary )
        , ( "error", encodeColor keyColors.error )
        , ( "neutral", encodeColor keyColors.neutral )
        , ( "neutralVariant", encodeColor keyColors.neutralVariant )
        ]


decodeKeyColors : Json.Decode.Decoder Color.KeyColors
decodeKeyColors =
    Json.Decode.map6 Color.KeyColors
        (Json.Decode.field "primary" decodeColor)
        (Json.Decode.field "secondary" decodeColor)
        (Json.Decode.field "tertiary" decodeColor)
        (Json.Decode.field "error" decodeColor)
        (Json.Decode.field "neutral" decodeColor)
        (Json.Decode.field "neutralVariant" decodeColor)


encodeColorScheme : Color.Scheme -> Json.Encode.Value
encodeColorScheme scheme =
    Json.Encode.object
        [ ( "keyColors", encodeKeyColors scheme.keyColors )
        , ( "primary", encodeColor scheme.primary )
        , ( "primaryContainer", encodeColor scheme.primaryContainer )
        , ( "onPrimary", encodeColor scheme.onPrimary )
        , ( "onPrimaryContainer", encodeColor scheme.onPrimaryContainer )
        , ( "inversePrimary", encodeColor scheme.inversePrimary )
        , ( "secondary", encodeColor scheme.secondary )
        , ( "secondaryContainer", encodeColor scheme.secondaryContainer )
        , ( "onSecondary", encodeColor scheme.onSecondary )
        , ( "onSecondaryContainer", encodeColor scheme.onSecondaryContainer )
        , ( "tertiary", encodeColor scheme.tertiary )
        , ( "tertiaryContainer", encodeColor scheme.tertiaryContainer )
        , ( "onTertiary", encodeColor scheme.onTertiary )
        , ( "onTertiaryContainer", encodeColor scheme.onTertiaryContainer )
        , ( "surface", encodeColor scheme.surface )
        , ( "surfaceDim", encodeColor scheme.surfaceDim )
        , ( "surfaceBright", encodeColor scheme.surfaceBright )
        , ( "surfaceContainerLowest", encodeColor scheme.surfaceContainerLowest )
        , ( "surfaceContainerLow", encodeColor scheme.surfaceContainerLow )
        , ( "surfaceContainer", encodeColor scheme.surfaceContainer )
        , ( "surfaceContainerHigh", encodeColor scheme.surfaceContainerHigh )
        , ( "surfaceContainerHighest", encodeColor scheme.surfaceContainerHighest )
        , ( "surfaceVariant", encodeColor scheme.surfaceVariant )
        , ( "onSurface", encodeColor scheme.onSurface )
        , ( "onSurfaceVariant", encodeColor scheme.onSurfaceVariant )
        , ( "inverseSurface", encodeColor scheme.inverseSurface )
        , ( "inverseOnSurface", encodeColor scheme.inverseOnSurface )
        , ( "background", encodeColor scheme.background )
        , ( "onBackground", encodeColor scheme.onBackground )
        , ( "error", encodeColor scheme.error )
        , ( "errorContainer", encodeColor scheme.errorContainer )
        , ( "onError", encodeColor scheme.onError )
        , ( "onErrorContainer", encodeColor scheme.onErrorContainer )
        , ( "outline", encodeColor scheme.outline )
        , ( "outlineVariant", encodeColor scheme.outlineVariant )
        , ( "shadow", encodeColor scheme.shadow )
        , ( "surfaceTint", encodeColor scheme.surfaceTint )
        , ( "scrim", encodeColor scheme.scrim )
        ]


decodeColorScheme : Json.Decode.Decoder Color.Scheme
decodeColorScheme =
    Json.Decode.succeed Color.Scheme
        |> required "keyColors" decodeKeyColors
        |> required "primary" decodeColor
        |> required "primaryContainer" decodeColor
        |> required "onPrimary" decodeColor
        |> required "onPrimaryContainer" decodeColor
        |> required "inversePrimary" decodeColor
        |> required "secondary" decodeColor
        |> required "secondaryContainer" decodeColor
        |> required "onSecondary" decodeColor
        |> required "onSecondaryContainer" decodeColor
        |> required "tertiary" decodeColor
        |> required "tertiaryContainer" decodeColor
        |> required "onTertiary" decodeColor
        |> required "onTertiaryContainer" decodeColor
        |> required "surface" decodeColor
        |> required "surfaceDim" decodeColor
        |> required "surfaceBright" decodeColor
        |> required "surfaceContainerLowest" decodeColor
        |> required "surfaceContainerLow" decodeColor
        |> required "surfaceContainer" decodeColor
        |> required "surfaceContainerHigh" decodeColor
        |> required "surfaceContainerHighest" decodeColor
        |> required "surfaceVariant" decodeColor
        |> required "onSurface" decodeColor
        |> required "onSurfaceVariant" decodeColor
        |> required "inverseSurface" decodeColor
        |> required "inverseOnSurface" decodeColor
        |> required "background" decodeColor
        |> required "onBackground" decodeColor
        |> required "error" decodeColor
        |> required "errorContainer" decodeColor
        |> required "onError" decodeColor
        |> required "onErrorContainer" decodeColor
        |> required "outline" decodeColor
        |> required "outlineVariant" decodeColor
        |> required "shadow" decodeColor
        |> required "surfaceTint" decodeColor
        |> required "scrim" decodeColor


{-| Returns a color represented by a valid 3- or 6-digit RGB hex string
or a 4- or 8-digit RGBA hex string.
String may (but are not required to) start with a `#` character.
Hex digits in the string may be either uppercase or lowercase.

If the input string is not a valid hex string, it will return `Nothing`.

    fromHex "#Ac3" --> Just (Color.rgb255 0xAA 0xCC 0x33)

    fromHex "ffe4e1" --> Just (Color.rgb255 0xFF 0xE4 0xE1)

    fromHex "#00ff00FF" --> Just (Color.rgba 0.0 1.0 0.0 1.0)

    fromHex "**purple**" --> Nothing

-}
colorFromHex : String -> Maybe Color.Color
colorFromHex hexString =
    case String.toList hexString of
        [ '#', r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ '#', r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ '#', r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ '#', r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        _ ->
            Nothing


fromHex8 : ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Maybe Color.Color
fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    Maybe.map4
        (\r g b a ->
            Color.rgba
                (toFloat r / 255)
                (toFloat g / 255)
                (toFloat b / 255)
                (toFloat a / 255)
        )
        (hex2ToInt r1 r2)
        (hex2ToInt g1 g2)
        (hex2ToInt b1 b2)
        (hex2ToInt a1 a2)


hex2ToInt : Char -> Char -> Maybe Int
hex2ToInt c1 c2 =
    Maybe.map2 (\v1 v2 -> Bitwise.shiftLeftBy 4 v1 + v2) (hexToInt c1) (hexToInt c2)


hexToInt : Char -> Maybe Int
hexToInt char =
    case Char.toLower char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


{-| This function will convert a color to a 6-digit hexadecimal string in the format `#rrggbb`.

NOTE: If you want to use the resulting string with CSS, you should instead use [`toCssString`](#toCssString),
which will represent the color more accurately, and preserve the alpha component.

-}
toHex : Color.Color -> { hex : String, alpha : Float }
toHex c =
    let
        components =
            Color.toRgba c
    in
    { hex =
        [ components.red, components.green, components.blue ]
            |> List.map ((*) 255)
            |> List.map round
            |> List.map int255ToHex
            |> String.concat
            |> (++) "#"
    , alpha = components.alpha
    }


int255ToHex : Int -> String
int255ToHex n =
    if n < 0 then
        "00"

    else if n > 255 then
        "ff"

    else
        unsafeInt255Digits n
            |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
            |> (\( a, b ) -> String.cons a (String.cons b ""))


unsafeInt255Digits : Int -> ( Int, Int )
unsafeInt255Digits n =
    let
        digit1 =
            n // 16

        digit0 =
            if digit1 /= 0 then
                modBy (digit1 * 16) n

            else
                n
    in
    ( digit1, digit0 )


unsafeIntToChar : Int -> Char
unsafeIntToChar i =
    if i < 10 then
        String.fromInt i
            |> String.uncons
            |> Maybe.map Tuple.first
            |> Maybe.withDefault '0'

    else
        case i of
            10 ->
                'a'

            11 ->
                'b'

            12 ->
                'c'

            13 ->
                'd'

            14 ->
                'e'

            15 ->
                'f'

            _ ->
                '0'
