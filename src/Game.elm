module Game
    exposing
        ( Tilemap
        , charToBlockers
        , hasBlockerAlong
        , mobHeight
        , mobWidth
        , tilemap
        )

import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Set exposing (Set)
import TileCollision exposing (BlockerDirections, Tile, Vector)


worldSize =
    10


mobWidth =
    0.5


mobHeight =
    1.0


tilemapSrc =
    """

     ^^    ====
###       ##
###############

"""



--


charToBlockers : Char -> BlockerDirections Bool
charToBlockers char =
    case char of
        '#' ->
            { positiveX = True
            , negativeX = True
            , positiveY = True
            , negativeY = True
            }

        '=' ->
            { positiveX = False
            , negativeX = False
            , positiveY = True
            , negativeY = True
            }

        '^' ->
            { positiveX = False
            , negativeX = False
            , positiveY = False
            , negativeY = True
            }

        _ ->
            { positiveX = False
            , negativeX = False
            , positiveY = False
            , negativeY = False
            }


type alias Tilemap =
    Dict ( Int, Int ) Char


tilemap : Tilemap
tilemap =
    tilemapSrc
        |> String.split "\n"
        |> List.indexedMap rowToTuple
        |> List.concat
        |> Dict.fromList


rowToTuple : Int -> String -> List ( ( Int, Int ), Char )
rowToTuple invertedY row =
    let
        y =
            3 - invertedY

        charToTuple index char =
            ( ( index - 8, y )
            , char
            )
    in
    row
        |> String.toList
        |> List.indexedMap charToTuple


getBlockers : (BlockerDirections Bool -> Bool) -> Int -> Int -> Bool
getBlockers getter x y =
    case Dict.get ( x, y ) tilemap of
        Nothing ->
            False

        Just char ->
            char
                |> charToBlockers
                |> getter


hasBlockerAlong : BlockerDirections (Int -> Int -> Bool)
hasBlockerAlong =
    { positiveX = getBlockers .positiveX
    , negativeX = getBlockers .negativeX
    , positiveY = getBlockers .positiveY
    , negativeY = getBlockers .negativeY
    }
