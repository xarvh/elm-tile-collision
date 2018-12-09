module Game exposing (..)

import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Set exposing (Set)
import TileCollision exposing (BlockerDirections, Tile, Vector)


tileSize =
    16


worldSize =
    10


mobSize =
    { halfWidth = tileSize // 2
    , halfHeight = tileSize
    }


tilemapSrc =
    """
 #
 #   ^^    ====
###       ##
###############

"""



--




tileCenter : Tile -> Vec2
tileCenter tile =
    vec2 (toFloat tile.x + 0.5) (toFloat tile.y + 0.5)


vectorToVec2 : Vector -> Vec2
vectorToVec2 vector =
    vec2 (toFloat vector.x / toFloat tileSize) (toFloat vector.y / toFloat tileSize)


vec2ToVector : Vec2 -> Vector
vec2ToVector v =
  { x = round (Vec2.getX v * tileSize)
  , y = round (Vec2.getY v * tileSize)
  }







--


charToBlockers : Char -> BlockerDirections Bool
charToBlockers char =
    case char of
        '#' ->
            { positiveDeltaX = True
            , negativeDeltaX = True
            , positiveDeltaY = True
            , negativeDeltaY = True
            }

        '=' ->
            { positiveDeltaX = False
            , negativeDeltaX = False
            , positiveDeltaY = True
            , negativeDeltaY = True
            }

        '^' ->
            { positiveDeltaX = False
            , negativeDeltaX = False
            , positiveDeltaY = False
            , negativeDeltaY = True
            }

        _ ->
            { positiveDeltaX = False
            , negativeDeltaX = False
            , positiveDeltaY = False
            , negativeDeltaY = False
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
    { positiveDeltaX = getBlockers .positiveDeltaX
    , negativeDeltaX = getBlockers .negativeDeltaX
    , positiveDeltaY = getBlockers .positiveDeltaY
    , negativeDeltaY = getBlockers .negativeDeltaY
    }
