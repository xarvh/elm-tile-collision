module Scene exposing (..)

import Circle
import Dict exposing (Dict)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Obstacle
import Quad
import Set exposing (Set)
import TileCollision exposing (BlockerDirections, Tile, Vector)
import WebGL exposing (Entity, Mesh, Shader)


--


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



-- Periodic functions


periodLinear : Float -> Float -> Float -> Float
periodLinear time phase period =
    let
        t =
            time + phase * period

        n =
            t / period |> floor |> toFloat
    in
    t / period - n


periodHarmonic : Float -> Float -> Float -> Float
periodHarmonic time phase period =
    2 * pi * periodLinear time phase period |> sin



-- Model


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



-- Entities


type alias EntitiesArgs =
    { cameraToViewport : Mat4
    , mousePosition : Vec2
    , clickPosition : Vec2
    , time : Float
    }


entities : EntitiesArgs -> List Entity
entities { cameraToViewport, mousePosition, clickPosition, time } =
    let
        worldToViewport =
            cameraToViewport
                |> Mat4.scale3 0.2 0.2 1

        obsEntities =
            tilemap
                |> Dict.toList
                |> List.map (obstacleToEntity worldToViewport)
                |> List.concat

        dots =
            [ mob worldToViewport clickPosition (vec3 0 0 1)
            ]

        collisions =
            TileCollision.collisionsAlongX
                hasBlockerAlong
                mobWidth
                (Vec2.toRecord clickPosition)
                (Vec2.toRecord mousePosition)

        collisionEntities =
            case collisions of
                Nothing ->
                    []

                Just collision ->
                    collision.tiles
                        |> List.map (\tile -> dot worldToViewport (vec2 (toFloat tile.x) (toFloat tile.y)) (vec3 0 1 0))
                        |> (::) (mob worldToViewport (Vec2.fromRecord collision.point) (vec3 1 0 0))
    in
    List.concat
        [ obsEntities
        , dots
        , collisionEntities
        ]


mob : Mat4 -> Vec2 -> Vec3 -> Entity
mob worldToViewport position color =
    let
        { x, y } =
            Vec2.toRecord position

        entityToViewport =
            worldToViewport
                |> Mat4.translate3 x y 0
                |> Mat4.scale3 mobWidth mobHeight 1
    in
    Quad.entity entityToViewport color


dot : Mat4 -> Vec2 -> Vec3 -> Entity
dot worldToViewport position color =
    let
        { x, y } =
            Vec2.toRecord position

        entityToViewport =
            worldToViewport
                |> Mat4.translate3 x y 0
    in
    Circle.entity entityToViewport color


obstacleToEntity : Mat4 -> ( ( Int, Int ), Char ) -> List Entity
obstacleToEntity worldToViewport ( ( x, y ), char ) =
    let
        blockers =
            charToBlockers char

        anglesAndBlockers =
            [ ( .negativeY, 0 )
            , ( .positiveY, pi )
            , ( .positiveX, pi / 2 )
            , ( .negativeX, -pi / 2 )
            ]

        stuff ( getter, angle ) =
            if getter blockers then
                worldToViewport
                    |> Mat4.translate3 (toFloat x) (toFloat y) 0
                    |> Mat4.rotate angle (vec3 0 0 1)
                    |> Obstacle.entity
                    |> Just
            else
                Nothing
    in
    List.filterMap stuff anglesAndBlockers
