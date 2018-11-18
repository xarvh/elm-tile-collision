module Scene exposing (..)

import Circle
import Dict exposing (Dict)
import Game
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Obstacle
import Quad
import Set exposing (Set)
import TileCollision exposing (BlockerDirections, Tile, Vector)
import WebGL exposing (Entity, Mesh, Shader)


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
            Game.tilemap
                |> Dict.toList
                |> List.map (obstacleToEntity worldToViewport)
                |> List.concat

        dots =
            [ mob worldToViewport clickPosition (vec3 0 0 1)
            ]

        collisions =
            TileCollision.collisionsAlongX
                Game.hasBlockerAlong
                Game.mobWidth
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
                |> Mat4.scale3 Game.mobWidth Game.mobHeight 1
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
            Game.charToBlockers char

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
