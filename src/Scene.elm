module Scene exposing (..)

import Circle
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import Obstacle
import Quad
import Set exposing (Set)
import TileCollision exposing (Tile, Vector)
import WebGL exposing (Entity, Mesh, Shader)


--


mobWidth =
    0.5


mobHeight =
    1.0



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


obstacles : List Tile
obstacles =
    [ { x = 0, y = 0 }
    , { x = 0, y = -1 }
    , { x = 1, y = 1 }
    , { x = -4, y = -4 }
    , { x = 4, y = 4 }
    ]



hasBlockerAlong =
  { positiveX = (\tileX tileY -> List.any (\tile -> tile == Tile tileX tileY) obstacles)
  , negativeX = (\tileX tileY -> List.any (\tile -> tile == Tile tileX tileY) obstacles)
  , positiveY = (\tileX tileY -> List.any (\tile -> tile == Tile tileX tileY) obstacles)
  , negativeY = (\tileX tileY -> List.any (\tile -> tile == Tile tileX tileY) obstacles)
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
            obstacles
                |> List.map (obstacleToEntity worldToViewport)

        dots =
            [ mob worldToViewport clickPosition (vec3 0 0 1)
            ]


        collisions =
            TileCollision.collisionsAlongY
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


obstacleToEntity : Mat4 -> Tile -> Entity
obstacleToEntity worldToViewport { x, y } =
    worldToViewport
        |> Mat4.translate3 (toFloat x) (toFloat y) 0
        |> Mat4.rotate (pi / 2) (vec3 0 0 1)
        |> Obstacle.entity
