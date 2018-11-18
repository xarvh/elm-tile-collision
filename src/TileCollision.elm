module TileCollision exposing (..)

import List.Extra


type alias Vector =
    { x : Float
    , y : Float
    }


type alias Tile =
    { x : Int
    , y : Int
    }


type alias BlockerDirections a =
    {-

          _____
         |     |
       --|->   |   posX
         |_____|

          _____
         |     |
         |   <-|-- negX
         |_____|

          _____
         |     |
         |  A  |   posY
         |__|__|
            |

          __|__
         |  |  |
         |  v  |   negY
         |_____|

    -}
    { positiveX : a
    , negativeX : a
    , positiveY : a
    , negativeY : a
    }


type alias Collision =
    { point : Vector
    , tiles : List Tile
    }


vectorToContainingTile : Vector -> Tile
vectorToContainingTile { x, y } =
    { x = floor <| x + 0.5
    , y = floor <| y + 0.5
    }


coordinateToContainingTile : Float -> Int
coordinateToContainingTile x =
    floor (x + 0.5)


collisions : BlockerDirections (Int -> Int -> Bool) -> { width : Float, height : Float } -> Vector -> Vector -> Maybe Collision
collisions hasBlockerAlong size start end =
    Nothing


{-|

    Equation of the line connecting Start with End

    (x - Sx)(Ey - Sy) = (y - Sy)(Ex - Sx)

             S
              \
               \
                \
                 \
                  E

    1. Offset S and E by moving object bounding box width

    2. Ordered from start to end, find all points where the trajectory
      intersects offsetted tile lines

    3. Find the first intersection point where the blockers are within the bounding box

-}
collisionsAlongX : BlockerDirections (Int -> Int -> Bool) -> Float -> Vector -> Vector -> Maybe Collision
collisionsAlongX hasBlockerAlong height s e =
    let
        dx =
            e.x - s.x
    in
    if dx == 0 then
        Nothing
    else
        let
            trajectoryY x =
                (x - s.x) * (e.y - s.y) / dx + s.y

            startTile =
                vectorToContainingTile s

            endTile =
                vectorToContainingTile e

            halfHeight =
                height / 2

            ( sign, offset, hasBlocker ) =
                if dx > 0 then
                    ( 1, -0.5, hasBlockerAlong.positiveX )
                else
                    ( -1, 0.5, hasBlockerAlong.negativeX )
        in
        findFirstInRange
            sign
            startTile.x
            endTile.x
            (collidesWithX offset trajectoryY hasBlocker halfHeight)


findFirstInRange : Int -> Int -> Int -> (Int -> Maybe a) -> Maybe a
findFirstInRange sign start end f =
    let
        next =
            start + sign
    in
    case f next of
        Just v ->
            Just v

        Nothing ->
            if sign * (start - end) > 0 then
                Nothing
            else
                findFirstInRange sign next end f


collidesWithX : Float -> (Float -> Float) -> (Int -> Int -> Bool) -> Float -> Int -> Maybe Collision
collidesWithX collisionOffset trajectoryY hasBlockerAlongPositiveX halfHeight tileX =
    let
        collisionX =
            toFloat tileX + collisionOffset

        collisionY =
            trajectoryY collisionX

        maxTileY =
            collisionY + halfHeight |> coordinateToContainingTile

        minTileY =
            collisionY - halfHeight |> coordinateToContainingTile

        tileYsToMaybeCollision tileYs =
            if tileYs == [] then
                Nothing
            else
                Just
                    { point = Vector collisionX collisionY
                    , tiles = List.map (\tileY -> Tile tileX tileY) tileYs
                    }
    in
    List.range minTileY maxTileY
        |> List.filter (\tileY -> hasBlockerAlongPositiveX tileX tileY)
        |> tileYsToMaybeCollision


collisionsAlongY : BlockerDirections (Int -> Int -> Bool) -> Float -> Vector -> Vector -> Maybe Collision
collisionsAlongY hasBlockerAlong width s e =
    let
        flipBlockers : BlockerDirections (Int -> Int -> Bool)
        flipBlockers =
            { hasBlockerAlong
                | positiveX = \x y -> hasBlockerAlong.positiveY y x
                , negativeX = \x y -> hasBlockerAlong.negativeY y x
            }

        flipXY : { x : a, y : a } -> { x : a, y : a }
        flipXY { x, y } =
            { x = y, y = x }

        flipCollision : Collision -> Collision
        flipCollision { point, tiles } =
            { point = flipXY point
            , tiles = List.map flipXY tiles
            }
    in
    collisionsAlongX flipBlockers width (flipXY s) (flipXY e)
        |> Maybe.map flipCollision
