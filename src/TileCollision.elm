module TileCollision
    exposing
        ( Args
        , BlockerDirections
        , Collision
        , Tile
        , Vector
        , collide
        )

import List.Extra


type alias Pixels =
    Int


type alias Tiles =
    Int


type alias Vector =
    { x : Pixels
    , y : Pixels
    }


type alias Tile =
    { x : Tiles
    , y : Tiles
    }


type alias Size =
    { halfWidth : Int
    , halfHeight : Int
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
    { positiveDeltaX : a
    , negativeDeltaX : a
    , positiveDeltaY : a
    , negativeDeltaY : a
    }


type alias Args =
    { hasBlockerAlong : BlockerDirections (Int -> Int -> Bool)
    , tileSize : Int
    , mobSize : Size
    , start : Vector
    , end : Vector
    }


type alias Collision =
    { point : Vector
    , fix : Vector
    , tiles : List Tile
    }



--


squaredDistance : Vector -> Vector -> Int
squaredDistance a b =
    let
        dx =
            a.x - b.x

        dy =
            a.y - b.y
    in
    dx * dx + dy * dy



--
-- Trace all tiles swept by an horizontal movement ---------------------------
--


tileCenterInPixels : Pixels -> Tiles -> Pixels
tileCenterInPixels tileSizeInPixels tiles =
    tiles * tileSizeInPixels + tileSizeInPixels // 2


{-| Takes a tile only if it can block movement
(If the mob is already in a tile, that tile can't block it)

      [tile0] [tile1] [tile2]
                 ^       ^
                 s ----- e    (Take only tile2, ignore tile1)

      [tile0] [tile1] [tile2]
                 ^       ^
                 e ----- s    (Take only tile1, ignore tile2)

-}
tilesRangeAccountingForMovement : Int -> Int -> Int -> List Int
tilesRangeAccountingForMovement tileSize start end =
    -- all arguments are in pixels
    if start < end then
        List.range
            (toFloat start / toFloat tileSize |> ceiling)
            (toFloat (end - 1) / toFloat tileSize |> floor)
    else
        List.range
            (toFloat end / toFloat tileSize |> floor)
            (toFloat (start - tileSize) / toFloat tileSize |> floor)


{-| Take a tile if the point is inside it

      [tile0] [tile1] [tile2]
                 ^       ^
                 s ----- e    (Take both tile1 and tile 2)

      [tile0] [tile1] [tile2]
             ^       ^
             s ----- e    (Take only tile1)

-}
tilesRangeInclusive : Int -> Int -> Int -> List Int
tilesRangeInclusive tileSize start end =
    let
        mi =
            min start end

        ma =
            max start end
    in
    List.range
        (toFloat mi / toFloat tileSize |> floor)
        (toFloat (ma - 1) / toFloat tileSize |> floor)


sweepAlongX : Args -> List Collision
sweepAlongX { tileSize, mobSize, start, end } =
    if end.x == start.x then
        []
    else
        let
            { halfWidth, halfHeight } =
                mobSize

            sign =
                if end.x > start.x then
                    1
                else
                    -1

            s =
                { start | x = start.x + halfWidth * sign }

            e =
                { end | x = end.x + halfWidth * sign }

            ( xs, ys ) =
                ( toFloat s.x, toFloat s.y )

            ( xe, ye ) =
                ( toFloat e.x, toFloat e.y )

            {-

               Equation of the line connecting Start with End

               (x - Sx)(Ey - Sy) = (y - Sy)(Ex - Sx)

                        S
                         \
                          \
                           E

            -}
            floatTrajectoryY x =
                (x - xs) * (ye - ys) / (xe - xs) + ys

            trajectoryY =
                toFloat >> floatTrajectoryY >> round

            collidesWithX tileX =
                let
                    tileCenterX =
                        tileCenterInPixels tileSize tileX

                    blockingSurfaceOffset =
                        -sign * tileSize // 2

                    collisionX =
                        tileCenterX + blockingSurfaceOffset

                    collisionY =
                        trajectoryY collisionX
                in
                { point = Vector collisionX collisionY
                , fix = Vector (collisionX - halfWidth * sign) collisionY
                , tiles =
                    tilesRangeInclusive tileSize (collisionY - halfHeight) (collisionY + halfHeight)
                        |> List.map (Tile tileX)
                }

            range =
                tilesRangeAccountingForMovement tileSize s.x e.x
        in
        List.map collidesWithX range



--
-- Collisions against tile blockers
--


findClosestTo : Pixels -> Vector -> List Collision -> Maybe Collision
findClosestTo tileSize start l =
    let
        distance : Collision -> Int
        distance collision =
            squaredDistance start collision.point
    in
    List.Extra.minimumBy distance l


intersectSweepWithBlockers : (Int -> Int -> Bool) -> Collision -> Maybe Collision
intersectSweepWithBlockers hasBlocker collision =
    case List.filter (\t -> hasBlocker t.x t.y) collision.tiles of
        [] ->
            Nothing

        tiles ->
            Just { collision | tiles = tiles }


collisionAlongX : Args -> Maybe Collision
collisionAlongX args =
    let
        hasBlockerX =
            if args.end.x - args.start.x > 0 then
                args.hasBlockerAlong.positiveDeltaX
            else
                args.hasBlockerAlong.negativeDeltaX
    in
    args
        -- TODO: ensure that the sweep is ordered from nearest to start to furthest
        -- then skip building filtered lists?
        |> sweepAlongX
        |> List.filterMap (intersectSweepWithBlockers hasBlockerX)
        |> findClosestTo args.tileSize args.start



--
-- Collision resolution ------------------------------------------------------
--


resolveSecondaryCollisionAlongY : Collision -> Args -> Collision
resolveSecondaryCollisionAlongY collAlongX args =
    let
        start =
            collAlongX.fix

        end =
            { x = start.x
            , y = args.end.y
            }
    in
    case collisionAlongY { args | start = start, end = end } of
        Nothing ->
            { collAlongX | fix = end }

        Just collAlongY ->
            { point = collAlongX.point
            , fix = collAlongY.fix
            , tiles = collAlongX.tiles ++ collAlongY.tiles
            }



--
-- Flipped axes versions of the functions above
--


flipWH : Size -> Size
flipWH { halfWidth, halfHeight } =
    { halfWidth = halfHeight
    , halfHeight = halfWidth
    }


flipXY : { x : a, y : a } -> { x : a, y : a }
flipXY { x, y } =
    { x = y
    , y = x
    }


flipBlockers : BlockerDirections (Int -> Int -> Bool) -> BlockerDirections (Int -> Int -> Bool)
flipBlockers b =
    { positiveDeltaX = \x y -> b.positiveDeltaY y x
    , negativeDeltaX = \x y -> b.negativeDeltaY y x
    , positiveDeltaY = \x y -> b.positiveDeltaX y x
    , negativeDeltaY = \x y -> b.negativeDeltaX y x
    }


flipArgs : Args -> Args
flipArgs args =
    { args
        | mobSize = flipWH args.mobSize
        , start = flipXY args.start
        , end = flipXY args.end
        , hasBlockerAlong = flipBlockers args.hasBlockerAlong
    }


flipCollision : Collision -> Collision
flipCollision collision =
    { point = flipXY collision.point
    , fix = flipXY collision.fix
    , tiles = List.map flipXY collision.tiles
    }


sweepAlongY : Args -> List Collision
sweepAlongY args =
    args
        |> flipArgs
        |> sweepAlongX
        |> List.map flipCollision


collisionAlongY : Args -> Maybe Collision
collisionAlongY args =
    args
        |> flipArgs
        |> collisionAlongX
        |> Maybe.map flipCollision


resolveSecondaryCollisionAlongX : Collision -> Args -> Collision
resolveSecondaryCollisionAlongX collision args =
    args
        |> flipArgs
        |> resolveSecondaryCollisionAlongY (flipCollision collision)
        |> flipCollision



--
-- Put X and Y together
--


collide : Args -> Maybe Collision
collide args =
    case ( collisionAlongX args, collisionAlongY args ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Just collisionX, Nothing ) ->
            Just <| resolveSecondaryCollisionAlongY collisionX args

        ( Nothing, Just collisionY ) ->
            Just <| resolveSecondaryCollisionAlongX collisionY args

        ( Just collisionX, Just collisionY ) ->
            if squaredDistance args.start collisionX.fix < squaredDistance args.start collisionY.fix then
                Just <| resolveSecondaryCollisionAlongY collisionX args
            else
                Just <| resolveSecondaryCollisionAlongX collisionY args
