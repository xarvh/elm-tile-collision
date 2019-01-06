module Decompose exposing (..)


type alias RowColumn =
    { row : Int
    , column : Int
    }



-- Sweep


{-| Find all tiles swept by a horizontal segment whose center moves from start to end.
The resulting tiles will be ordered by time of impact.
sweepHorizontalSegment : AabbTrajectory -> List RowColumn
sweepHorizontalSegment { start, end, tileSize } =
Debug.todo ""
-}
type alias Vec =
    { x : Float
    , y : Float
    }


{-| Same as above, only `start` and `end` are given relative to the tile center.
-}
type alias RelativeAabbTrajectory =
    { relativeStart : Vec
    , relativeEnd : Vec
    , halfWidth : Float
    , halfHeight : Float
    , minimumDistance : Float
    }


type alias Collision geometry =
    { geometry : geometry
    , fix : Vec
    , point : Vec
    }


{-| Defines how a certain tile type reacts to an AABB bumping into it

`start` and `end` of the AabbTrajectory are

-}
type alias TileCollider geometry =
    RelativeAabbTrajectory -> Maybe (Collision geometry)


tileColliderFlipX : TileCollider a -> TileCollider a
tileColliderFlipX original =
    let
        vecFlipX : Vec -> Vec
        vecFlipX v =
            { v | x = -v.x }

        flipped : TileCollider a
        flipped input =
            { input
                | relativeStart = vecFlipX input.relativeStart
                , relativeEnd = vecFlipX input.relativeEnd
            }
                |> original
                |> Maybe.map
                    (\collision ->
                        { collision
                            | fix = vecFlipX collision.fix
                            , point = vecFlipX collision.point
                        }
                    )
    in
    flipped



-- Empty block


emptyTile : TileCollider Never
emptyTile aabbTrajectory =
    Nothing



-- Left to right blocker


makeTrajectory : Vec -> Vec -> Float -> Float
makeTrajectory s e x =
    (x - s.x) * (e.y - s.y) / (e.x - s.x) + s.y


leftToRightBlocker : TileCollider ()
leftToRightBlocker { relativeStart, relativeEnd, halfWidth, halfHeight, minimumDistance } =
    let
        -- The actual X coordinate of the blocker respect to the tile center is -0.5
        blockX =
            -0.5
    in
    if relativeStart.x >= relativeEnd.x then
        -- If movement is not left to right, no point in continuing
        Nothing
    else if relativeStart.x + halfWidth > blockX then
        -- The AABB is already past the block, so it should pass
        Nothing
    else if relativeEnd.x + halfWidth <= blockX then
        -- The AABB stops before actually encountering the block
        Nothing
    else
        let
            trajectory =
                makeTrajectory relativeStart relativeEnd

            -- The AAB side that can collide is the right side
            collisionY =
                trajectory (blockX - halfWidth)
        in
        if collisionY + halfHeight < -0.5 then
            -- Top of the AABB is below the tile at the collision point
            Nothing
        else if collisionY - halfHeight > 0.5 then
            -- Bottom of the AABB is above the tile at the collision point
            Nothing
        else
            let
                fixedX =
                    blockX - halfWidth - minimumDistance
            in
            Just
                { geometry = ()
                , fix =
                    { relativeEnd
                        | x = max relativeStart.x fixedX
                    }
                , point =
                    { x = blockX
                    , y = collisionY
                    }
                }


rightToLeftBlocker : TileCollider ()
rightToLeftBlocker =
    tileColliderFlipX leftToRightBlocker
