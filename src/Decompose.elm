module Decompose exposing (..)

{-| Same as above, only `start` and `end` are given relative to the tile center.
-}


type alias RelativeAabbTrajectory =
    { relativeStart : Vec2
    , relativeEnd : Vec2
    , halfWidth : Float
    , halfHeight : Float
    , minimumDistance : Float
    }


{-| Defines how a certain tile type reacts to an AABB bumping into it

`start` and `end` of the AabbTrajectory are

-}
type alias TileType collision =
    RelativeAabbTrajectory -> Maybe { collision : collision, fix : Vec2 }


type alias Vec2 =
    { x : Float
    , y : Float
    }


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



-- Empty block


emptyTile : TileType Never
emptyTile aabbTrajectory =
    Nothing



-- Left to right blocker


makeTrajectory : Vec2 -> Vec2 -> Float -> Float
makeTrajectory s e x =
    (x - s.x) * (e.y - s.y) / (e.x - s.x) + s.y


leftToRightBlocker : TileType ()
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
                { collision = ()
                , fix = { relativeEnd | x = max relativeStart.x fixedX }
                }
