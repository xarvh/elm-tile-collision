module Decompose exposing (..)

import List.Extra


-- Types


type alias Vec =
    { x : Float
    , y : Float
    }


type alias RowColumn =
    { row : Int
    , column : Int
    }


type alias RawCollision geometry =
    { geometry : geometry
    , fix : Vec
    , point : Vec
    , distanceSquared : Float
    }


type alias FinalCollision geometry =
    { tile : RowColumn
    , point : Vec2
    , geometry : geometry
    }


type alias AabbTrajectory =
    { start : Vec
    , end : Vec
    , width : Float
    , height : Float
    , minimumDistance : Float
    }


type alias RelativeAabbTrajectory =
    { relativeStart : Vec
    , relativeEnd : Vec
    , halfWidth : Float
    , halfHeight : Float
    , minimumDistance : Float
    }



-- Vec


distanceSquared : Vec -> Vec -> Float
distanceSquared a b =
    let
        dx =
            a.x - b.x

        dy =
            a.y - b.y
    in
    dx * dx + dy * dy


vecInvertX : Vec -> Vec
vecInvertX v =
    { v | x = -v.x }


vecInvertY : Vec -> Vec
vecInvertY v =
    { v | y = -v.y }


vecFlipXY : Vec -> Vec
vecFlipXY v =
    { x = v.y
    , y = v.x
    }



-- Collision


geometryMap : (a -> b) -> Collision a -> Collision b
geometryMap f collision =
    { geometry = f collision.geometry
    , point = collision.point
    , fix = collision.fix
    }



-- Tiles & Vecs


coordinateToTile : Float -> Int
coordinateToTile =
    round


vecToRowColumn : Vec -> RowColumn
vecToRowColumn v =
    { row = coordinateToTile v.y
    , column = coordinateToTile v.x
    }


tileRange : Float -> Float -> Float -> List Int
tileRange start end half =
    if start < end then
        List.range
            (coordinateToTile <| start + half)
            (coordinateToTile <| end + half)
    else
        List.range
            (coordinateToTile <| end - half)
            (coordinateToTile <| start - half)



-- Sweep


{-| Find all tiles swept by a horizontal segment whose center moves from start to end.

If the AABB is moving right, consider only the tiles swept by the right side of the AABB
...

-}
sweep : AabbTrajectory -> List RowColumn
sweep t =
    let
        halfWidth =
            t.width / 2

        halfHeight =
            t.height / 2
    in
    case ( t.start.x - t.end.x, t.start.y - t.end.y ) of
        ( 0, 0 ) ->
            []

        ( _, 0 ) ->
            tileRange s.x e.x halfWidth
                |> map (\x -> { column = x, row = coordinateToTile s.y })

        ( 0, _ ) ->
            tileRange s.y e.y halfHeight
                |> map (\y -> { column = coordinateToTile s.x, row = y })

        ( _, _ ) ->
            List.Extra.lift2
                (\x y -> { column = x, row = y })
                (tileRange s.x e.x halfWidth)
                (tileRange s.y e.y halfHeight)


type alias Pair g =
    ( RowColumn, RawCollision g )


recursiveCollide : (RowColumn -> TileCollider geometry) -> AabbTrajectory -> List (Pair geometry) -> List (Pair geometry)
recursiveCollide getCollider t pairs =
    let
        testCollision : RowColumn -> Maybe Collision
        testCollision tile =
            { relativeStart = vectorRelativeToTile tile t.start
            , relativeEnd = vectorRelativeToTile tile t.end
            , halfWidth = t.width / 2
            , halfHeight = t.height / 2
            , minimumDistance = t.minimumDistance
            }
                |> getCollider tile
                |> Maybe.map (makeAbsolute tile >> Tuple.pair tile)

        maybeCollision =
            sweep t
                |> List.filterMap testCollision
                |> List.minimumBy (Tuple.second >> .distanceSquared)
    in
    case maybeCollision of
        Nothing ->
            pairs

        Just ( tile, collision ) ->
            pairs
                |> (::) ( tile, collison )
                |> collide getCollider
                    { t
                        | start = collision.aabbAtCollision
                        , end = collision.fix
                    }



-- type alias Result geometry ={ fix : Vec2, collisions : List (Collision2 geometry) }


collide : (RowColumn -> TileCollider geometry) -> AabbTrajectory -> { fix : Vec, collisions : List (FinalCollision geometry) }
collide getCollider t =
    case recursiveCollide getCollider t [] of
        [] ->
            { fix = t.end
            , collisions = []
            }

        head :: tail ->
            { fix = head.fix
            , collisions =
                (head :: tail)
                    |> List.map pairToFinalCollision
            }


{-| Defines how a certain tile type reacts to an AABB bumping into it

`start` and `end` of the AabbTrajectory are

-}
type alias TileCollider geometry =
    RelativeAabbTrajectory -> Maybe (Collision geometry)


combine : List (TileCollider a) -> TileCollider a
combine colliders =
    let
        combined : TileCollider a
        combined input =
            colliders
                |> List.filterMap (\collider -> collider input)
                |> List.Extra.minimumBy .distanceSquared
    in
    combined


map : (a -> b) -> TileCollider a -> TileCollider b
map f collider =
    collider >> Maybe.map (geometryMap f)


{-| `forwards` and `backwards` can be any two functions that satisfy `forwards >> backwards == identity`
-}
transform : (Vec -> Vec) -> (Vec -> Vec) -> (a -> b) -> TileCollider a -> TileCollider b
transform forwards backwards geo collider =
    let
        transformed : TileCollider b
        transformed input =
            { input
                | relativeStart = forwards input.relativeStart
                , relativeEnd = forwards input.relativeEnd
            }
                |> collider
                |> Maybe.map
                    (\collision ->
                        { geometry = geo collision.geometry
                        , fix = backwards collision.fix
                        , point = backwards collision.point
                        , distanceSquared = collision.distanceSquared
                        }
                    )
    in
    transformed


invertX : TileCollider a -> TileCollider a
invertX =
    transform vecInvertX vecInvertX identity


invertY : TileCollider a -> TileCollider a
invertY =
    transform vecInvertY vecInvertY identity


flipXY : TileCollider a -> TileCollider a
flipXY collider =
    let
        transformed : TileCollider a
        transformed input =
            { input
                | relativeStart = vecFlipXY input.relativeStart
                , relativeEnd = vecFlipXY input.relativeEnd
                , halfWidth = input.halfHeight
                , halfHeight = input.halfWidth
            }
                |> collider
                |> Maybe.map
                    (\collision ->
                        { geometry = {- TODO use a map? -} collision.geometry
                        , fix = vecFlipXY collision.fix
                        , point = vecFlipXY collision.point
                        , distanceSquared = collision.distanceSquared
                        }
                    )
    in
    transformed



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

                point =
                    { x = blockX
                    , y = collisionY
                    }
            in
            Just
                { geometry = ()
                , fix = { relativeEnd | x = max relativeStart.x fixedX }
                , point = point
                , distanceSquared = distanceSquared point relativeStart
                }


rightToLeftBlocker : TileCollider ()
rightToLeftBlocker =
    invertX leftToRightBlocker


type DeltaSign
    = Positive
    | Negative


type SquareBlocker
    = X DeltaSign
    | Y DeltaSign


squareBlocker : TileCollider SquareBlocker
squareBlocker =
    combine
        [ map (\() -> X Positive) leftToRightBlocker
        , map (\() -> X Negative) rightToLeftBlocker
        , map (\() -> Y Positive) (flipXY leftToRightBlocker)
        , map (\() -> Y Negative) (flipXY rightToLeftBlocker)
        ]
