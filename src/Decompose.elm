module Decompose exposing (..)

import List.Extra


-- Vec


type alias Vec =
    { x : Float
    , y : Float
    }


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



--


type alias RowColumn =
    { row : Int
    , column : Int
    }
















-- Sweep
{- Find all tiles swept by a horizontal segment whose center moves from start to end.

   If the AABB is moving right, consider only the tiles swept by the right side of the AABB
   ...

-}

coordinateToTile : Float -> Int
coordinateToTile =
  round



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





tiles =
  case (dx, dy) of
    (0, 0) ->
      []
    (_, 0) ->
      tileRange s.x e.x mob.halfWidth |> map (withY s.y)

    (0, _) ->
      tileRange s.y e.y mob.halfHeight |> map (withX s.x)

    (_, _) ->
      ...









{-|
-}
type alias RelativeAabbTrajectory =
    { relativeStart : Vec
    , relativeEnd : Vec
    , halfWidth : Float
    , halfHeight : Float
    , minimumDistance : Float
    }



--


type alias Collision geometry =
    { geometry : geometry
    , fix : Vec
    , point : Vec
    }


geometryMap : (a -> b) -> Collision a -> Collision b
geometryMap f collision =
    { geometry = f collision.geometry
    , point = collision.point
    , fix = collision.fix
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
                |> List.Extra.minimumBy (\collision -> distanceSquared input.relativeStart collision.point)
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
