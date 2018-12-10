module TileCollisionTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import TileCollision exposing (Size, Vector)


-- Fuzzers


fuzzVector : Fuzzer Vector
fuzzVector =
    Fuzz.map2 Vector (Fuzz.intRange -10000 10000) (Fuzz.intRange -10000 10000)


fuzzSize : Fuzzer Size
fuzzSize =
    Fuzz.map2 Size (Fuzz.intRange 1 100) (Fuzz.intRange 1 100)



--


shouldNotPassWalls : Test
shouldNotPassWalls =
    let
        one =
            Fuzz.map2 Tuple.pair
                (Fuzz.intRange -100 100)
                fuzzSize
    in
    Test.fuzz3 one fuzzVector fuzzVector "Should not pass walls" <|
        \( infiniteWallX, mobSize ) s end ->
            let
                tileSizeInPixels =
                    16

                hasBlockerAlong =
                    { positiveDeltaX = \x y -> x == infiniteWallX
                    , negativeDeltaX = \x y -> False
                    , positiveDeltaY = \x y -> False
                    , negativeDeltaY = \x y -> False
                    }

                wallBlockerX =
                    infiniteWallX * tileSizeInPixels

                maximumStartX =
                    wallBlockerX - mobSize.halfWidth - 1

                start =
                    { s | x = maximumStartX - abs s.x }

                fix =
                    TileCollision.collide
                        { hasBlockerAlong = hasBlockerAlong
                        , tileSize = tileSizeInPixels
                        , mobSize = mobSize
                        , start = start
                        , end = end
                        }
                        |> Maybe.map .fix
                        |> Maybe.withDefault end
            in
            (fix.x + mobSize.halfWidth)
                |> Expect.atMost wallBlockerX


shouldNotLeaveACorral : Test
shouldNotLeaveACorral =
    Test.fuzz3 (Fuzz.intRange 2 20) fuzzVector fuzzVector "Should not leave a corral" <|
        \c s e ->
            let
                (corralRadius, start, end) = (c, s, e)
                --(corralRadius, start, end) = (2,{ x = 0, y = 1240 },{ x = 0, y = 0 })
                tileSizeInPixels =
                    8

                mobSize =
                    { halfWidth = 2
                    , halfHeight = 2
                    }

                -- find the tile that contains the start vector
                centerTileX =
                    TileCollision.tilesRangeInclusive tileSizeInPixels start.x (start.x + 1)
                        |> List.head
                        |> Maybe.withDefault 0

                centerTileY =
                    TileCollision.tilesRangeInclusive tileSizeInPixels start.y (start.y + 1)
                        |> List.head
                        |> Maybe.withDefault 0

                hasBlockerAlong =
                    { positiveDeltaX = \x y -> x - centerTileX > corralRadius
                    , negativeDeltaX = \x y -> x - centerTileX < -corralRadius
                    , positiveDeltaY = \x y -> y - centerTileY > corralRadius
                    , negativeDeltaY = \x y -> y - centerTileY < -corralRadius
                    }

                fix =
                    TileCollision.collide
                        { hasBlockerAlong = hasBlockerAlong
                        , tileSize = tileSizeInPixels
                        , mobSize = mobSize
                        , start = start
                        , end = end
                        }
                        |> Maybe.map .fix
                        |> Maybe.withDefault end

                manhattanDistance a b =
                  abs (a.x - b.x) + abs (a.y - b.y)
            in
            manhattanDistance fix start
                |> Expect.atMost (2 * (corralRadius + 1) * tileSizeInPixels)


shouldNotLeaveACorral1 : Test
shouldNotLeaveACorral1 =
    Test.test "Should not leave a corral 1" <|
        \_ ->
            let
                (corralRadius, start, end) = (2,{ x = 0, y = 1240 },{ x = 0, y = 0 })
                tileSizeInPixels =
                    8

                mobSize =
                    { halfWidth = 2
                    , halfHeight = 2
                    }

                -- find the tile that contains the start vector
                centerTileX =
                    TileCollision.tilesRangeInclusive tileSizeInPixels start.x (start.x + 1)
                        |> List.head
                        |> Maybe.withDefault 0

                centerTileY =
                    TileCollision.tilesRangeInclusive tileSizeInPixels start.y (start.y + 1)
                        |> List.head
                        |> Maybe.withDefault 0

                hasBlockerAlong =
                    { positiveDeltaX = \x y -> x - centerTileX > corralRadius
                    , negativeDeltaX = \x y -> x - centerTileX < -corralRadius
                    , positiveDeltaY = \x y -> y - centerTileY > corralRadius
                    , negativeDeltaY = \x y -> y - centerTileY < -corralRadius
                    }

                fix =
                    TileCollision.collide
                        { hasBlockerAlong = hasBlockerAlong
                        , tileSize = tileSizeInPixels
                        , mobSize = mobSize
                        , start = start
                        , end = end
                        }
                        |> Maybe.map .fix
                        |> Maybe.withDefault end

                manhattanDistance a b =
                  abs (a.x - b.x) + abs (a.y - b.y)
            in
            manhattanDistance fix start
                |> Expect.atMost (2 * (corralRadius + 1) * tileSizeInPixels)
