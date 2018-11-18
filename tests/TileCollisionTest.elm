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
    Fuzz.map2 Size (Fuzz.intRange 0 100) (Fuzz.intRange 0 100)


type alias TestParams =
    { infiniteWallX : Int
    , mobSize : Size
    , s : Vector
    , end : Vector
    }


fuzzTestParams : Fuzzer TestParams
fuzzTestParams =
    Fuzz.map4 TestParams
        (Fuzz.intRange -100 100)
        fuzzSize
        fuzzVector
        fuzzVector



--


suite : Test
suite =
    Test.fuzz fuzzTestParams "Should not pass walls" <|
        \params ->
            let
                { infiniteWallX, mobSize, s, end } =
                    params

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
