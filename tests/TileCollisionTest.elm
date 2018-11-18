module TileCollisionTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import TileCollision exposing (Size, Vector)


-- Fuzzers


fuzzVector : Fuzzer Vector
fuzzVector =
    Fuzz.map2 Vector (Fuzz.floatRange -100 100) (Fuzz.floatRange -100 100)


fuzzSize : Fuzzer Size
fuzzSize =
    Fuzz.map2 Size (Fuzz.floatRange 0 100) (Fuzz.floatRange 0 100)


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
        \{ infiniteWallX, mobSize, s, end } ->
            let
                hasBlockerAlong =
                    { positiveX = \x y -> x == infiniteWallX
                    , negativeX = \x y -> False
                    , positiveY = \x y -> False
                    , negativeY = \x y -> False
                    }

                wallBlockerX =
                    toFloat infiniteWallX - 0.5

                start =
                    { s | x = wallBlockerX - abs s.x }

                ( fix, collisions ) =
                    TileCollision.collisionsAlongX hasBlockerAlong mobSize start end
            in
            fix.x
                + mobSize.width
                |> Expect.atMost wallBlockerX
