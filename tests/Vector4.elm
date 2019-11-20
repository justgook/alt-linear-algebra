module Vector4 exposing (suite)

import AltMath.Advanced.ADT.Vector4 as ADT
import AltMath.Advanced.Record.Vector4 as Record
import AltMath.Advanced.Tuple.Vector4 as Tuple
import AltMath.Const exposing (epsilon)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Math.Vector4 as Math
import Test exposing (..)


suite : Test
suite =
    describe "Vector4!"
        [ fuzzWrap "getX" Math.getX ADT.getX Record.getX Tuple.getX
        , fuzzWrap "getY" Math.getX ADT.getX Record.getX Tuple.getX
        , fuzzWrap "getZ" Math.getZ ADT.getZ Record.getZ Tuple.getZ
        , fuzzWrap "getW" Math.getW ADT.getW Record.getW Tuple.getW
        , fuzzWrapFloatVec "setW" Math.setW ADT.setW Record.setW Tuple.setW
        , fuzzWrapFloatVec "setX" Math.setX ADT.setX Record.setX Tuple.setX
        , fuzzWrapFloatVec "setY" Math.setY ADT.setY Record.setY Tuple.setY
        , fuzzWrapFloatVec "setZ" Math.setZ ADT.setZ Record.setZ Tuple.setZ
        , fuzzWrap2 "distance" Math.distance ADT.distance Record.distance Tuple.distance
        , fuzzWrap2 "distanceSquared" Math.distanceSquared ADT.distanceSquared Record.distanceSquared Tuple.distanceSquared
        , fuzzWrap2 "dot" Math.dot ADT.dot Record.dot Tuple.dot
        , fuzzWrap "length" Math.length ADT.length Record.length Tuple.length
        , fuzzWrap "lengthSquared" Math.lengthSquared ADT.lengthSquared Record.lengthSquared Tuple.lengthSquared
        , fuzzWrap2Vec "add" Math.add ADT.add Record.add Tuple.add
        , fuzzWrap2Vec "sub" Math.sub ADT.sub Record.sub Tuple.sub
        , fuzzWrap2Vec "direction" Math.direction ADT.direction Record.direction Tuple.direction
        , fuzzWrapVec "negate" Math.negate ADT.negate Record.negate Tuple.negate
        , fuzzWrapVec "normalize" Math.normalize ADT.normalize Record.normalize Tuple.normalize
        , fuzzWrapFloatVec "scale" Math.scale ADT.scale Record.scale Tuple.scale
        ]


record : Fuzz.Fuzzer { x : Float, y : Float, z : Float, w : Float }
record =
    Fuzz.map4 (\x y z w -> { x = x, y = y, z = z, w = w })
        Fuzz.float
        Fuzz.float
        Fuzz.float
        Fuzz.float


fuzzWrap : String -> (Math.Vec4 -> Float) -> (ADT.Vec4 -> Float) -> (Record.Vec4 -> Float) -> (Tuple.Vec4 -> Float) -> Test
fuzzWrap name fn0 fn1 fn2 fn3 =
    fuzz record name <|
        \r1 ->
            Expect.all
                [ Expect.within (Absolute epsilon) (fn1 (ADT.fromRecord r1))
                , Expect.within (Absolute epsilon) (fn2 (Record.fromRecord r1))
                , Expect.within (Absolute epsilon) (fn3 (Tuple.fromRecord r1))
                ]
                (fn0 (Math.fromRecord r1))


fuzzWrapVec : String -> (Math.Vec4 -> Math.Vec4) -> (ADT.Vec4 -> ADT.Vec4) -> (Record.Vec4 -> Record.Vec4) -> (Tuple.Vec4 -> Tuple.Vec4) -> Test
fuzzWrapVec name fn0 fn1 fn2 fn3 =
    fuzz record name <|
        \r1 ->
            Expect.all
                [ \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn1 (ADT.fromRecord r1) |> ADT.toRecord)
                , \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn2 (Record.fromRecord r1) |> Record.toRecord)
                , \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn3 (Tuple.fromRecord r1) |> Tuple.toRecord)
                ]
                (fn0 (Math.fromRecord r1) |> Math.toRecord)


fuzzWrap2 : String -> (Math.Vec4 -> Math.Vec4 -> Float) -> (ADT.Vec4 -> ADT.Vec4 -> Float) -> (Record.Vec4 -> Record.Vec4 -> Float) -> (Tuple.Vec4 -> Tuple.Vec4 -> Float) -> Test
fuzzWrap2 name fn0 fn1 fn2 fn3 =
    fuzz2 record record name <|
        \r1 r2 ->
            Expect.all
                [ Expect.within (Absolute epsilon) (fn1 (ADT.fromRecord r1) (ADT.fromRecord r2))
                , Expect.within (Absolute epsilon) (fn2 (Record.fromRecord r1) (Record.fromRecord r2))
                , Expect.within (Absolute epsilon) (fn3 (Tuple.fromRecord r1) (Tuple.fromRecord r2))
                ]
                (fn0 (Math.fromRecord r1) (Math.fromRecord r2))


fuzzWrap2Vec : String -> (Math.Vec4 -> Math.Vec4 -> Math.Vec4) -> (ADT.Vec4 -> ADT.Vec4 -> ADT.Vec4) -> (Record.Vec4 -> Record.Vec4 -> Record.Vec4) -> (Tuple.Vec4 -> Tuple.Vec4 -> Tuple.Vec4) -> Test
fuzzWrap2Vec name fn0 fn1 fn2 fn3 =
    fuzz2 record record name <|
        \r1 r2 ->
            Expect.all
                [ \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn1 (ADT.fromRecord r1) (ADT.fromRecord r2) |> ADT.toRecord)
                , \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn2 (Record.fromRecord r1) (Record.fromRecord r2) |> Record.toRecord)
                , \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn3 (Tuple.fromRecord r1) (Tuple.fromRecord r2) |> Tuple.toRecord)
                ]
                (fn0 (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)


fuzzWrapFloatVec : String -> (Float -> Math.Vec4 -> Math.Vec4) -> (Float -> ADT.Vec4 -> ADT.Vec4) -> (Float -> Record.Vec4 -> Record.Vec4) -> (Float -> Tuple.Vec4 -> Tuple.Vec4) -> Test
fuzzWrapFloatVec name fn0 fn1 fn2 fn3 =
    fuzz2 record Fuzz.float name <|
        \r1 fl ->
            Expect.all
                [ \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn1 fl (ADT.fromRecord r1) |> ADT.toRecord)
                , \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn2 fl (Record.fromRecord r1) |> Record.toRecord)
                , \{ x, y, z, w } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        , .z >> Expect.within (Absolute epsilon) z
                        , .w >> Expect.within (Absolute epsilon) w
                        ]
                        (fn3 fl (Tuple.fromRecord r1) |> Tuple.toRecord)
                ]
                (fn0 fl (Math.fromRecord r1) |> Math.toRecord)
