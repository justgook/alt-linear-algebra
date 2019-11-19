module Vector2 exposing (..)

import AltMath.ADT.Vector2 as ADT
import AltMath.Const exposing (epsilon)
import AltMath.Record.Vector2 as Record
import AltMath.Tuple.Vector2 as Tuple
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Math.Vector2 as Math
import Test exposing (..)


suite : Test
suite =
    describe "Vector2!"
        [ fuzzWrap "getX" Math.getX ADT.getX Record.getX Tuple.getX
        , fuzzWrap "getY" Math.getX ADT.getX Record.getX Tuple.getX
        , fuzzWrapFloatVec "setX" Math.setX ADT.setX Record.setX Tuple.setX
        , fuzzWrapFloatVec "setY" Math.setY ADT.setY Record.setY Tuple.setY
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


record : Fuzz.Fuzzer { x : Float, y : Float }
record =
    Fuzz.map2 (\x y -> { x = x, y = y })
        Fuzz.float
        Fuzz.float


fuzzWrap : String -> (Math.Vec2 -> Float) -> (ADT.Vec2 -> Float) -> (Record.Vec2 -> Float) -> (Tuple.Vec2 -> Float) -> Test
fuzzWrap name fn0 fn1 fn2 fn3 =
    fuzz record name <|
        \r1 ->
            Expect.all
                [ Expect.within (Absolute epsilon) (fn1 (ADT.fromRecord r1))
                , Expect.within (Absolute epsilon) (fn2 (Record.fromRecord r1))
                , Expect.within (Absolute epsilon) (fn3 (Tuple.fromRecord r1))
                ]
                (fn0 (Math.fromRecord r1))


fuzzWrapVec : String -> (Math.Vec2 -> Math.Vec2) -> (ADT.Vec2 -> ADT.Vec2) -> (Record.Vec2 -> Record.Vec2) -> (Tuple.Vec2 -> Tuple.Vec2) -> Test
fuzzWrapVec name fn0 fn1 fn2 fn3 =
    fuzz record name <|
        \r1 ->
            Expect.all
                [ \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn1 (ADT.fromRecord r1) |> ADT.toRecord)
                , \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn2 (Record.fromRecord r1) |> Record.toRecord)
                , \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn3 (Tuple.fromRecord r1) |> Tuple.toRecord)
                ]
                (fn0 (Math.fromRecord r1) |> Math.toRecord)


fuzzWrap2 : String -> (Math.Vec2 -> Math.Vec2 -> Float) -> (ADT.Vec2 -> ADT.Vec2 -> Float) -> (Record.Vec2 -> Record.Vec2 -> Float) -> (Tuple.Vec2 -> Tuple.Vec2 -> Float) -> Test
fuzzWrap2 name fn0 fn1 fn2 fn3 =
    fuzz2 record record name <|
        \r1 r2 ->
            Expect.all
                [ Expect.within (Absolute epsilon) (fn1 (ADT.fromRecord r1) (ADT.fromRecord r2))
                , Expect.within (Absolute epsilon) (fn2 (Record.fromRecord r1) (Record.fromRecord r2))
                , Expect.within (Absolute epsilon) (fn3 (Tuple.fromRecord r1) (Tuple.fromRecord r2))
                ]
                (fn0 (Math.fromRecord r1) (Math.fromRecord r2))


fuzzWrap2Vec : String -> (Math.Vec2 -> Math.Vec2 -> Math.Vec2) -> (ADT.Vec2 -> ADT.Vec2 -> ADT.Vec2) -> (Record.Vec2 -> Record.Vec2 -> Record.Vec2) -> (Tuple.Vec2 -> Tuple.Vec2 -> Tuple.Vec2) -> Test
fuzzWrap2Vec name fn0 fn1 fn2 fn3 =
    fuzz2 record record name <|
        \r1 r2 ->
            Expect.all
                [ \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn1 (ADT.fromRecord r1) (ADT.fromRecord r2) |> ADT.toRecord)
                , \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn2 (Record.fromRecord r1) (Record.fromRecord r2) |> Record.toRecord)
                , \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn3 (Tuple.fromRecord r1) (Tuple.fromRecord r2) |> Tuple.toRecord)
                ]
                (fn0 (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)


fuzzWrapFloatVec : String -> (Float -> Math.Vec2 -> Math.Vec2) -> (Float -> ADT.Vec2 -> ADT.Vec2) -> (Float -> Record.Vec2 -> Record.Vec2) -> (Float -> Tuple.Vec2 -> Tuple.Vec2) -> Test
fuzzWrapFloatVec name fn0 fn1 fn2 fn3 =
    fuzz2 record Fuzz.float name <|
        \r1 fl ->
            Expect.all
                [ \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn1 fl (ADT.fromRecord r1) |> ADT.toRecord)
                , \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn2 fl (Record.fromRecord r1) |> Record.toRecord)
                , \{ x, y } ->
                    Expect.all
                        [ .x >> Expect.within (Absolute epsilon) x
                        , .y >> Expect.within (Absolute epsilon) y
                        ]
                        (fn3 fl (Tuple.fromRecord r1) |> Tuple.toRecord)
                ]
                (fn0 fl (Math.fromRecord r1) |> Math.toRecord)
