module Matrix4 exposing (..)

import AltMath.Alternative.ADT.Matrix4 as ADT
import AltMath.Alternative.ADT.Vector3 as ADTVec3
import AltMath.Alternative.Record.Matrix4 as Record
import AltMath.Alternative.Record.Vector3 as RecordVec3
import AltMath.Alternative.Tuple.Matrix4 as Tuple
import AltMath.Alternative.Tuple.Vector3 as TupleVec3
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
import Math.Matrix4 as Math
import Math.Vector3 as MathVec3
import Test exposing (..)


suite : Test
suite =
    describe "Matrix4!"
        [ test "identity" <|
            \_ ->
                Expect.all
                    [ compare (ADT.toRecord ADT.identity)
                    , compare (Record.toRecord Record.identity)
                    , compare (Tuple.toRecord Tuple.identity)
                    ]
                    (Math.toRecord Math.identity)
        , fuzz record "inverse" <|
            \r ->
                Expect.all
                    [ Maybe.map2 compare (r |> ADT.fromRecord |> ADT.inverse |> Maybe.map ADT.toRecord) >> Maybe.withDefault Expect.pass
                    , Maybe.map2 compare (Record.fromRecord r |> Record.inverse |> Maybe.map Record.toRecord) >> Maybe.withDefault Expect.pass
                    , Maybe.map2 compare (Tuple.fromRecord r |> Tuple.inverse |> Maybe.map Tuple.toRecord) >> Maybe.withDefault Expect.pass
                    ]
                    (Math.fromRecord r |> Math.inverse |> Maybe.map Math.toRecord)
        , fuzz record "inverseOrthonormal" <|
            \r ->
                Expect.all
                    [ compare (r |> ADT.fromRecord |> ADT.inverseOrthonormal |> ADT.toRecord)
                    , compare (r |> Record.fromRecord |> Record.inverseOrthonormal |> Record.toRecord)
                    , compare (r |> Tuple.fromRecord |> Tuple.inverseOrthonormal |> Tuple.toRecord)
                    ]
                    (r |> Math.fromRecord |> Math.inverseOrthonormal |> Math.toRecord)
        , fuzz2 record record "mul" <|
            \r1 r2 ->
                Expect.all
                    [ compare (ADT.mul (ADT.fromRecord r1) (ADT.fromRecord r2) |> ADT.toRecord)
                    , compare (Record.mul (Record.fromRecord r1) (Record.fromRecord r2) |> Record.toRecord)
                    , compare (Tuple.mul (Tuple.fromRecord r1) (Tuple.fromRecord r2) |> Tuple.toRecord)
                    ]
                    (Math.mul (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)
        , fuzz2 record record "mulAffine" <|
            \r1 r2 ->
                Expect.all
                    [ compare (ADT.mulAffine (ADT.fromRecord r1) (ADT.fromRecord r2) |> ADT.toRecord)
                    , compare (Record.mulAffine (Record.fromRecord r1) (Record.fromRecord r2) |> Record.toRecord)
                    , compare (Tuple.mulAffine (Tuple.fromRecord r1) (Tuple.fromRecord r2) |> Tuple.toRecord)
                    ]
                    (Math.mulAffine (Math.fromRecord r1) (Math.fromRecord r2) |> Math.toRecord)
        , fuzz record "transpose" <|
            \r ->
                Expect.all
                    [ compare (r |> ADT.fromRecord |> ADT.transpose |> ADT.toRecord)
                    , compare (r |> Record.fromRecord |> Record.transpose |> Record.toRecord)
                    , compare (r |> Tuple.fromRecord |> Tuple.transpose |> Tuple.toRecord)
                    ]
                    (r |> Math.fromRecord |> Math.transpose |> Math.toRecord)
        , fuzz3 recordVec3 recordVec3 recordVec3 "makeBasis" <|
            \v1 v2 v3 ->
                Expect.all
                    [ compare (ADT.makeBasis v1.adt v2.adt v3.adt |> ADT.toRecord)
                    , compare (Record.makeBasis v1.record v2.record v3.record |> Record.toRecord)
                    , compare (Tuple.makeBasis v1.tuple v2.tuple v3.tuple |> Tuple.toRecord)
                    ]
                    (Math.makeBasis v1.math v2.math v3.math |> Math.toRecord)
        , fuzz2 record recordVec3 "transform" <|
            \r1 v2 ->
                Expect.all
                    [ compareVec3 (ADT.transform (ADT.fromRecord r1) v2.adt |> ADTVec3.toRecord)
                    , compareVec3 (Record.transform (Record.fromRecord r1) v2.record |> RecordVec3.toRecord)
                    , compareVec3 (Tuple.transform (Tuple.fromRecord r1) v2.tuple |> TupleVec3.toRecord)
                    ]
                    (Math.transform (Math.fromRecord r1) v2.math |> MathVec3.toRecord)
        , fuzz6 Fuzz.float positive Fuzz.float positive Fuzz.float positive "makeFrustum" <|
            \f1 f2 f3 f4 f5 f6 ->
                Expect.all
                    [ compare (ADT.makeFrustum f1 f2 f3 f4 f5 f6 |> ADT.toRecord)
                    , compare (Record.makeFrustum f1 f2 f3 f4 f5 f6 |> Record.toRecord)
                    , compare (Tuple.makeFrustum f1 f2 f3 f4 f5 f6 |> Tuple.toRecord)
                    ]
                    (Math.makeFrustum f1 f2 f3 f4 f5 f6 |> Math.toRecord)
        , fuzz4 positive positive positive positive "makePerspective" <|
            \f1 f2 f3 f4 ->
                Expect.all
                    [ compare (ADT.makePerspective f1 f2 f3 f4 |> ADT.toRecord)
                    , compare (Record.makePerspective f1 f2 f3 f4 |> Record.toRecord)
                    , compare (Tuple.makePerspective f1 f2 f3 f4 |> Tuple.toRecord)
                    ]
                    (Math.makePerspective f1 f2 f3 f4 |> Math.toRecord)
        , fuzz6 Fuzz.float positive Fuzz.float positive Fuzz.float positive "makeOrtho" <|
            \f1 f2 f3 f4 f5 f6 ->
                Expect.all
                    [ compare (ADT.makeOrtho f1 f2 f3 f4 f5 f6 |> ADT.toRecord)
                    , compare (Record.makeOrtho f1 f2 f3 f4 f5 f6 |> Record.toRecord)
                    , compare (Tuple.makeOrtho f1 f2 f3 f4 f5 f6 |> Tuple.toRecord)
                    ]
                    (Math.makeOrtho f1 f2 f3 f4 f5 f6 |> Math.toRecord)
        , fuzz4 Fuzz.float positive Fuzz.float positive "makeOrtho2D" <|
            \f1 f2 f3 f4 ->
                Expect.all
                    [ compare (ADT.makeOrtho2D f1 f2 f3 f4 |> ADT.toRecord)
                    , compare (Record.makeOrtho2D f1 f2 f3 f4 |> Record.toRecord)
                    , compare (Tuple.makeOrtho2D f1 f2 f3 f4 |> Tuple.toRecord)
                    ]
                    (Math.makeOrtho2D f1 f2 f3 f4 |> Math.toRecord)
        , fuzz3 recordVec3 recordVec3 recordVec3 "makeLookAt" <|
            \v1 v2 v3 ->
                Expect.all
                    [ compareCustom 0.000002 (ADT.makeLookAt v1.adt v2.adt v3.adt |> ADT.toRecord)
                    , compareCustom 0.000002 (Record.makeLookAt v1.record v2.record v3.record |> Record.toRecord)
                    , compareCustom 0.000002 (Tuple.makeLookAt v1.tuple v2.tuple v3.tuple |> Tuple.toRecord)
                    ]
                    (Math.makeLookAt v1.math v2.math v3.math |> Math.toRecord)
        , fuzz3 Fuzz.float recordVec3 record "rotate" <|
            \f v r ->
                Expect.all
                    [ compare (ADT.rotate f v.adt (ADT.fromRecord r) |> ADT.toRecord)
                    , compare (Record.rotate f v.record (Record.fromRecord r) |> Record.toRecord)
                    , compare (Tuple.rotate f v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
                    ]
                    (Math.rotate f v.math (Math.fromRecord r) |> Math.toRecord)
        , fuzz2 recordVec3 record "scale" <|
            \v r ->
                Expect.all
                    [ compare (ADT.scale v.adt (ADT.fromRecord r) |> ADT.toRecord)
                    , compare (Record.scale v.record (Record.fromRecord r) |> Record.toRecord)
                    , compare (Tuple.scale v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
                    ]
                    (Math.scale v.math (Math.fromRecord r) |> Math.toRecord)
        , fuzz4 Fuzz.float Fuzz.float Fuzz.float record "scale3" <|
            \f1 f2 f3 r ->
                Expect.all
                    [ compare (ADT.scale3 f1 f2 f3 (ADT.fromRecord r) |> ADT.toRecord)
                    , compare (Record.scale3 f1 f2 f3 (Record.fromRecord r) |> Record.toRecord)
                    , compare (Tuple.scale3 f1 f2 f3 (Tuple.fromRecord r) |> Tuple.toRecord)
                    ]
                    (Math.scale3 f1 f2 f3 (Math.fromRecord r) |> Math.toRecord)
        , fuzz2 recordVec3 record "translate" <|
            \v r ->
                Expect.all
                    [ compare (ADT.translate v.adt (ADT.fromRecord r) |> ADT.toRecord)
                    , compare (Record.translate v.record (Record.fromRecord r) |> Record.toRecord)
                    , compare (Tuple.translate v.tuple (Tuple.fromRecord r) |> Tuple.toRecord)
                    ]
                    (Math.translate v.math (Math.fromRecord r) |> Math.toRecord)
        , fuzz4 Fuzz.float Fuzz.float Fuzz.float record "translate3" <|
            \f1 f2 f3 r ->
                Expect.all
                    [ compare (ADT.translate3 f1 f2 f3 (ADT.fromRecord r) |> ADT.toRecord)
                    , compare (Record.translate3 f1 f2 f3 (Record.fromRecord r) |> Record.toRecord)
                    , compare (Tuple.translate3 f1 f2 f3 (Tuple.fromRecord r) |> Tuple.toRecord)
                    ]
                    (Math.translate3 f1 f2 f3 (Math.fromRecord r) |> Math.toRecord)
        , fuzz2 Fuzz.float recordVec3 "makeRotate" <|
            \f v ->
                Expect.all
                    [ compare (ADT.makeRotate f v.adt |> ADT.toRecord)
                    , compare (Record.makeRotate f v.record |> Record.toRecord)
                    , compare (Tuple.makeRotate f v.tuple |> Tuple.toRecord)
                    ]
                    (Math.makeRotate f v.math |> Math.toRecord)
        , fuzz recordVec3 "makeScale" <|
            \v ->
                Expect.all
                    [ compare (ADT.makeScale v.adt |> ADT.toRecord)
                    , compare (Record.makeScale v.record |> Record.toRecord)
                    , compare (Tuple.makeScale v.tuple |> Tuple.toRecord)
                    ]
                    (Math.makeScale v.math |> Math.toRecord)
        , fuzz3 Fuzz.float Fuzz.float Fuzz.float "makeScale3" <|
            \f1 f2 f3 ->
                Expect.all
                    [ compare (ADT.makeScale3 f1 f2 f3 |> ADT.toRecord)
                    , compare (Record.makeScale3 f1 f2 f3 |> Record.toRecord)
                    , compare (Tuple.makeScale3 f1 f2 f3 |> Tuple.toRecord)
                    ]
                    (Math.makeScale3 f1 f2 f3 |> Math.toRecord)
        , fuzz recordVec3 "makeTranslate" <|
            \v ->
                Expect.all
                    [ compare (ADT.makeTranslate v.adt |> ADT.toRecord)
                    , compare (Record.makeTranslate v.record |> Record.toRecord)
                    , compare (Tuple.makeTranslate v.tuple |> Tuple.toRecord)
                    ]
                    (Math.makeTranslate v.math |> Math.toRecord)
        , fuzz3 Fuzz.float Fuzz.float Fuzz.float "makeTranslate3" <|
            \f1 f2 f3 ->
                Expect.all
                    [ compare (ADT.makeTranslate3 f1 f2 f3 |> ADT.toRecord)
                    , compare (Record.makeTranslate3 f1 f2 f3 |> Record.toRecord)
                    , compare (Tuple.makeTranslate3 f1 f2 f3 |> Tuple.toRecord)
                    ]
                    (Math.makeTranslate3 f1 f2 f3 |> Math.toRecord)
        ]


compare : Matrix4Record -> Matrix4Record -> Expectation
compare mat1 mat2 =
    Expect.all
        [ .m11 >> Expect.within (Absolute 0.000001) mat1.m11
        , .m21 >> Expect.within (Absolute 0.000001) mat1.m21
        , .m31 >> Expect.within (Absolute 0.000001) mat1.m31
        , .m41 >> Expect.within (Absolute 0.000001) mat1.m41
        , .m12 >> Expect.within (Absolute 0.000001) mat1.m12
        , .m22 >> Expect.within (Absolute 0.000001) mat1.m22
        , .m32 >> Expect.within (Absolute 0.000001) mat1.m32
        , .m42 >> Expect.within (Absolute 0.000001) mat1.m42
        , .m13 >> Expect.within (Absolute 0.000001) mat1.m13
        , .m23 >> Expect.within (Absolute 0.000001) mat1.m23
        , .m33 >> Expect.within (Absolute 0.000001) mat1.m33
        , .m43 >> Expect.within (Absolute 0.000001) mat1.m43
        , .m14 >> Expect.within (Absolute 0.000001) mat1.m14
        , .m24 >> Expect.within (Absolute 0.000001) mat1.m24
        , .m34 >> Expect.within (Absolute 0.000001) mat1.m34
        , .m44 >> Expect.within (Absolute 0.000001) mat1.m44
        ]
        mat2


compareCustom : Float -> Matrix4Record -> Matrix4Record -> Expectation
compareCustom precision mat1 mat2 =
    Expect.all
        [ .m11 >> Expect.within (Absolute precision) mat1.m11
        , .m21 >> Expect.within (Absolute precision) mat1.m21
        , .m31 >> Expect.within (Absolute precision) mat1.m31
        , .m41 >> Expect.within (Absolute precision) mat1.m41
        , .m12 >> Expect.within (Absolute precision) mat1.m12
        , .m22 >> Expect.within (Absolute precision) mat1.m22
        , .m32 >> Expect.within (Absolute precision) mat1.m32
        , .m42 >> Expect.within (Absolute precision) mat1.m42
        , .m13 >> Expect.within (Absolute precision) mat1.m13
        , .m23 >> Expect.within (Absolute precision) mat1.m23
        , .m33 >> Expect.within (Absolute precision) mat1.m33
        , .m43 >> Expect.within (Absolute precision) mat1.m43
        , .m14 >> Expect.within (Absolute precision) mat1.m14
        , .m24 >> Expect.within (Absolute precision) mat1.m24
        , .m34 >> Expect.within (Absolute precision) mat1.m34
        , .m44 >> Expect.within (Absolute precision) mat1.m44
        ]
        mat2


compareVec3 : { x : Float, y : Float, z : Float } -> { x : Float, y : Float, z : Float } -> Expectation
compareVec3 vec1 vec2 =
    Expect.all
        [ .x >> Expect.within (Absolute 0.0000000001) vec1.x
        , .y >> Expect.within (Absolute 0.0000000001) vec1.y
        , .z >> Expect.within (Absolute 0.0000000001) vec1.z
        ]
        vec2



--
--compareVec3Custom precision vec1 vec2 =
--    Expect.all
--        [ .x >> Expect.within (Absolute precision) vec1.x
--        , .y >> Expect.within (Absolute precision) vec1.y
--        , .z >> Expect.within (Absolute precision) vec1.z
--        ]
--        vec2


positive : Fuzz.Fuzzer Float
positive =
    Fuzz.floatRange 0.000001 100


recordVec3 : Fuzz.Fuzzer { adt : ADTVec3.Vec3, record : RecordVec3.Vec3, tuple : TupleVec3.Vec3, math : MathVec3.Vec3 }
recordVec3 =
    Fuzz.map3 (\x y z -> { x = x, y = y, z = z })
        Fuzz.float
        Fuzz.float
        Fuzz.float
        |> Fuzz.map
            (\r ->
                { adt = ADTVec3.fromRecord r
                , record = RecordVec3.fromRecord r
                , tuple = TupleVec3.fromRecord r
                , math = MathVec3.fromRecord r
                }
            )


fuzz6 :
    Fuzz.Fuzzer a5
    -> Fuzz.Fuzzer a4
    -> Fuzz.Fuzzer a3
    -> Fuzz.Fuzzer a2
    -> Fuzz.Fuzzer a1
    -> Fuzz.Fuzzer a
    -> String
    -> (a5 -> a4 -> a3 -> a2 -> a1 -> a -> Expect.Expectation)
    -> Test.Test
fuzz6 a1 a2 a3 a4 a5 a6 desc fn =
    Test.fuzz (Fuzz.map fn a1 |> Fuzz.andMap a2 |> Fuzz.andMap a3 |> Fuzz.andMap a4 |> Fuzz.andMap a5 |> Fuzz.andMap a6) desc identity


fuzz4 :
    Fuzz.Fuzzer a5
    -> Fuzz.Fuzzer a4
    -> Fuzz.Fuzzer a3
    -> Fuzz.Fuzzer a2
    -> String
    -> (a5 -> a4 -> a3 -> a2 -> Expect.Expectation)
    -> Test.Test
fuzz4 a1 a2 a3 a4 desc fn =
    Test.fuzz (Fuzz.map fn a1 |> Fuzz.andMap a2 |> Fuzz.andMap a3 |> Fuzz.andMap a4) desc identity


record : Fuzz.Fuzzer Matrix4Record
record =
    Fuzz.map Matrix4Record Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float
        |> Fuzz.andMap Fuzz.float


type alias Matrix4Record =
    { m11 : Float
    , m21 : Float
    , m31 : Float
    , m41 : Float
    , m12 : Float
    , m22 : Float
    , m32 : Float
    , m42 : Float
    , m13 : Float
    , m23 : Float
    , m33 : Float
    , m43 : Float
    , m14 : Float
    , m24 : Float
    , m34 : Float
    , m44 : Float
    }
