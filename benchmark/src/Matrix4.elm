module Matrix4 exposing (all)

import AltMath.Alternative.ADT.Matrix4 as AdtMat4
import AltMath.Alternative.ADT.Vector3 as AdtVec3
import AltMath.Alternative.Record.Matrix4 as RecordMat4
import AltMath.Alternative.Record.Vector3 as RecordVec3
import AltMath.Alternative.Tuple.Matrix4 as TupleMat4
import AltMath.Alternative.Tuple.Vector3 as TupleVec3
import Benchmark exposing (..)
import Math.Matrix4 as GLMat4
import Math.Vector3 as GLVec3


all { m1, m2 } =
    let
        dataM1 =
            { m11 = m1.m11 |> String.toFloat |> Maybe.withDefault 42
            , m21 = m1.m21 |> String.toFloat |> Maybe.withDefault 42
            , m31 = m1.m31 |> String.toFloat |> Maybe.withDefault 42
            , m41 = m1.m41 |> String.toFloat |> Maybe.withDefault 42
            , m12 = m1.m12 |> String.toFloat |> Maybe.withDefault 42
            , m22 = m1.m22 |> String.toFloat |> Maybe.withDefault 42
            , m32 = m1.m32 |> String.toFloat |> Maybe.withDefault 42
            , m42 = m1.m42 |> String.toFloat |> Maybe.withDefault 42
            , m13 = m1.m13 |> String.toFloat |> Maybe.withDefault 42
            , m23 = m1.m23 |> String.toFloat |> Maybe.withDefault 42
            , m33 = m1.m33 |> String.toFloat |> Maybe.withDefault 42
            , m43 = m1.m43 |> String.toFloat |> Maybe.withDefault 42
            , m14 = m1.m14 |> String.toFloat |> Maybe.withDefault 42
            , m24 = m1.m24 |> String.toFloat |> Maybe.withDefault 42
            , m34 = m1.m34 |> String.toFloat |> Maybe.withDefault 42
            , m44 = m1.m44 |> String.toFloat |> Maybe.withDefault 42
            }

        dataM2 =
            { m11 = m2.m11 |> String.toFloat |> Maybe.withDefault 43
            , m21 = m2.m21 |> String.toFloat |> Maybe.withDefault 43
            , m31 = m2.m31 |> String.toFloat |> Maybe.withDefault 43
            , m41 = m2.m41 |> String.toFloat |> Maybe.withDefault 43
            , m12 = m2.m12 |> String.toFloat |> Maybe.withDefault 43
            , m22 = m2.m22 |> String.toFloat |> Maybe.withDefault 43
            , m32 = m2.m32 |> String.toFloat |> Maybe.withDefault 43
            , m42 = m2.m42 |> String.toFloat |> Maybe.withDefault 43
            , m13 = m2.m13 |> String.toFloat |> Maybe.withDefault 43
            , m23 = m2.m23 |> String.toFloat |> Maybe.withDefault 43
            , m33 = m2.m33 |> String.toFloat |> Maybe.withDefault 43
            , m43 = m2.m43 |> String.toFloat |> Maybe.withDefault 43
            , m14 = m2.m14 |> String.toFloat |> Maybe.withDefault 43
            , m24 = m2.m24 |> String.toFloat |> Maybe.withDefault 43
            , m34 = m2.m34 |> String.toFloat |> Maybe.withDefault 43
            , m44 = m2.m44 |> String.toFloat |> Maybe.withDefault 43
            }

        data1 =
            { glMat1 = GLMat4.fromRecord dataM1
            , glMat2 = GLMat4.fromRecord dataM2
            , adtMat1 = AdtMat4.fromRecord dataM1
            , adtMat2 = AdtMat4.fromRecord dataM2
            , recMat1 = RecordMat4.fromRecord dataM1
            , recMat2 = RecordMat4.fromRecord dataM2
            , tupleMat1 = TupleMat4.fromRecord dataM1
            , tupleMat2 = TupleMat4.fromRecord dataM2
            }

        data2 =
            { glVec1 = GLVec3.vec3 dataM1.m11 dataM1.m21 dataM1.m31
            , glVec2 = GLVec3.vec3 dataM2.m11 dataM2.m21 dataM2.m31
            , glVec3 = GLVec3.vec3 dataM2.m12 dataM2.m22 dataM2.m32
            , adtVec1 = AdtVec3.vec3 dataM1.m11 dataM1.m21 dataM1.m31
            , adtVec2 = AdtVec3.vec3 dataM2.m11 dataM2.m21 dataM2.m31
            , adtVec3 = AdtVec3.vec3 dataM2.m12 dataM2.m22 dataM2.m32
            , recVec1 = RecordVec3.vec3 dataM1.m11 dataM1.m21 dataM1.m31
            , recVec2 = RecordVec3.vec3 dataM2.m11 dataM2.m21 dataM2.m31
            , recVec3 = RecordVec3.vec3 dataM2.m12 dataM2.m22 dataM2.m32
            , tupleVec1 = TupleVec3.vec3 dataM1.m11 dataM1.m21 dataM1.m31
            , tupleVec2 = TupleVec3.vec3 dataM2.m11 dataM2.m21 dataM2.m31
            , tupleVec3 = TupleVec3.vec3 dataM2.m12 dataM2.m22 dataM2.m32
            }
    in
    [ identity
    , inverse data1
    , inverseOrthonormal data1
    , mul data1
    , mulAffine data1
    , transpose data1
    , makeBasis data2
    , transform data1 data2
    , makeFrustum dataM1.m11 dataM1.m21 dataM1.m31 dataM1.m41 dataM1.m12 dataM1.m22
    , makePerspective dataM1.m11 dataM1.m21 dataM1.m31 dataM1.m41
    , makeOrtho dataM1.m11 dataM1.m21 dataM1.m31 dataM1.m41 dataM1.m12 dataM1.m22
    , makeOrtho2D dataM1.m11 dataM1.m21 dataM1.m31 dataM1.m41
    , makeLookAt data2
    , rotate dataM2.m11 data1 data2
    , scale data1 data2
    , scale3 dataM1.m11 dataM1.m21 dataM1.m31 data1
    , translate data1 data2
    , translate3 dataM1.m11 dataM1.m21 dataM1.m31 data1
    , makeRotate dataM1.m11 data2
    , makeScale data2
    , makeScale3 dataM1.m11 dataM1.m21 dataM1.m31
    , makeTranslate data2
    , makeTranslate3 dataM1.m11 dataM1.m21 dataM1.m31
    ]


type Wrap
    = GLMat4 GLMat4.Mat4
    | AdtMat4 AdtMat4.Mat4
    | RecordMat4 RecordMat4.Mat4
    | TupleMat4 TupleMat4.Mat4


type WrapMaybe
    = MaybeGLMat4 (Maybe GLMat4.Mat4)
    | MaybeAdtMat4 (Maybe AdtMat4.Mat4)
    | MaybeRecordMat4 (Maybe RecordMat4.Mat4)
    | MaybeTupleMat4 (Maybe TupleMat4.Mat4)


type WrapМус3
    = GLVec3 GLVec3.Vec3
    | AdtVec3 AdtVec3.Vec3
    | RecordVec3 RecordVec3.Vec3
    | TupleVec3 TupleVec3.Vec3


identity =
    Benchmark.scale "identity"
        [ ( "Linear Algebra", \_ -> GLMat4.identity |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.identity |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.identity |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.identity |> TupleMat4 )
        ]


inverse { glMat1, adtMat1, recMat1, tupleMat1 } =
    Benchmark.scale "inverse"
        [ ( "Linear Algebra", \_ -> GLMat4.inverse glMat1 |> MaybeGLMat4 )
        , ( "ADT", \_ -> AdtMat4.inverse adtMat1 |> MaybeAdtMat4 )
        , ( "Record", \_ -> RecordMat4.inverse recMat1 |> MaybeRecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.inverse tupleMat1 |> MaybeTupleMat4 )
        ]


inverseOrthonormal { glMat1, adtMat1, recMat1, tupleMat1 } =
    Benchmark.scale "inverseOrthonormal"
        [ ( "Linear Algebra", \_ -> GLMat4.inverseOrthonormal glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.inverseOrthonormal adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.inverseOrthonormal recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.inverseOrthonormal tupleMat1 |> TupleMat4 )
        ]


mul { glMat1, glMat2, adtMat1, adtMat2, recMat1, recMat2, tupleMat1, tupleMat2 } =
    Benchmark.scale "mul"
        [ ( "Linear Algebra", \_ -> GLMat4.mul glMat1 glMat2 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.mul adtMat1 adtMat2 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.mul recMat1 recMat2 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.mul tupleMat1 tupleMat2 |> TupleMat4 )
        ]


mulAffine { glMat1, glMat2, adtMat1, adtMat2, recMat1, recMat2, tupleMat1, tupleMat2 } =
    Benchmark.scale "mulAffine"
        [ ( "Linear Algebra", \_ -> GLMat4.mulAffine glMat1 glMat2 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.mulAffine adtMat1 adtMat2 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.mulAffine recMat1 recMat2 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.mulAffine tupleMat1 tupleMat2 |> TupleMat4 )
        ]


transpose { glMat1, glMat2, adtMat1, adtMat2, recMat1, recMat2, tupleMat1, tupleMat2 } =
    Benchmark.scale "transpose"
        [ ( "Linear Algebra", \_ -> GLMat4.transpose glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.transpose adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.transpose recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.transpose tupleMat1 |> TupleMat4 )
        ]


makeBasis { glVec1, glVec2, glVec3, adtVec1, adtVec2, adtVec3, recVec1, recVec2, recVec3, tupleVec1, tupleVec2, tupleVec3 } =
    Benchmark.scale "makeBasis"
        [ ( "Linear Algebra", \_ -> GLMat4.makeBasis glVec1 glVec2 glVec3 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeBasis adtVec1 adtVec2 adtVec3 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeBasis recVec1 recVec2 recVec3 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeBasis tupleVec1 tupleVec2 tupleVec3 |> TupleMat4 )
        ]


transform { glMat1, adtMat1, recMat1, tupleMat1 } { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "transform"
        [ ( "Linear Algebra", \_ -> GLMat4.transform glMat1 glVec1 |> GLVec3 )
        , ( "ADT", \_ -> AdtMat4.transform adtMat1 adtVec1 |> AdtVec3 )
        , ( "Record", \_ -> RecordMat4.transform recMat1 recVec1 |> RecordVec3 )
        , ( "Tuple", \_ -> TupleMat4.transform tupleMat1 tupleVec1 |> TupleVec3 )
        ]


makeFrustum left right bottom top znear zfar =
    Benchmark.scale "makeFrustum"
        [ ( "Linear Algebra", \_ -> GLMat4.makeFrustum left right bottom top znear zfar |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeFrustum left right bottom top znear zfar |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeFrustum left right bottom top znear zfar |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeFrustum left right bottom top znear zfar |> TupleMat4 )
        ]


makePerspective fovy aspect znear zfar =
    Benchmark.scale "makePerspective"
        [ ( "Linear Algebra", \_ -> GLMat4.makePerspective fovy aspect znear zfar |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makePerspective fovy aspect znear zfar |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makePerspective fovy aspect znear zfar |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makePerspective fovy aspect znear zfar |> TupleMat4 )
        ]


makeOrtho left right bottom top znear zfar =
    Benchmark.scale "makeOrtho"
        [ ( "Linear Algebra", \_ -> GLMat4.makeOrtho left right bottom top znear zfar |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeOrtho left right bottom top znear zfar |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeOrtho left right bottom top znear zfar |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeOrtho left right bottom top znear zfar |> TupleMat4 )
        ]


makeOrtho2D left right bottom top =
    Benchmark.scale "makeOrtho2D"
        [ ( "Linear Algebra", \_ -> GLMat4.makeOrtho2D left right bottom top |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeOrtho2D left right bottom top |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeOrtho2D left right bottom top |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeOrtho2D left right bottom top |> TupleMat4 )
        ]


makeLookAt { glVec1, glVec2, glVec3, adtVec1, adtVec2, adtVec3, recVec1, recVec2, recVec3, tupleVec1, tupleVec2, tupleVec3 } =
    Benchmark.scale "makeLookAt"
        [ ( "Linear Algebra", \_ -> GLMat4.makeLookAt glVec1 glVec2 glVec3 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeLookAt adtVec1 adtVec2 adtVec3 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeLookAt recVec1 recVec2 recVec3 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeLookAt tupleVec1 tupleVec2 tupleVec3 |> TupleMat4 )
        ]


rotate angle { glMat1, adtMat1, recMat1, tupleMat1 } { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "rotate"
        [ ( "Linear Algebra", \_ -> GLMat4.rotate angle glVec1 glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.rotate angle adtVec1 adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.rotate angle recVec1 recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.rotate angle tupleVec1 tupleMat1 |> TupleMat4 )
        ]


scale { glMat1, adtMat1, recMat1, tupleMat1 } { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "scale"
        [ ( "Linear Algebra", \_ -> GLMat4.scale glVec1 glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.scale adtVec1 adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.scale recVec1 recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.scale tupleVec1 tupleMat1 |> TupleMat4 )
        ]


scale3 x y z { glMat1, adtMat1, recMat1, tupleMat1 } =
    Benchmark.scale "scale3"
        [ ( "Linear Algebra", \_ -> GLMat4.scale3 x y z glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.scale3 x y z adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.scale3 x y z recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.scale3 x y z tupleMat1 |> TupleMat4 )
        ]


translate { glMat1, adtMat1, recMat1, tupleMat1 } { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "translate"
        [ ( "Linear Algebra", \_ -> GLMat4.translate glVec1 glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.translate adtVec1 adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.translate recVec1 recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.translate tupleVec1 tupleMat1 |> TupleMat4 )
        ]


translate3 x y z { glMat1, adtMat1, recMat1, tupleMat1 } =
    Benchmark.scale "translate3"
        [ ( "Linear Algebra", \_ -> GLMat4.translate3 x y z glMat1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.translate3 x y z adtMat1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.translate3 x y z recMat1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.translate3 x y z tupleMat1 |> TupleMat4 )
        ]


makeRotate angle { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "makeRotate"
        [ ( "Linear Algebra", \_ -> GLMat4.makeRotate angle glVec1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeRotate angle adtVec1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeRotate angle recVec1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeRotate angle tupleVec1 |> TupleMat4 )
        ]


makeScale { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "makeScale"
        [ ( "Linear Algebra", \_ -> GLMat4.makeScale glVec1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeScale adtVec1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeScale recVec1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeScale tupleVec1 |> TupleMat4 )
        ]


makeScale3 x y z =
    Benchmark.scale "makeScale3"
        [ ( "Linear Algebra", \_ -> GLMat4.makeScale3 x y z |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeScale3 x y z |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeScale3 x y z |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeScale3 x y z |> TupleMat4 )
        ]


makeTranslate { glVec1, adtVec1, recVec1, tupleVec1 } =
    Benchmark.scale "makeTranslate"
        [ ( "Linear Algebra", \_ -> GLMat4.makeTranslate glVec1 |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeTranslate adtVec1 |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeTranslate recVec1 |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeTranslate tupleVec1 |> TupleMat4 )
        ]


makeTranslate3 x y z =
    Benchmark.scale "makeTranslate3"
        [ ( "Linear Algebra", \_ -> GLMat4.makeTranslate3 x y z |> GLMat4 )
        , ( "ADT", \_ -> AdtMat4.makeTranslate3 x y z |> AdtMat4 )
        , ( "Record", \_ -> RecordMat4.makeTranslate3 x y z |> RecordMat4 )
        , ( "Tuple", \_ -> TupleMat4.makeTranslate3 x y z |> TupleMat4 )
        ]
