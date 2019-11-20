module AltMath.Advanced.Tuple.Matrix4 exposing
    ( Mat4, identity
    , inverse, inverseOrthonormal, mul, mulAffine, transpose, makeBasis, transform
    , makeFrustum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt
    , rotate, scale, scale3, translate, translate3
    , makeRotate, makeScale, makeScale3, makeTranslate, makeTranslate3
    , toRecord, fromRecord, Matrix4Record
    )

{-| This library uses the convention that the prefix `make` is creating a new
array,as without the prefix, you are applying some transform to an
existing matrix.


# Create

@docs Mat4, identity


# Operations

@docs inverse, inverseOrthonormal, mul, mulAffine, transpose, makeBasis, transform


# Projections

@docs makeFrustum, makePerspective, makeOrtho, makeOrtho2D, makeLookAt


# Apply Transformations

@docs rotate, scale, scale3, translate, translate3


# Create Transformations

@docs makeRotate, makeScale, makeScale3, makeTranslate, makeTranslate3


# Conversions

@docs toRecord, fromRecord, Matrix4Record

-}

import AltMath.Advanced.Tuple.Vector3 as Vec3 exposing (Vec3)


{-| 4x4 matrix type
-}
type alias Mat4 =
    ( ( ( ( Float, Float ), ( Float, Float ) ), ( ( Float, Float ), ( Float, Float ) ) ), ( ( ( Float, Float ), ( Float, Float ) ), ( ( Float, Float ), ( Float, Float ) ) ) )


{-| Multiply a vector by a 4x4 matrix: m \* v
-}
transform : Mat4 -> Vec3 -> Vec3
transform ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) ( x, y, z ) =
    let
        w =
            x * m41 + y * m42 + z * m43 + m44
    in
    ( (m11 * x + m12 * y + m13 * z + m14) / w
    , (m21 * x + m22 * y + m23 * z + m24) / w
    , (m31 * x + m32 * y + m33 * z + m34) / w
    )


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat4
identity =
    ( ( ( ( 1, 0 ), ( 0, 0 ) ), ( ( 0, 1 ), ( 0, 0 ) ) ), ( ( ( 0, 0 ), ( 1, 0 ) ), ( ( 0, 0 ), ( 0, 1 ) ) ) )


{-| Computes the inverse of any matrix. This is somewhat computationally
intensive. If the matrix is not invertible, `Nothing` is returned.
-}
inverse : Mat4 -> Maybe Mat4
inverse ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    let
        r11 =
            m22 * m33 * m44 - m22 * m43 * m34 - m23 * m32 * m44 + m23 * m42 * m34 + m24 * m32 * m43 - m24 * m42 * m33

        r12 =
            -m12 * m33 * m44 + m12 * m43 * m34 + m13 * m32 * m44 - m13 * m42 * m34 - m14 * m32 * m43 + m14 * m42 * m33

        r13 =
            m12 * m23 * m44 - m12 * m43 * m24 - m13 * m22 * m44 + m13 * m42 * m24 + m14 * m22 * m43 - m14 * m42 * m23

        r14 =
            -m12 * m23 * m34 + m12 * m33 * m24 + m13 * m22 * m34 - m13 * m32 * m24 - m14 * m22 * m33 + m14 * m32 * m23

        r21 =
            -m21 * m33 * m44 + m21 * m43 * m34 + m23 * m31 * m44 - m23 * m41 * m34 - m24 * m31 * m43 + m24 * m41 * m33

        r22 =
            m11 * m33 * m44 - m11 * m43 * m34 - m13 * m31 * m44 + m13 * m41 * m34 + m14 * m31 * m43 - m14 * m41 * m33

        r23 =
            -m11 * m23 * m44 + m11 * m43 * m24 + m13 * m21 * m44 - m13 * m41 * m24 - m14 * m21 * m43 + m14 * m41 * m23

        r24 =
            m11 * m23 * m34 - m11 * m33 * m24 - m13 * m21 * m34 + m13 * m31 * m24 + m14 * m21 * m33 - m14 * m31 * m23

        r31 =
            m21 * m32 * m44 - m21 * m42 * m34 - m22 * m31 * m44 + m22 * m41 * m34 + m24 * m31 * m42 - m24 * m41 * m32

        r32 =
            -m11 * m32 * m44 + m11 * m42 * m34 + m12 * m31 * m44 - m12 * m41 * m34 - m14 * m31 * m42 + m14 * m41 * m32

        r33 =
            m11 * m22 * m44 - m11 * m42 * m24 - m12 * m21 * m44 + m12 * m41 * m24 + m14 * m21 * m42 - m14 * m41 * m22

        r34 =
            -m11 * m22 * m34 + m11 * m32 * m24 + m12 * m21 * m34 - m12 * m31 * m24 - m14 * m21 * m32 + m14 * m31 * m22

        r41 =
            -m21 * m32 * m43 + m21 * m42 * m33 + m22 * m31 * m43 - m22 * m41 * m33 - m23 * m31 * m42 + m23 * m41 * m32

        r42 =
            m11 * m32 * m43 - m11 * m42 * m33 - m12 * m31 * m43 + m12 * m41 * m33 + m13 * m31 * m42 - m13 * m41 * m32

        r43 =
            -m11 * m22 * m43 + m11 * m42 * m23 + m12 * m21 * m43 - m12 * m41 * m23 - m13 * m21 * m42 + m13 * m41 * m22

        r44 =
            m11 * m22 * m33 - m11 * m32 * m23 - m12 * m21 * m33 + m12 * m31 * m23 + m13 * m21 * m32 - m13 * m31 * m22

        det =
            m11 * r11 + m21 * r12 + m31 * r13 + m41 * r14

        idet =
            1 / det
    in
    if det == 0 then
        Nothing

    else
        Just ( ( ( ( r11 * idet, r21 * idet ), ( r31 * idet, r41 * idet ) ), ( ( r12 * idet, r22 * idet ), ( r32 * idet, r42 * idet ) ) ), ( ( ( r13 * idet, r23 * idet ), ( r33 * idet, r43 * idet ) ), ( ( r14 * idet, r24 * idet ), ( r34 * idet, r44 * idet ) ) ) )


{-| Computes the inverse of the given matrix, assuming that the matrix is
orthonormal. This algorithm is more efficient than general matrix inversion, and
has no possibility of failing.
-}
inverseOrthonormal : Mat4 -> Mat4
inverseOrthonormal ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11
    , m21 = m12
    , m31 = m13
    , m41 = 0
    , m12 = m21
    , m22 = m22
    , m32 = m23
    , m42 = 0
    , m13 = m31
    , m23 = m32
    , m33 = m33
    , m43 = 0
    , m14 = -(m11 * m14 + m21 * m24 + m31 * m34)
    , m24 = -(m12 * m14 + m22 * m24 + m32 * m34)
    , m34 = -(m13 * m14 + m23 * m24 + m33 * m34)
    , m44 = m44
    }
        |> fromRecord


{-| Creates a matrix for a projection frustum with the given parameters.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum
  - znear - the near z distance of the frustum
  - zfar - the far z distance of the frustum

-}
makeFrustum : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeFrustum left right bottom top znear zfar =
    { m11 = 2 * znear / (right - left)
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 2 * znear / (top - bottom)
    , m32 = 0
    , m42 = 0
    , m13 = (right + left) / (right - left)
    , m23 = (top + bottom) / (top - bottom)
    , m33 = -(zfar + znear) / (zfar - znear)
    , m43 = -1
    , m14 = 0
    , m24 = 0
    , m34 = -2 * zfar * znear / (zfar - znear)
    , m44 = 0
    }
        |> fromRecord


{-| Creates a matrix for a perspective projection with the given parameters.

Parameters:

  - fovy - field of view in the y axis, in degrees
  - aspect - aspect ratio
  - znear - the near z distance of the projection
  - zfar - the far z distance of the projection

-}
makePerspective : Float -> Float -> Float -> Float -> Mat4
makePerspective fovy aspect znear zfar =
    let
        ymax =
            znear * tan (fovy * pi / 360.0)

        ymin =
            -ymax

        xmin =
            ymin * aspect

        xmax =
            ymax * aspect
    in
    makeFrustum xmin xmax ymin ymax znear zfar


{-| Creates a matrix for an orthogonal frustum projection with the given parameters.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum
  - znear - the near z distance of the frustum
  - zfar - the far z distance of the frustum

-}
makeOrtho : Float -> Float -> Float -> Float -> Float -> Float -> Mat4
makeOrtho left right bottom top znear zfar =
    { m11 = 2 / (right - left)
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 2 / (top - bottom)
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = -2 / (zfar - znear)
    , m43 = 0
    , m14 = -(right + left) / (right - left)
    , m24 = -(top + bottom) / (top - bottom)
    , m34 = -(zfar + znear) / (zfar - znear)
    , m44 = 1
    }
        |> fromRecord


{-| Creates a matrix for a 2D orthogonal frustum projection with the given
parameters. `znear` and `zfar` are assumed to be -1 and 1, respectively.

Parameters:

  - left - the left coordinate of the frustum
  - right- the right coordinate of the frustum
  - bottom - the bottom coordinate of the frustum
  - top - the top coordinate of the frustum

-}
makeOrtho2D : Float -> Float -> Float -> Float -> Mat4
makeOrtho2D left right bottom top =
    makeOrtho left right bottom top -1 1


{-| Matrix multiplcation: a \* b
-}
mul : Mat4 -> Mat4 -> Mat4
mul ( ( ( ( am11, am21 ), ( am31, am41 ) ), ( ( am12, am22 ), ( am32, am42 ) ) ), ( ( ( am13, am23 ), ( am33, am43 ) ), ( ( am14, am24 ), ( am34, am44 ) ) ) ) ( ( ( ( bm11, bm21 ), ( bm31, bm41 ) ), ( ( bm12, bm22 ), ( bm32, bm42 ) ) ), ( ( ( bm13, bm23 ), ( bm33, bm43 ) ), ( ( bm14, bm24 ), ( bm34, bm44 ) ) ) ) =
    { m11 = am11 * bm11 + am12 * bm21 + am13 * bm31 + am14 * bm41
    , m21 = am21 * bm11 + am22 * bm21 + am23 * bm31 + am24 * bm41
    , m31 = am31 * bm11 + am32 * bm21 + am33 * bm31 + am34 * bm41
    , m41 = am41 * bm11 + am42 * bm21 + am43 * bm31 + am44 * bm41
    , m12 = am11 * bm12 + am12 * bm22 + am13 * bm32 + am14 * bm42
    , m22 = am21 * bm12 + am22 * bm22 + am23 * bm32 + am24 * bm42
    , m32 = am31 * bm12 + am32 * bm22 + am33 * bm32 + am34 * bm42
    , m42 = am41 * bm12 + am42 * bm22 + am43 * bm32 + am44 * bm42
    , m13 = am11 * bm13 + am12 * bm23 + am13 * bm33 + am14 * bm43
    , m23 = am21 * bm13 + am22 * bm23 + am23 * bm33 + am24 * bm43
    , m33 = am31 * bm13 + am32 * bm23 + am33 * bm33 + am34 * bm43
    , m43 = am41 * bm13 + am42 * bm23 + am43 * bm33 + am44 * bm43
    , m14 = am11 * bm14 + am12 * bm24 + am13 * bm34 + am14 * bm44
    , m24 = am21 * bm14 + am22 * bm24 + am23 * bm34 + am24 * bm44
    , m34 = am31 * bm14 + am32 * bm24 + am33 * bm34 + am34 * bm44
    , m44 = am41 * bm14 + am42 * bm24 + am43 * bm34 + am44 * bm44
    }
        |> fromRecord


{-| Matrix multiplication, assuming a and b are affine: a \* b
-}
mulAffine : Mat4 -> Mat4 -> Mat4
mulAffine ( ( ( ( am11, am21 ), ( am31, am41 ) ), ( ( am12, am22 ), ( am32, am42 ) ) ), ( ( ( am13, am23 ), ( am33, am43 ) ), ( ( am14, am24 ), ( am34, am44 ) ) ) ) ( ( ( ( bm11, bm21 ), ( bm31, bm41 ) ), ( ( bm12, bm22 ), ( bm32, bm42 ) ) ), ( ( ( bm13, bm23 ), ( bm33, bm43 ) ), ( ( bm14, bm24 ), ( bm34, bm44 ) ) ) ) =
    { m11 = am11 * bm11 + am12 * bm21 + am13 * bm31
    , m21 = am21 * bm11 + am22 * bm21 + am23 * bm31
    , m31 = am31 * bm11 + am32 * bm21 + am33 * bm31
    , m41 = 0
    , m12 = am11 * bm12 + am12 * bm22 + am13 * bm32
    , m22 = am21 * bm12 + am22 * bm22 + am23 * bm32
    , m32 = am31 * bm12 + am32 * bm22 + am33 * bm32
    , m42 = 0
    , m13 = am11 * bm13 + am12 * bm23 + am13 * bm33
    , m23 = am21 * bm13 + am22 * bm23 + am23 * bm33
    , m33 = am31 * bm13 + am32 * bm23 + am33 * bm33
    , m43 = 0
    , m14 = am11 * bm14 + am12 * bm24 + am13 * bm34 + am14
    , m24 = am21 * bm14 + am22 * bm24 + am23 * bm34 + am24
    , m34 = am31 * bm14 + am32 * bm24 + am33 * bm34 + am34
    , m44 = 1
    }
        |> fromRecord


{-| Creates a transformation matrix for rotation in radians about the
3-element vector axis.
-}
makeRotate : Float -> Vec3 -> Mat4
makeRotate angle axis =
    let
        ( x, y, z ) =
            Vec3.normalize axis

        c =
            cos angle

        c1 =
            1 - c

        s =
            sin angle
    in
    { m11 = x * x * c1 + c
    , m21 = y * x * c1 + z * s
    , m31 = z * x * c1 - y * s
    , m41 = 0
    , m12 = x * y * c1 - z * s
    , m22 = y * y * c1 + c
    , m32 = y * z * c1 + x * s
    , m42 = 0
    , m13 = x * z * c1 + y * s
    , m23 = y * z * c1 - x * s
    , m33 = z * z * c1 + c
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }
        |> fromRecord


{-| Concatenates a rotation in radians about an axis to the given matrix.
-}
rotate : Float -> Vec3 -> Mat4 -> Mat4
rotate angle (( axisx, axisy, axisz ) as axis) ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    let
        im =
            1.0 / Vec3.length axis

        x =
            axisx * im

        y =
            axisy * im

        z =
            axisz * im

        c =
            cos angle

        c1 =
            1 - c

        s =
            sin angle

        xs =
            x * s

        ys =
            y * s

        zs =
            z * s

        xyc1 =
            x * y * c1

        xzc1 =
            x * z * c1

        yzc1 =
            y * z * c1

        t11 =
            x * x * c1 + c

        t21 =
            xyc1 + zs

        t31 =
            xzc1 - ys

        t12 =
            xyc1 - zs

        t22 =
            y * y * c1 + c

        t32 =
            yzc1 + xs

        t13 =
            xzc1 + ys

        t23 =
            yzc1 - xs

        t33 =
            z * z * c1 + c
    in
    { m11 = m11 * t11 + m12 * t21 + m13 * t31
    , m21 = m21 * t11 + m22 * t21 + m23 * t31
    , m31 = m31 * t11 + m32 * t21 + m33 * t31
    , m41 = m41 * t11 + m42 * t21 + m43 * t31
    , m12 = m11 * t12 + m12 * t22 + m13 * t32
    , m22 = m21 * t12 + m22 * t22 + m23 * t32
    , m32 = m31 * t12 + m32 * t22 + m33 * t32
    , m42 = m41 * t12 + m42 * t22 + m43 * t32
    , m13 = m11 * t13 + m12 * t23 + m13 * t33
    , m23 = m21 * t13 + m22 * t23 + m23 * t33
    , m33 = m31 * t13 + m32 * t23 + m33 * t33
    , m43 = m41 * t13 + m42 * t23 + m43 * t33
    , m14 = m14
    , m24 = m24
    , m34 = m34
    , m44 = m44
    }
        |> fromRecord


{-| Creates a transformation matrix for scaling by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeScale3 : Float -> Float -> Float -> Mat4
makeScale3 x y z =
    { m11 = x
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = y
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = z
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }
        |> fromRecord


{-| Creates a transformation matrix for scaling each of the x, y, and z axes by
the amount given in the corresponding element of the 3-element vector.
-}
makeScale : Vec3 -> Mat4
makeScale ( x, y, z ) =
    { m11 = x
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = y
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = z
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }
        |> fromRecord


{-| Concatenates a scaling to the given matrix.
-}
scale3 : Float -> Float -> Float -> Mat4 -> Mat4
scale3 x y z ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11 * x
    , m21 = m21 * x
    , m31 = m31 * x
    , m41 = m41 * x
    , m12 = m12 * y
    , m22 = m22 * y
    , m32 = m32 * y
    , m42 = m42 * y
    , m13 = m13 * z
    , m23 = m23 * z
    , m33 = m33 * z
    , m43 = m43 * z
    , m14 = m14
    , m24 = m24
    , m34 = m34
    , m44 = m44
    }
        |> fromRecord


{-| Concatenates a scaling to the given matrix.
-}
scale : Vec3 -> Mat4 -> Mat4
scale ( x, y, z ) ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11 * x
    , m21 = m21 * x
    , m31 = m31 * x
    , m41 = m41 * x
    , m12 = m12 * y
    , m22 = m22 * y
    , m32 = m32 * y
    , m42 = m42 * y
    , m13 = m13 * z
    , m23 = m23 * z
    , m33 = m33 * z
    , m43 = m43 * z
    , m14 = m14
    , m24 = m24
    , m34 = m34
    , m44 = m44
    }
        |> fromRecord


{-| Creates a transformation matrix for translating by 3 scalar values, one for
each of the x, y, and z directions.
-}
makeTranslate3 : Float -> Float -> Float -> Mat4
makeTranslate3 x y z =
    { m11 = 1
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 1
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = 1
    , m43 = 0
    , m14 = x
    , m24 = y
    , m34 = z
    , m44 = 1
    }
        |> fromRecord


{-| Creates a transformation matrix for translating each of the x, y, and z
axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Vec3 -> Mat4
makeTranslate ( x, y, z ) =
    { m11 = 1
    , m21 = 0
    , m31 = 0
    , m41 = 0
    , m12 = 0
    , m22 = 1
    , m32 = 0
    , m42 = 0
    , m13 = 0
    , m23 = 0
    , m33 = 1
    , m43 = 0
    , m14 = x
    , m24 = y
    , m34 = z
    , m44 = 1
    }
        |> fromRecord


{-| Concatenates a translation to the given matrix.
-}
translate3 : Float -> Float -> Float -> Mat4 -> Mat4
translate3 x y z ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11
    , m21 = m21
    , m31 = m31
    , m41 = m41
    , m12 = m12
    , m22 = m22
    , m32 = m32
    , m42 = m42
    , m13 = m13
    , m23 = m23
    , m33 = m33
    , m43 = m43
    , m14 = m11 * x + m12 * y + m13 * z + m14
    , m24 = m21 * x + m22 * y + m23 * z + m24
    , m34 = m31 * x + m32 * y + m33 * z + m34
    , m44 = m41 * x + m42 * y + m43 * z + m44
    }
        |> fromRecord


{-| Concatenates a translation to the given matrix.
-}
translate : Vec3 -> Mat4 -> Mat4
translate ( x, y, z ) ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11
    , m21 = m21
    , m31 = m31
    , m41 = m41
    , m12 = m12
    , m22 = m22
    , m32 = m32
    , m42 = m42
    , m13 = m13
    , m23 = m23
    , m33 = m33
    , m43 = m43
    , m14 = m11 * x + m12 * y + m13 * z + m14
    , m24 = m21 * x + m22 * y + m23 * z + m24
    , m34 = m31 * x + m32 * y + m33 * z + m34
    , m44 = m41 * x + m42 * y + m43 * z + m44
    }
        |> fromRecord


{-| Creates a transformation matrix for a camera.

Parameters:

  - eye - The location of the camera
  - center - The location of the focused object
  - up - The "up" direction according to the camera

-}
makeLookAt : Vec3 -> Vec3 -> Vec3 -> Mat4
makeLookAt (( eyex, eyey, eyez ) as eye) center up =
    let
        (( zx, zy, zz ) as z) =
            Vec3.direction eye center

        (( xx, xy, xz ) as x) =
            Vec3.normalize (Vec3.cross up z)

        ( yx, yy, yz ) =
            Vec3.normalize (Vec3.cross z x)
    in
    mul
        (fromRecord
            { m11 = xx
            , m21 = yx
            , m31 = zx
            , m41 = 0
            , m12 = xy
            , m22 = yy
            , m32 = zy
            , m42 = 0
            , m13 = xz
            , m23 = yz
            , m33 = zz
            , m43 = 0
            , m14 = 0
            , m24 = 0
            , m34 = 0
            , m44 = 1
            }
        )
        (fromRecord
            { m11 = 1
            , m21 = 0
            , m31 = 0
            , m41 = 0
            , m12 = 0
            , m22 = 1
            , m32 = 0
            , m42 = 0
            , m13 = 0
            , m23 = 0
            , m33 = 1
            , m43 = 0
            , m14 = -eyex
            , m24 = -eyey
            , m34 = -eyez
            , m44 = 1
            }
        )


{-| "Flip" the matrix across the diagonal by swapping row index and column
index.
-}
transpose : Mat4 -> Mat4
transpose ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11
    , m21 = m12
    , m31 = m13
    , m41 = m14
    , m12 = m21
    , m22 = m22
    , m32 = m23
    , m42 = m24
    , m13 = m31
    , m23 = m32
    , m33 = m33
    , m43 = m34
    , m14 = m41
    , m24 = m42
    , m34 = m43
    , m44 = m44
    }
        |> fromRecord


{-| Creates a transform from a basis consisting of 3 linearly independent vectors.
-}
makeBasis : Vec3 -> Vec3 -> Vec3 -> Mat4
makeBasis ( v1x, v1y, v1z ) ( v2x, v2y, v2z ) ( v3x, v3y, v3z ) =
    { m11 = v1x
    , m21 = v1y
    , m31 = v1z
    , m41 = 0
    , m12 = v2x
    , m22 = v2y
    , m32 = v2z
    , m42 = 0
    , m13 = v3x
    , m23 = v3y
    , m33 = v3z
    , m43 = 0
    , m14 = 0
    , m24 = 0
    , m34 = 0
    , m44 = 1
    }
        |> fromRecord


{-| Convert a matrix to a record. Elements are given by their row and column indices, starting at 1, so `m23` means the element in the second row, third column.
-}
toRecord : Mat4 -> Matrix4Record
toRecord ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) ) =
    { m11 = m11, m21 = m21, m31 = m31, m41 = m41, m12 = m12, m22 = m22, m32 = m32, m42 = m42, m13 = m13, m23 = m23, m33 = m33, m43 = m43, m14 = m14, m24 = m24, m34 = m34, m44 = m44 }


{-| Convert a record to a matrix.
-}
fromRecord : Matrix4Record -> Mat4
fromRecord { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    ( ( ( ( m11, m21 ), ( m31, m41 ) ), ( ( m12, m22 ), ( m32, m42 ) ) ), ( ( ( m13, m23 ), ( m33, m43 ) ), ( ( m14, m24 ), ( m34, m44 ) ) ) )


{-| Just type alias for matrix record
-}
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
