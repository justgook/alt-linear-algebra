module AltMath.Vector4 exposing
    ( Vec4, vec4
    , getX, getY, getZ, getW, setX, setY, setZ, setW
    , add, sub, negate, scale, dot, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , toRecord, fromRecord
    )

{-|


# Create

@docs Vec4, vec4


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, getW, setX, setY, setZ, setW


# Operations

@docs add, sub, negate, scale, dot, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Conversions

@docs toRecord, fromRecord

-}

import AltMath.Record.Vector4 as Vector4


{-| Four dimensional vector type
-}
type alias Vec4 =
    Vector4.Vec4


{-| Creates a new 4-element vector with the given x, y, z, and w values.
-}
vec4 : Float -> Float -> Float -> Float -> Vec4
vec4 =
    Vector4.vec4


{-| Extract the x component of a vector.
-}
getX : Vec4 -> Float
getX =
    Vector4.getX


{-| Extract the y component of a vector.
-}
getY : Vec4 -> Float
getY =
    Vector4.getY


{-| Extract the z component of a vector.
-}
getZ : Vec4 -> Float
getZ =
    Vector4.getZ


{-| Extract the w component of a vector.
-}
getW : Vec4 -> Float
getW =
    Vector4.getW


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec4 -> Vec4
setX =
    Vector4.setX


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec4 -> Vec4
setY =
    Vector4.setY


{-| Update the z component of a vector, returning a new vector.
-}
setZ : Float -> Vec4 -> Vec4
setZ =
    Vector4.setZ


{-| Update the w component of a vector, returning a new vector.
-}
setW : Float -> Vec4 -> Vec4
setW =
    Vector4.setW


{-| Convert a vector to a record.
-}
toRecord : Vec4 -> { x : Float, y : Float, z : Float, w : Float }
toRecord =
    Vector4.toRecord


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float, z : Float, w : Float } -> Vec4
fromRecord =
    Vector4.fromRecord


{-| Vector addition: a + b
-}
add : Vec4 -> Vec4 -> Vec4
add =
    Vector4.add


{-| Vector subtraction: a - b
-}
sub : Vec4 -> Vec4 -> Vec4
sub =
    Vector4.sub


{-| Vector negation: -a
-}
negate : Vec4 -> Vec4
negate =
    Vector4.negate


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec4 -> Vec4 -> Vec4
direction =
    Vector4.direction


{-| The length of the given vector: |a|
-}
length : Vec4 -> Float
length =
    Vector4.length


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec4 -> Float
lengthSquared =
    Vector4.lengthSquared


{-| The distance between two vectors.
-}
distance : Vec4 -> Vec4 -> Float
distance =
    Vector4.distance


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec4 -> Vec4 -> Float
distanceSquared =
    Vector4.distanceSquared


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec4 -> Vec4
normalize =
    Vector4.normalize


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec4 -> Vec4
scale =
    Vector4.scale


{-| The dot product of a and b
-}
dot : Vec4 -> Vec4 -> Float
dot a b =
    a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
