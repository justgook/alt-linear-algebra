module AltMath.Vector3 exposing
    ( Vec3, vec3, i, j, k
    , getX, getY, getZ, setX, setY, setZ
    , add, sub, negate, scale, dot, cross, normalize, direction
    , length, lengthSquared, distance, distanceSquared
    , toRecord, fromRecord
    )

{-|


# Create

@docs Vec3, vec3, i, j, k


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, getZ, setX, setY, setZ


# Operations

@docs add, sub, negate, scale, dot, cross, normalize, direction
@docs length, lengthSquared, distance, distanceSquared


# Conversions

@docs toRecord, fromRecord

-}

import AltMath.Record.Vector3 as Vector3


{-| Three dimensional vector type
-}
type alias Vec3 =
    Vector3.Vec3


{-| Creates a new 3-element vector with the given values.
-}
vec3 : Float -> Float -> Float -> Vec3
vec3 =
    Vector3.vec3


{-| The unit vector &icirc; which points in the x direction: `vec3 1 0 0`
-}
i : Vec3
i =
    Vector3.i


{-| The unit vector &jcirc; which points in the y direction: `vec3 0 1 0`
-}
j : Vec3
j =
    Vector3.j


{-| The unit vector k&#0770; which points in the z direction: `vec3 0 0 1`
-}
k : Vec3
k =
    Vector3.k


{-| Extract the x component of a vector.
-}
getX : Vec3 -> Float
getX =
    Vector3.getX


{-| Extract the y component of a vector.
-}
getY : Vec3 -> Float
getY =
    Vector3.getY


{-| Extract the z component of a vector.
-}
getZ : Vec3 -> Float
getZ =
    Vector3.getZ


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec3 -> Vec3
setX x { y, z } =
    { x = x, y = y, z = z }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec3 -> Vec3
setY y { x, z } =
    { x = x, y = y, z = z }


{-| Update the z component of a vector, returning a new vector.
-}
setZ : Float -> Vec3 -> Vec3
setZ z { x, y } =
    { x = x, y = y, z = z }


{-| Convert a vector to a record.
-}
toRecord : Vec3 -> { x : Float, y : Float, z : Float }
toRecord =
    Vector3.toRecord


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float, z : Float } -> Vec3
fromRecord =
    Vector3.fromRecord


{-| Vector addition: a + b
-}
add : Vec3 -> Vec3 -> Vec3
add =
    Vector3.add


{-| Vector subtraction: a - b
-}
sub : Vec3 -> Vec3 -> Vec3
sub =
    Vector3.sub


{-| Vector negation: -a
-}
negate : Vec3 -> Vec3
negate =
    Vector3.negate


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec3 -> Vec3 -> Vec3
direction =
    Vector3.direction


{-| The length of the given vector: |a|
-}
length : Vec3 -> Float
length =
    Vector3.length


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec3 -> Float
lengthSquared =
    Vector3.lengthSquared


{-| The distance between two vectors.
-}
distance : Vec3 -> Vec3 -> Float
distance =
    Vector3.distance


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec3 -> Vec3 -> Float
distanceSquared =
    Vector3.distanceSquared


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec3 -> Vec3
normalize =
    Vector3.normalize


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec3 -> Vec3
scale =
    Vector3.scale


{-| The dot product of a and b
-}
dot : Vec3 -> Vec3 -> Float
dot =
    Vector3.dot


{-| The cross product of a and b
-}
cross : Vec3 -> Vec3 -> Vec3
cross =
    Vector3.cross
