module AltMath.Vector2 exposing
    ( Vec2, vec2
    , getX, getY, setX, setY
    , add, sub, negate, scale, dot, normalize, direction, mul
    , length, lengthSquared, distance, distanceSquared
    , toRecord, fromRecord
    )

{-|


# Create

@docs Vec2, vec2


# Get and Set

The set functions create a new copy of the vector, updating a single field.

@docs getX, getY, setX, setY


# Operations

@docs add, sub, negate, scale, dot, normalize, direction, mul
@docs length, lengthSquared, distance, distanceSquared


# Conversions

@docs toRecord, fromRecord

-}

import AltMath.Record.Vector2 as Vector2


{-| Two dimensional vector type
-}
type alias Vec2 =
    Vector2.Vec2


{-| Creates a new 2-element vector with the given values.
-}
vec2 : Float -> Float -> Vec2
vec2 =
    Vector2.vec2


{-| Extract the x component of a vector.
-}
getX : Vec2 -> Float
getX =
    Vector2.getX


{-| Extract the y component of a vector.
-}
getY : Vec2 -> Float
getY =
    Vector2.getY


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec2 -> Vec2
setX =
    Vector2.setX


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec2 -> Vec2
setY =
    Vector2.setY


{-| Convert a vector to a record.
-}
toRecord : Vec2 -> { x : Float, y : Float }
toRecord =
    Vector2.toRecord


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float } -> Vec2
fromRecord =
    Vector2.fromRecord


{-| Vector addition: a + b
-}
add : Vec2 -> Vec2 -> Vec2
add =
    Vector2.add


{-| Vector subtraction: a - b
-}
sub : Vec2 -> Vec2 -> Vec2
sub =
    Vector2.sub


{-| Vector negation: -a
-}
negate : Vec2 -> Vec2
negate =
    Vector2.negate


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec2 -> Vec2 -> Vec2
direction =
    Vector2.direction


{-| The length of the given vector: |a|
-}
length : Vec2 -> Float
length =
    Vector2.length


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec2 -> Float
lengthSquared =
    Vector2.lengthSquared


{-| The distance between two vectors.
-}
distance : Vec2 -> Vec2 -> Float
distance =
    Vector2.distance


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared =
    Vector2.distanceSquared


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec2 -> Vec2
normalize =
    Vector2.normalize


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec2 -> Vec2
scale =
    Vector2.scale


{-| The dot product of a and b
-}
dot : Vec2 -> Vec2 -> Float
dot =
    Vector2.dot


{-| Multiply the vector by a vector: a \* b
-}
mul : Vec2 -> Vec2 -> Vec2
mul =
    Vector2.mul
