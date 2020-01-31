module AltMath.Vector2 exposing
    ( Vec2, vec2
    , getX, getY, setX, setY
    , add, sub, negate, scale, dot, normalize, direction, mul
    , length, lengthSquared, distance, distanceSquared
    , max, projection
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
@docs max, projection


# Conversions

@docs toRecord, fromRecord

-}


{-| Two dimensional vector type
-}
type alias Vec2 =
    { x : Float, y : Float }


{-| Creates a new 2-element vector with the given values.
-}
vec2 : Float -> Float -> Vec2
vec2 x y =
    { x = x, y = y }


{-| Extract the x component of a vector.
-}
getX : Vec2 -> Float
getX =
    .x


{-| Extract the y component of a vector.
-}
getY : Vec2 -> Float
getY =
    .y


{-| Update the x component of a vector, returning a new vector.
-}
setX : Float -> Vec2 -> Vec2
setX x { y } =
    { x = x, y = y }


{-| Update the y component of a vector, returning a new vector.
-}
setY : Float -> Vec2 -> Vec2
setY y { x } =
    { x = x, y = y }


{-| Convert a vector to a record.
-}
toRecord : Vec2 -> { x : Float, y : Float }
toRecord =
    identity


{-| Convert a record to a vector.
-}
fromRecord : { x : Float, y : Float } -> Vec2
fromRecord =
    identity


{-| Vector addition: a + b
-}
add : Vec2 -> Vec2 -> Vec2
add a b =
    { x = a.x + b.x, y = a.y + b.y }


{-| Vector subtraction: a - b
-}
sub : Vec2 -> Vec2 -> Vec2
sub a b =
    { x = a.x - b.x, y = a.y - b.y }


{-| Vector negation: -a
-}
negate : Vec2 -> Vec2
negate a =
    { x = -a.x, y = -a.y }


{-| The normalized direction from b to a: (a - b) / |a - b|
-}
direction : Vec2 -> Vec2 -> Vec2
direction a b =
    let
        c =
            { x = a.x - b.x, y = a.y - b.y }

        len =
            sqrt (c.x * c.x + c.y * c.y)
    in
    { x = c.x / len, y = c.y / len }


{-| The length of the given vector: |a|
-}
length : Vec2 -> Float
length { x, y } =
    sqrt (x * x + y * y)


{-| The square of the length of the given vector: |a| \* |a|
-}
lengthSquared : Vec2 -> Float
lengthSquared { x, y } =
    x * x + y * y


{-| The distance between two vectors.
-}
distance : Vec2 -> Vec2 -> Float
distance a b =
    let
        c =
            { x = a.x - b.x, y = a.y - b.y }
    in
    sqrt (c.x * c.x + c.y * c.y)


{-| The square of the distance between two vectors.
-}
distanceSquared : Vec2 -> Vec2 -> Float
distanceSquared a b =
    let
        c =
            { x = a.x - b.x, y = a.y - b.y }
    in
    c.x * c.x + c.y * c.y


{-| Get longest vector of two
-}
max : Vec2 -> Vec2 -> Vec2
max a b =
    if lengthSquared a > lengthSquared b then
        a

    else
        b


{-| Projecting one vector onto another
-}
projection : Vec2 -> Vec2 -> Vec2
projection a b =
    scale (dot a b / lengthSquared b) b


{-| A unit vector with the same direction as the given vector: a / |a|
-}
normalize : Vec2 -> Vec2
normalize v2 =
    let
        len =
            length v2
    in
    { x = v2.x / len, y = v2.y / len }


{-| Multiply the vector by a scalar: s \* v
-}
scale : Float -> Vec2 -> Vec2
scale s v2 =
    { x = s * v2.x, y = s * v2.y }


{-| The dot product of a and b
-}
dot : Vec2 -> Vec2 -> Float
dot a b =
    a.x * b.x + a.y * b.y


{-| Multiply the vector values by other vector values: a \* b
-}
mul : Vec2 -> Vec2 -> Vec2
mul a b =
    { x = a.x * b.x, y = a.y * b.y }
