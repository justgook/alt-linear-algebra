module Util exposing (compareFloat, epsilon, infinity)

import Expect exposing (Expectation, FloatingPointTolerance(..))


epsilon : Float
epsilon =
    2 ^ -52


infinity : Float
infinity =
    1 / 0


compareFloat : Float -> Float -> Expectation
compareFloat a b =
    if isNaN a && isNaN b then
        Expect.pass

    else
        Expect.within (Absolute epsilon) a b
