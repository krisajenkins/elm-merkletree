module Main exposing (..)

import ElmTest exposing (..)
import Test.Merkle as Merkle


tests : Test
tests =
    suite "Tests for Merkle Tree library"
        [{- Merkle.tests -}]


main : Program Never
main =
    runSuite tests
