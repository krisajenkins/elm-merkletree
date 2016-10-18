module Test.Merkle exposing (tests)

import ElmTest exposing (..)
import Merkle exposing (..)


tests : Test
tests =
    suite "Merkle Trees (coming soon)"
        (data
            |> List.map
                (\( msg, md ) -> test msg <| assertEqual md <| identity msg)
        )


data : List ( String, String )
data =
    []
