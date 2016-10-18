module Merkle
    exposing
        ( Tree
        , initialize
        , singleton
        , fromList
        , insert
        , insertFromList
        , contains
        , get
        , flatten
        , depth
        , isValid
        , toJson
        , fromJson
        )

{-| This is an implementation of the [Merkle
tree](https://en.wikipedia.org/wiki/Merkle_tree) data structure in Elm. It's
implemented as an immutable balanced binary hash tree, which guarantees for
logarithmic inserts. Default hash function is
[**SHA-2**56](https://en.wikipedia.org/wiki/SHA-2#Comparison_of_SHA_functions)
but others can be used. Hash functions are specified on initiation and can't be
changed afterwards which ensures data consistency.

# Trees

@docs Tree

# Creating Trees

@docs initialize, singleton, fromList

# Basics

@docs insert, insertFromList, get, depth, flatten, contains

# Check for Consistency

@docs isValid

# Persistence

@docs fromJson,toJson

-}

-- ELM-LANG LIBS
-- OTHER LIBS

import Basics exposing (max)
import Bitwise exposing (and, or, shiftLeft, shiftRight)
import Json.Decode as JsonD exposing ((:=), Decoder, andThen, customDecoder, decodeString, decodeValue, int, maybe, object1, object2, object3, oneOf, string, value)
import Json.Encode as JsonE exposing (Value, encode, int, null, object, string)
import List exposing (foldl)
import SHA exposing (sha256sum)
import String exposing (left)


{-| Representation of the immutable balanced binary hash tree. You can create
Merkle trees of integers (`Merkle.Tree Int`) or strings (`Merkle.Tree String`)
or any other type of values supported by Elm (Ints, Floats, Bools, Strings,
Maybes, Lists, Arrays, Tuples, Json.Values or concrete records).
-}
type Tree a
    = Tree (Config a)


type alias Config a =
    { hashfs : List (String -> String)
    , encode : a -> JsonE.Value
    , decode : JsonD.Decoder a
    , bintree : BinaryTree a
    }


type alias Hash =
    String


type BinaryTree a
    = Leaf (Maybe ( a, Hash ))
    | Branch ( Int, Hash ) (BinaryTree a) (BinaryTree a)


{-| Initialize an empty (`Merkle.Tree a`) that is bounded to the type assigned
in the hash functions (`hashfs`) and JSON encoder/decoder
(`encode`/`decode`). If no hash functions are assigned or an empty list is
given, then it will default to `sha256sum`.

    initialize Nothing Json.Encode.int Json.Decode.int
    initialize (Just ([ sha256sum ])) Json.Encode.int Json.Decode.int
    initialize (Just ([ identity ])) Json.Encode.int JsonD.Decode.int
-}
initialize :
    Maybe (List (String -> String))
    -> (a -> Value)
    -> Decoder a
    -> Tree a
initialize hashfs encode decode =
    Tree
        { hashfs = ensure hashfs
        , encode = encode
        , decode = decode
        , bintree = Leaf Nothing
        }


{-| Creates a (`Merkle.Tree a`) with the single element that is given which
is bounded to the type assigned in the hash functions (`hashfs`) and JSON
encoder/decoder (`encode`/`decode`). If no hash functions are assigned or an
empty list is given, then it will default to `sha256sum`.

    singleton 42 Nothing Json.Encode.int Json.Decode.int
    singleton 42 (Just ([ sha256sum ])) Json.Encode.int Json.Decode.int
    singleton 42 (Just ([ identity ])) Json.Encode.int JsonD.Decode.int
-}
singleton :
    a
    -> Maybe (List (String -> String))
    -> (a -> Value)
    -> Decoder a
    -> Tree a
singleton x hashfs encode decode =
    let
        bt =
            initbranch 1 x hashfs encode
    in
        init bt hashfs encode decode


{-| Creates a (`Merkle.Tree a`) from the elements in a list which are bounded to
the type assigned in the hash functions (`hashfs`) and JSON encoder/decoder
(`encode`/`decode`). If no hash functions are assigned or an empty list is
given, then it will default to `sha256sum`. The order of the elements is
significant, therefore the first element in the list will be the first to be
inserted in the `Merkle.Tree`. This is an important property in order to
maintain consistency and to be able to recreate a tree.

    [42] |> fromList Nothing Json.Encode.int Json.Decode.int
    [42] |> fromList (Just ([ sha256sum ])) Json.Encode.int Json.Decode.int
    [42] |> fromList (Just ([ identity ])) Json.Encode.int JsonD.Decode.int
-}
fromList :
    Maybe (List (String -> String))
    -> (a -> Value)
    -> Decoder a
    -> List a
    -> Tree a
fromList hashfs encode decode =
    let
        init =
            initialize hashfs encode decode
    in
        List.foldl (\x t -> t |> insert x) init


{-| Adds an element to the (`Merkle.Tree a`) in logarithmic time. It's important
to understand that this implementation of `Merkle.Tree` will ensure that all
elements have the same `depth` in the tree (in the same way as the
**Blockchain** for **BTC** does it). For more information, please see the
examples on GitHub.

    initialize Nothing Json.Encode.int Json.Decode.int
        |> insert 42
-}
insert : a -> Tree a -> Tree a
insert x (Tree ({ hashfs, encode, decode, bintree } as config)) =
    let
        m =
            bintree |> max

        hlp bt m i hashfs encode =
            case bt of
                Leaf _ ->
                    Debug.crash "Don't call hlp with a Leaf"

                Branch ( n, h ) lb rb ->
                    case n > 1 && n == m of
                        True ->
                            ( bt
                            , initbranch (log2 n) x (Just hashfs) encode
                            )

                        False ->
                            let
                                ln =
                                    case lb of
                                        Leaf _ ->
                                            1

                                        Branch ( n', _ ) _ _ ->
                                            n'
                            in
                                case ln < i of
                                    True ->
                                        ( lb |> rec (i // 2)
                                        , rb
                                        )

                                    False ->
                                        case n > 1 && (n == i) of
                                            True ->
                                                ( lb
                                                , initbranch (log2 n) x (Just hashfs) encode
                                                )

                                            False ->
                                                ( lb
                                                , rb |> rec (i // 2)
                                                )

        rec i bt =
            case bt of
                Leaf Nothing ->
                    let
                        h =
                            hashfs |> combine (stringify x encode)
                    in
                        ( x, h ) |> Just |> Leaf

                (Leaf (Just ( _, h ))) as leaf ->
                    let
                        hx =
                            hashfs |> combine (stringify x encode)

                        h' =
                            hashfs |> combine (h ++ hx)
                    in
                        Branch ( 2, h' ) leaf (( x, hx ) |> Just |> Leaf)

                (Branch ( n, h ) lb rb) as branch ->
                    let
                        ( lb', rb' ) =
                            hlp branch m i hashfs encode

                        h' =
                            hashHlp h lb' rb' hashfs
                    in
                        Branch ( (n + 1), h' ) lb' rb'
    in
        Tree
            { config
                | bintree = rec ((bintree |> max) // 2) bintree
            }


{-| Adds elements from the list to the (`Merkle.Tree a`). The order of the
elements is significant, therefore the first element in the list will be the
first to be inserted in the `Merkle.Tree`. This is an important property in
order to maintain consistency and to be able to recreate a tree.

    [0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)

-}
insertFromList : Tree a -> List a -> Tree a
insertFromList =
    List.foldl (\x a -> a |> insert x)


{-| Checks if the element is in the (`Merkle.Tree a`). As an element can appear
several times in a tree, therefore if we find early that the element is in the
tree we stop the recursion down this path.

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> contains 42) == True

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> contains 43) == False
-}
contains : a -> Tree a -> Bool
contains x (Tree ({ hashfs, encode, decode, bintree } as config)) =
    let
        rec bt =
            case bt of
                Leaf Nothing ->
                    False

                Leaf (Just ( data, hash )) ->
                    x == data

                Branch _ lb rb ->
                    (rec lb) || (rec rb)
    in
        rec bintree


{-| Returns the element(s) together with its/their `hash`(es) if any. Otherwise
it's returning an empty list. As an element can appear several times in a tree
and order of the elements is significant, therefore as the first element
inserted in the `Merkle.Tree` will be the first element of the list.

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> get 42) == [ (42, "Some hash") ]

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> get 43) == []

    ([42, 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> get 42) == [ (42, "Some hash"), (42, "Same hash")  ]
-}
get : a -> Tree a -> List ( a, String )
get x (Tree ({ hashfs, encode, decode, bintree } as config)) =
    let
        rec bt =
            case bt of
                Leaf Nothing ->
                    []

                Leaf (Just ( data, hash )) ->
                    case x == data of
                        True ->
                            [ ( data, hash ) ]

                        False ->
                            []

                Branch _ lb rb ->
                    (rec lb) ++ (rec rb)
    in
        rec bintree


{-| Returns all the elements together with their `hash`(es) if any. Otherwise it's
returning an empty list. As an element can appear several times in a tree and
order of the elements is significant, therefore as the first element inserted in
the `Merkle.Tree` will be the first element of the list.

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> flatten) == [ (0, "Some hash"), ..., (42, "Some hash") ]
-}
flatten : Tree a -> List ( a, String )
flatten (Tree ({ hashfs, encode, decode, bintree } as config)) =
    let
        rec bt =
            case bt of
                Leaf Nothing ->
                    []

                Leaf (Just ( data, hash )) ->
                    [ ( data, hash ) ]

                Branch _ lb rb ->
                    (rec lb) ++ (rec rb)
    in
        rec bintree


{-| Return the `depth` of the `Merkle.Tree`. We can calculate the max amount of
elements in the tree by power-of-two to the depth of the tree (`depth = 3` and
`2^3`) as we know that tree is logarithmic in `depth`.

    ([0 .. 7]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> depth) == 3

    ([0 .. 8]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> depth) == 4
-}
depth : Tree a -> Int
depth (Tree ({ hashfs, encode, decode, bintree } as config)) =
    let
        rec bt =
            case bt of
                Leaf _ ->
                    0

                Branch _ lb rb ->
                    1 + Basics.max (rec lb) (rec rb)
    in
        rec bintree


{-| To ensure data consistency and correctness, we can at any given point check
if the root `hash` is actually based on all the underlying elements. This
function is important to use whenever trees are imported `fromJson` as `hash`es
can be modified in the text format.

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> isValid Nothing) == True

    ([0 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> isValid (Just [ identity ])) == False

**Note**: If trees imported `fromJson` have both modified `data` and `hash`es
  consistently, it will not be possible for this function to ensure historic
  consistency but only correctness.
-}
isValid : Maybe (List (String -> String)) -> Tree a -> Bool
isValid hash (Tree ({ hashfs, encode, decode, bintree } as config)) =
    let
        hashfs' =
            case hash of
                Just _ ->
                    ensure hash

                Nothing ->
                    hashfs

        rec bt =
            case bt of
                Leaf Nothing ->
                    True

                Leaf (Just ( x, h )) ->
                    let
                        h' =
                            hashfs' |> combine (stringify x encode)
                    in
                        h == h'

                Branch ( n, h ) lb rb ->
                    let
                        h' =
                            hashHlp h lb rb hashfs'
                    in
                        (h == h') && (rec lb) && (rec rb)
    in
        rec bintree


{-| Converts a `Merkle Tree a` into a prettified `JSON` string. The first
argument specifies the amount of indentation in the resulting string while the
second specifies a hash mask to ensure better readability. For compact, use `0`
as indentation (no line break) and for readability use `4` (with line
breaks). For `Git` alike `hash` mask use `7` and 0 for full `hash`.

    ([40 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> toJson 0 7)

Becomes:

    {
        "count": 3,
        "hash": "37536cf",
        "left": {
            "count": 2,
            "hash": "ae41226",
            "left": {
                "hash": "d59eced",
                "data": 40
            },
            "right": {
                "hash": "3d914f9",
                "data": 41
            }
        },
        "right": {
            "count": 1,
            "hash": "c6e4fe6",
            "left": {
                "hash": "73475cb",
                "data": 42
            },
            "right": null
        }
    }

**Note**: If data is needed to persist, don't add any mask and ensure that full
  length is saved. It's important in order to check for data consistency
  (`isValid`).
-}
toJson : Int -> Int -> Tree a -> String
toJson indentation mask (Tree { hashfs, encode, decode, bintree }) =
    let
        shortHash h =
            case mask of
                0 ->
                    h

                _ ->
                    String.left mask h

        rec bt =
            case bt of
                Leaf Nothing ->
                    JsonE.null

                Leaf (Just ( v, h )) ->
                    JsonE.object
                        [ ( "hash", JsonE.string (shortHash h) )
                        , ( "data", encode v )
                        ]

                Branch ( n', h ) lb rb ->
                    JsonE.object
                        [ ( "count", JsonE.int n' )
                        , ( "hash", JsonE.string (shortHash h) )
                        , ( "left", rec lb )
                        , ( "right", rec rb )
                        ]
    in
        rec bintree |> JsonE.encode indentation


{-| Re-creates a `Merkle Tree a` from a prettified `JSON` string. It's important
to use the same initial parameters as the elements are bounded to the type
assigned in the hash functions (`hashfs`) and JSON encoder/decoder
(`encode`/`decode`). If any of these parameters defer from the initial, the tree
will neither be consistent nor correct (`isValid`).

    (case ([40 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> toJson 0 0
        |> fromJson Nothing JsonE.int JsonD.int) of
            Ok t ->
                t |> isValid Nothing
            Err msg ->
                Debug.crash msg) == True

    (case ([40 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> toJson 0 7
        |> fromJson Nothing JsonE.int JsonD.int) of
            Ok t ->
                t |> isValid Nothing
            Err msg ->
                Debug.crash msg) == False

    (case ([40 .. 42]
        |> insertFromList
            (initialize Nothing Json.Encode.int Json.Decode.int)
        |> toJson 0 0
        |> fromJson (Just [ identity ]) JsonE.int JsonD.int) of
            Ok t ->
                t |> isValid Nothing
            Err msg ->
                Debug.crash msg) == False
-}
fromJson :
    Maybe (List (String -> String))
    -> (a -> Value)
    -> Decoder a
    -> String
    -> Result String (Tree a)
fromJson hash encode decode json =
    let
        result =
            json
                |> JsonD.decodeString (decodeTree decode)
    in
        case result of
            Ok bt ->
                Ok
                    (Tree
                        { hashfs = ensure hash
                        , encode = encode
                        , decode = decode
                        , bintree = bt
                        }
                    )

            Err msg ->
                Err msg



-- HELPERS


initbranch :
    Int
    -> a
    -> Maybe (List (String -> String))
    -> (a -> Value)
    -> BinaryTree a
initbranch n x hash encode =
    let
        hl =
            (ensure hash) |> combine (stringify x encode)

        hb h =
            (ensure hash) |> combine (h ++ h)

        rec i h acc =
            case i of
                0 ->
                    acc

                _ ->
                    let
                        h' =
                            hb h

                        t =
                            Branch ( 1, h' ) acc (Nothing |> Leaf)
                    in
                        rec (i - 1) h' t
    in
        rec n hl (( x, hl ) |> Just |> Leaf)


init :
    BinaryTree a
    -> Maybe (List (String -> String))
    -> (a -> Value)
    -> Decoder a
    -> Tree a
init bt hash encode decode =
    Tree
        { hashfs = ensure hash
        , encode = encode
        , decode = decode
        , bintree = bt
        }


ensure : Maybe (List (String -> String)) -> List (String -> String)
ensure hash =
    case hash of
        Nothing ->
            [ sha256sum ]

        Just xs ->
            case xs of
                [] ->
                    [ sha256sum ]

                _ ->
                    xs


stringify : a -> (a -> Value) -> String
stringify x encode =
    x |> encode |> JsonE.encode 0


combine : a -> List (a -> String) -> String
combine x =
    List.foldl (\f a -> a ++ (f x)) ""


max : BinaryTree a -> Int
max t =
    case t of
        Leaf Nothing ->
            1

        Leaf (Just _) ->
            2

        Branch ( n, _ ) _ _ ->
            n |> ceilPow


hashHlp :
    a
    -> BinaryTree b
    -> BinaryTree c
    -> List (String -> String)
    -> String
hashHlp h lh rh fs =
    case ( lh, rh ) of
        ( Leaf (Just ( _, h1 )), Leaf (Just ( _, h2 )) ) ->
            fs |> combine (h1 ++ h2)

        ( Leaf (Just ( _, h1 )), Leaf Nothing ) ->
            fs |> combine (h1 ++ h1)

        ( Branch ( _, h1 ) _ _, Branch ( _, h2 ) _ _ ) ->
            fs |> combine (h1 ++ h2)

        ( Branch ( _, h1 ) _ _, Leaf (Just ( _, h2 )) ) ->
            fs |> combine (h1 ++ h2)

        ( Branch ( _, h1 ) _ _, Leaf Nothing ) ->
            fs |> combine (h1 ++ h1)

        ( Leaf _, _ ) ->
            Debug.crash "It shouldn't be possible to reach this branch"


log2 : Int -> Int
log2 =
    toFloat >> logBase 2.0 >> truncate


ceilPow : Int -> Int
ceilPow n =
    let
        rec ( i, acc ) =
            case ( i, acc ) of
                ( 0, a ) ->
                    1 .<< a

                ( m, a ) ->
                    let
                        m' =
                            m .& 1
                    in
                        ( (m - m') .>> 1, a + 1 ) |> rec
    in
        rec ( n - 1, 0 )


(.&) : Int -> Int -> Int
(.&) =
    Bitwise.and


(.|) : Int -> Int -> Int
(.|) =
    Bitwise.or


(.<<) : Int -> Int -> Int
(.<<) =
    Bitwise.shiftLeft


(.>>) : Int -> Int -> Int
(.>>) =
    Bitwise.shiftRight


decodeTree : Decoder a -> Decoder (BinaryTree a)
decodeTree decode =
    let
        decodeLeaf =
            JsonD.object1 Leaf
                (JsonD.maybe
                    (JsonD.object2 (,)
                        ("data" := decode)
                        ("hash" := JsonD.string)
                    )
                )

        decodeBranch =
            JsonD.object3 Branch
                (JsonD.object2 (,)
                    ("count" := JsonD.int)
                    ("hash" := JsonD.string)
                )
                ("left" := (decodeLazy (\_ -> decodeTree decode)))
                ("right" := (decodeLazy (\_ -> decodeTree decode)))
    in
        JsonD.oneOf [ decodeBranch, decodeLeaf ]


decodeLazy :
    (() -> JsonD.Decoder a)
    -> JsonD.Decoder a
decodeLazy thunk =
    JsonD.customDecoder JsonD.value
        (\json -> JsonD.decodeValue (thunk ()) json)
