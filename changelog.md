# 1.0.2

### Documentation

  * Removed unnecesary test from code file

# 1.0.1

### Documentation

  * Named correctly in README.md

# 1.0.0

### Initial release

  * Added the following type to `Merkle`:

	Tree :
	{ List (String -> String)
 	, a -> Json.Encode.Value
	, Json.Decode.Decoder a
	, BinaryTree a }

  * Added the following functions to `Merkle`:

	initialize :
	    Maybe (List (String -> String))
	    -> (a -> Value)
	    -> Decoder a
	    -> Tree a

        singleton :
	    a
	    -> Maybe (List (String -> String))
	    -> (a -> Value)
	    -> Decoder a
	    -> Tree a

        fromList :
	    Maybe (List (String -> String))
	    -> (a -> Value)
	    -> Decoder a
	    -> List a
	    -> Tree a

        insert : a -> Tree a -> Tree a

        insertFromList : Tree a -> List a -> Tree a

        contains : a -> Tree a -> Bool

        get : a -> Tree a -> List ( a, String )

        flatten : Tree a -> List ( a, String )

        depth : Tree a -> Int

        isValid : Maybe (List (String -> String)) -> Tree a -> Bool

        toJson : Int -> Int -> Tree a -> String

        fromJson :
	    Maybe (List (String -> String))
	    -> (a -> Value)
	    -> Decoder a
	    -> String
	    -> Result String (Tree a)
