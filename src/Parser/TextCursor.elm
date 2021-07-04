module Parser.TextCursor exposing
    ( TextCursor, init
    , ErrorStatus(..), ParseError, empty, parseResult
    )

{-| TextCursor is the data structure used by Parser.parseLoop.

@docs TextCursor, init, incrementBlockIndex, incrementBlockOffset

-}

import Parser.AST as AST exposing (Element)


{-| SourceText structure used by Parser.Loop.run as it progressively "eats" bites of
the text, accumulating the parsed bites in the list `parsed: List Expression`.
The `offset` represents the position of beginning of the current `text` in the
original text. It is used to properly construct Expressions, which contain
as a component a SourceMap, which locates the bite of text in the original input
text.
-}
type alias TextCursor a =
    { count : Int
    , generation : Int
    , text : String
    , offset : Int
    , length : Int
    , parsed : List a
    , stack : List a
    }



-- summary : TextCursor Element -> { text : b, block : c, parsed : List (List Element.SimpleElement), stack : d }


type alias ParseError =
    { status : ErrorStatus, correctedText : List String }


type ErrorStatus
    = NoError
    | PipeError
    | RightBracketError
    | LeftBracketError
    | UnhandledError


parseResult : TextCursor a -> List a
parseResult t =
    t.parsed


empty : TextCursor a
empty =
    { count = 0
    , generation = 0
    , offset = 0
    , length = 0
    , text = ""
    , parsed = []
    , stack = []
    }


{-| Return a TextCursor with given chunkNumber and text
-}
init : Int -> String -> TextCursor a
init generation text =
    { count = 0
    , generation = generation
    , text = text
    , offset = 0
    , length = String.length text
    , parsed = []
    , stack = []
    }
