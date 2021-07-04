module Parser.Loop exposing (Packet, parseLoop)

import Parser.AST as AST exposing (Element)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Error exposing (Context, Problem)
import Parser.TextCursor as TextCursor exposing (TextCursor)
import Parser.Tool


type alias Parser a =
    Parser.Parser Context Problem a


type alias Packet a =
    { parser : Int -> Parser a
    , getLength : a -> Int
    , handleError : Maybe (List (Parser.DeadEnd Context Problem) -> TextCursor a -> TextCursor a)
    }


{-| parseLoop takes as input an integer representing a "chunkOffset" and
a string of source text, the "chunk." It returns a TextCursor, which
is a data structure which includes the parsed source text.
-}
parseLoop : Packet Element -> Int -> String -> TextCursor Element
parseLoop packet generation str =
    Parser.Tool.loop (TextCursor.init generation str) (nextCursor packet)


{-| nextCursor operates by running the expression parser on
`tc.text` with argument `tc.chunkNumber`. This argument is used
to track the location in the source text of the piece of text
parsed.

Recall that parseLoop is fed chunks of text by
Document.process. These chunks are "logical paragraphs,"
which one may think of as forming an array of strings indexed
by chunkNumber. A piece of text within a chunk is identified
by an offset and a length:

    piece =
        String.slice offset (offset + length) chunk

If parsing succeeds, resulting in a parsand `expr`, the textCursor
operated by parseLoop is updated:

    - the "program counter" tc.count is incremented
    - the piece of text corresponding to the parsand
      is removed from tc.text
    - `expr` is prepended to `tc.parsed`

-}
nextCursor : Packet Element -> TextCursor Element -> Parser.Tool.Step (TextCursor Element) (TextCursor Element)
nextCursor packet tc =
    let
        _ =
            Debug.log "(N, p, text)" ( tc.count, tc.offset, String.dropLeft tc.offset tc.text )
    in
    if tc.offset >= tc.length || tc.count > 10 then
        -- TODO: that usage of count needs to be removed after bug is fixed
        Parser.Tool.Done { tc | parsed = List.reverse tc.parsed }

    else
        let
            input =
                String.dropLeft tc.offset tc.text
        in
        case Parser.run (packet.parser tc.generation) input of
            Ok expr ->
                Parser.Tool.Loop
                    { tc
                        | count = tc.count + 1
                        , offset = tc.offset + packet.getLength expr
                        , parsed = expr :: tc.parsed
                    }

            Err e ->
                let
                    col =
                        List.head e |> Maybe.map .col |> Maybe.withDefault 0

                    errorElement =
                        AST.Problem e (String.left col input)

                    _ =
                        Debug.log "Error col" col
                in
                case packet.handleError of
                    Nothing ->
                        Parser.Tool.Loop { tc | count = tc.count + 1, offset = tc.offset + col, parsed = errorElement :: tc.parsed }

                    Just he ->
                        Parser.Tool.Loop (he e tc)
