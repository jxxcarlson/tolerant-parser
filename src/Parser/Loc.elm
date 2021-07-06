module Parser.Loc exposing (Position, dummy, positionOfList)


type alias Position =
    { start : Int, end : Int }


dummy =
    { start = -1, end = -1 }


positionOfList : List Position -> Position
positionOfList positions =
    let
        sorted =
            List.sortBy (\pos -> pos.start) positions

        first =
            List.head sorted |> Maybe.map .start |> Maybe.withDefault 0

        last =
            List.head (List.reverse sorted) |> Maybe.map .end |> Maybe.withDefault 0
    in
    { start = first, end = last }


data =
    [ { start = 0, end = 3 }, { start = 4, end = 5 }, { start = 6, end = 9 } ]


data2 =
    [ { start = 4, end = 5 }, { start = 0, end = 3 }, { start = 6, end = 9 } ]
