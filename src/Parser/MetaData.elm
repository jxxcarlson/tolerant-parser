module Parser.MetaData exposing (..)

import Parser.Loc as Loc


type alias MetaData =
    { position : Loc.Position, generation : Int }


meta generation start end =
    { position = { start = start, end = end }, generation = generation }


dummy =
    { position = { start = 0, end = 0 }, generation = 0 }



--init generation offset =
--    { offset = offset
--    , generation = generation
--    , location = { start = { line = 0, column = 0 }, finish = { line = 0, column = 0 } }
--    }
