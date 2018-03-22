module Request.Sorting exposing (SortDirection(..), SortParameters, sortParams)


type alias SortParameters =
    { sortName : String
    , direction : SortDirection
    }


type SortDirection
    = Ascending
    | Descending


sortParams : SortParameters -> List ( String, String )
sortParams { sortName, direction } =
    [ ( "sortBy", sortName )
    , ( "sortOrder"
      , if direction == Ascending then
            "asc"
        else
            "desc"
      )
    ]
