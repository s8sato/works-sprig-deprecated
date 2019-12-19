module Styles exposing (..)

import Html.Attributes exposing (style)


draft =
    [ style "border" "solid thin" ]


header =
    [ style "height" "30px"
    , style "background-color" "gainsboro"
    ]
        ++ draft


sprig =
    [ style "width" "30px"
    , style "height" "30px"
    ]
        ++ draft
