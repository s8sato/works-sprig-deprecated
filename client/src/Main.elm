module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { scale : Int
    , tasks : List Task
    , indicator : Int
    }


type alias Task =
    { isDone : Bool
    , isStarred : Bool
    , title : String
    , start : DateTime
    , deadline : DateTime
    , weight : Int
    , bar : Bar
    , isSelected : Bool
    }


type alias DateTime =
    { date : String -- "YYYY/MM/DD"
    , time : String -- "HH:MM:SS"
    }


type alias Bar =
    { dot : Int
    , sharp : Int
    , exclamation : Int
    }


yea : Int
yea =
    1


qua : Int
qua =
    4


mon : Int
mon =
    12


wee : Int
wee =
    52


day : Int
day =
    365


hou : Int
hou =
    8760


min : Int
min =
    525600


sec : Int
sec =
    31536000


type Direction
    = Asc
    | Dsc


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scale = day
      , tasks = []
      , indicator = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error Model)
    | CharacterKey Char
    | ControlKey String
    | Register String
    | Done (List Task)
    | Focus Int
    | Edit (List Task)
    | Star Int
      -- | Sort Param Direction
      -- | Reschedule
      -- | Configure
      -- | Next
      -- | Prev
      -- | Search (List Condition)
    | Select Int


select : Model -> Int -> ( Model, Cmd Msg )
select model i =
    ( case Array.get i (Array.fromList model.tasks) of
        Nothing ->
            model

        Just task ->
            let
                newTask =
                    { task | isSelected = not task.isSelected }
            in
            { model
                | tasks =
                    Array.toList <|
                        Array.set i newTask (Array.fromList model.tasks)
            }
    , Cmd.none
    )


star : Model -> Int -> ( Model, Cmd Msg )
star model i =
    ( case Array.get i (Array.fromList model.tasks) of
        Nothing ->
            model

        Just task ->
            let
                newTask =
                    { task | isStarred = not task.isStarred }
            in
            { model
                | tasks =
                    Array.toList <|
                        Array.set i newTask (Array.fromList model.tasks)
            }
    , Cmd.none
    )


focus : Model -> Int -> ( Model, Cmd Msg )
focus model i =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok newModel) ->
            ( { model
                | scale = newModel.scale
                , tasks = newModel.tasks
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( model, Cmd.none )

        CharacterKey 'k' ->
            ( { model
                | indicator =
                    if model.indicator > 0 then
                        model.indicator - 1

                    else
                        model.indicator
              }
            , Cmd.none
            )

        CharacterKey 'j' ->
            ( { model
                | indicator =
                    if model.indicator < List.length model.tasks - 1 then
                        model.indicator + 1

                    else
                        model.indicator
              }
            , Cmd.none
            )

        CharacterKey 'e' ->
            ( { model
                | tasks =
                    List.map
                        (\task ->
                            if task.isSelected then
                                { task | isDone = not task.isDone }

                            else
                                task
                        )
                        model.tasks
              }
            , Cmd.none
            )

        -- APIに送る：taskId
        -- APIからもらう：Model
        CharacterKey 'f' ->
            focus model model.indicator

        -- APIに送る：[taskId]
        -- APIからもらう：String
        CharacterKey 'c' ->
            ( model, Cmd.none )

        CharacterKey 's' ->
            star model model.indicator

        CharacterKey 'x' ->
            select model model.indicator

        CharacterKey _ ->
            ( model, httpCommand )

        ControlKey _ ->
            ( model, Cmd.none )

        Register text ->
            ( model, Cmd.none )

        Done tasks ->
            ( model, Cmd.none )

        Focus index ->
            focus model index

        Edit tasks ->
            ( model, Cmd.none )

        Star index ->
            star model index

        Select index ->
            select model index


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed Model
        |> required "scale" int
        |> required "tasks" (list taskDecoder)
        |> optional "indicator" int 0


taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "isDone" bool
        |> required "isStarred" bool
        |> required "title" string
        |> required "start" dateTimeDecoder
        |> required "deadline" dateTimeDecoder
        |> required "weight" int
        |> required "bar" barDecoder
        |> optional "isSelected" bool False


dateTimeDecoder : Decoder DateTime
dateTimeDecoder =
    Decode.succeed DateTime
        |> required "date" string
        |> required "time" string


barDecoder : Decoder Bar
barDecoder =
    Decode.succeed Bar
        |> required "dot" int
        |> required "sharp" int
        |> required "exclamation" int


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://localhost:8080/model"
        , expect = Http.expectJson DataReceived modelDecoder
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "height" "60px", style "background-color" "gray" ]
            [ span [] [ text ("scale: " ++ String.fromInt model.scale) ] ]
        , div [ style "height" "30px", style "background-color" "aqua" ] []
        , table [ style "font-size" "12px", style "font-family" "Courier" ]
            ([ viewTableHeader ] ++ List.map (viewTask model) (List.indexedMap Tuple.pair model.tasks))
        ]


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "Sel" ]
        , th []
            [ text "Sta" ]
        , th []
            [ text "title" ]
        , th []
            [ text "start" ]
        , th []
            [ text "bar" ]
        , th []
            [ text "dead" ]
        , th []
            [ text "wei" ]
        , th []
            [ text "Done" ]
        ]


viewTask : Model -> ( Int, Task ) -> Html Msg
viewTask model ( idx, task ) =
    tr
        [ if idx == model.indicator then
            style "color" "blue"

          else
            style "color" "black"
        ]
        [ td [ onClick (Select idx) ]
            [ if task.isSelected then
                text "SEL"

              else
                text "---"
            ]
        , td [ onClick (Star idx) ]
            [ if task.isStarred then
                text "★"

              else
                text "☆"
            ]
        , td []
            [ text task.title ]
        , td []
            [ text (viewDateTimeByScale task.start model.scale) ]
        , td []
            [ text (barStr task.bar) ]
        , td []
            [ text (viewDateTimeByScale task.deadline model.scale) ]
        , td []
            [ text (String.fromInt task.weight) ]
        , td []
            [ if task.isDone then
                text "DONE"

              else
                text "----"
            ]
        ]


viewDateTimeByScale : DateTime -> Int -> String
viewDateTimeByScale datetime scale =
    if scale <= yea then
        fill 7 <| String.left 4 datetime.date

    else if scale <= mon then
        fill 7 <| String.slice 2 7 datetime.date

    else if scale <= day then
        fill 7 <| String.right 5 datetime.date

    else if scale <= hou then
        String.right 3 datetime.date ++ " " ++ String.right 3 datetime.time

    else if scale <= min then
        fill 7 <| String.left 5 datetime.time

    else
        fill 7 <| String.right 5 datetime.time


fill : Int -> String -> String
fill n str =
    String.right n <| String.repeat n "." ++ str


barStr : Bar -> String
barStr bar =
    String.repeat bar.dot "."
        ++ String.repeat (bar.sharp - bar.dot) "#"
        ++ String.repeat (bar.exclamation - bar.sharp) "."
        ++ "!"
        ++ String.repeat (50 - bar.exclamation) "."



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyPress keyDecoder

        -- , onClick (Decode.succeed MouseClick)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue
