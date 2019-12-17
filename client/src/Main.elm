module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, string)
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
    { dpy : Int
    , tasks : List Task
    , indicator : Int
    , errorMessage : Maybe String
    }


type alias Task =
    { isDone : Bool
    , isStarred : Bool
    , title : String
    , link : String
    , start : String -- "YYYY/MM/DD HH:MM'SS"
    , deadline : String
    , weight : Float
    , bar : Bar
    , isSelected : Bool
    }


type alias Bar =
    { dot : Int
    , sha : Int
    , exc : Int
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
    ( { dpy = day
      , tasks = []
      , indicator = 0
      , errorMessage = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DataReceived (Result Http.Error Model)
    | TasksReceived (Result Http.Error (List Task))
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


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- SendHttpRequest ->
        --     ( model, httpCommand )
        TasksReceived (Ok newTasks) ->
            ( { model
                | tasks = newTasks
              }
            , Cmd.none
            )

        TasksReceived (Err httpError) ->
            ( { model
                | dpy = 404
                , errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        DataReceived (Ok newModel) ->
            ( { model
                | dpy = newModel.dpy
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
            ( model, getTasksAll )

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



-- modelDecoder : Decoder Model
-- modelDecoder =
--     Decode.succeed Model
--         |> required "dpy" int
--         |> required "tasks" (list taskDecoder)
--         |> optional "indicator" int 0


taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "elmTaskIsDone" bool
        |> required "elmTaskIsStarred" bool
        |> required "elmTaskTitle" string
        |> required "elmTaskLink" string
        |> required "elmTaskStart" string
        |> required "elmTaskDeadline" string
        |> required "elmTaskWeight" float
        |> required "elmTaskBar" barDecoder
        |> optional "elmTaskIsSelected" bool False


barDecoder : Decoder Bar
barDecoder =
    Decode.succeed Bar
        |> required "elmBarDot" int
        |> required "elmBarSha" int
        |> required "elmBarExc" int



-- httpCommand : Cmd Msg
-- httpCommand =
--     Http.get
--         { url = "http://localhost:8080/model"
--         , expect = Http.expectJson DataReceived modelDecoder
--         }


getTasksAll : Cmd Msg
getTasksAll =
    Http.get
        { url = "http://localhost:8080/tasks/all"
        , expect = Http.expectJson TasksReceived (list taskDecoder)
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "height" "60px", style "background-color" "gray" ]
            [ span [] [ text ("dpy: " ++ String.fromInt model.dpy ++ em model) ] ]
        , div [ style "height" "30px", style "background-color" "aqua" ] []
        , table [ style "font-size" "12px", style "font-family" "Courier" ]
            ([ viewTableHeader ] ++ List.map (viewTask model) (List.indexedMap Tuple.pair model.tasks))
        ]


em : Model -> String
em model =
    case model.errorMessage of
        Just e ->
            e

        Nothing ->
            ""


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
            [ a [ href task.link ] [ text task.title ] ]
        , td []
            [ text (viewTimeByDpy task.start model.dpy) ]
        , td []
            [ text (barStr task.bar) ]
        , td []
            [ text (viewTimeByDpy task.deadline model.dpy) ]
        , td []
            [ text (String.fromFloat task.weight) ]
        , td []
            [ if task.isDone then
                text "DONE"

              else
                text "----"
            ]
        ]


viewTimeByDpy : String -> Int -> String
viewTimeByDpy time dpy =
    -- "YYYY/MM/DD HH:MM'SS"
    if dpy <= yea then
        fill 7 <| String.left 4 time

    else if dpy <= mon then
        fill 7 <| String.slice 2 7 time

    else if dpy <= day then
        fill 7 <| String.slice 5 10 time

    else if dpy <= hou then
        fill 7 <| String.slice 7 14 time

    else if dpy <= min then
        fill 7 <| String.slice 11 16 time

    else
        fill 7 <| String.right 5 time


fill : Int -> String -> String
fill n str =
    String.left n <| str ++ String.repeat n "."


barStr : Bar -> String
barStr bar =
    String.repeat bar.dot "."
        ++ String.repeat bar.sha "#"
        |> fill 50
        |> String.toList
        |> Array.fromList
        |> Array.set bar.exc '!'
        |> Array.toList
        |> String.fromList



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
