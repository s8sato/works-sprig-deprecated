module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (class, cols, href, id, placeholder, rows, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Styles as S


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
    { user : Int
    , tasks : List Task
    , inputText : Maybe String
    , barLeftEdgeTime : Maybe String
    , dpy : Int
    , indicator : Int
    , errorMessage : Maybe String
    }


type alias Task =
    { id : Int
    , isDone : Bool
    , isStarred : Bool
    , title : String
    , link : String
    , start : String -- "YYYY/MM/DD HH:MM'SS"
    , deadline : String
    , weight : Float
    , secUntilStart : Int
    , secUntilDeadline : Int
    , isSelected : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dpy = dayPerY
      , tasks = []
      , indicator = 0
      , errorMessage = Nothing
      , inputText = Nothing
      , user = 1
      , barLeftEdgeTime = Nothing
      }
    , Cmd.none
    )


yeaPerY : Int
yeaPerY =
    1


quaPerY : Int
quaPerY =
    4


monPerY : Int
monPerY =
    12


weePerY : Int
weePerY =
    52


dayPerY : Int
dayPerY =
    365


houPerY : Int
houPerY =
    8760


minPerY : Int
minPerY =
    525600


secPerY : Int
secPerY =
    31536000


type Direction
    = Asc
    | Dsc



-- UPDATE


type Msg
    = DataReceived (Result Http.Error Model)
    | TasksReceived (Result Http.Error Model)
    | CharacterKey Char
    | ControlKey String
    | SwitchSelect Int
    | Input String
    | TextPost
    | DoneTasks
    | StarSwitched Int (Result Http.Error ())
    | FocusTask Int
    | Edit (List Task)
    | SwitchStar Int



-- | Sort Param Direction
-- | Reschedule
-- | Configure
-- | Next
-- | Prev
-- | Search (List Condition)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- SendHttpRequest ->
        --     ( model, httpCommand )
        TasksReceived (Ok newModel) ->
            ( { model
                | tasks = newModel.tasks
                , indicator = newModel.indicator
                , barLeftEdgeTime = newModel.barLeftEdgeTime
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
            ( model, doneTasks model )

        -- ( { model
        --     | tasks =
        --         List.map
        --             (\task ->
        --                 if task.isSelected then
        --                     { task | isDone = not task.isDone }
        --                 else
        --                     task
        --             )
        --             model.tasks
        --   }
        -- , Cmd.none
        -- )
        -- APIに送る：taskId
        -- APIからもらう：[Task]
        CharacterKey 'f' ->
            ( model, focusTask model model.indicator )

        -- APIに送る：[taskId]
        -- APIからもらう：String
        CharacterKey 'c' ->
            ( model, Cmd.none )

        CharacterKey 's' ->
            ( model, switchStar model model.indicator )

        CharacterKey 'x' ->
            switchSelect model model.indicator

        CharacterKey 'y' ->
            ( { model | dpy = houPerY }, getTasksAll )

        CharacterKey '#' ->
            -- Delete selected tasks
            ( model, getTasksAll )

        CharacterKey _ ->
            ( model, getTasksAll )

        ControlKey _ ->
            ( model, Cmd.none )

        TextPost ->
            ( model, textPost model )

        DoneTasks ->
            ( model, doneTasks model )

        StarSwitched i (Ok _) ->
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

        StarSwitched _ (Err httpError) ->
            ( { model
                | dpy = 404
                , errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        FocusTask index ->
            ( model, focusTask model index )

        Edit tasks ->
            ( model, Cmd.none )

        SwitchStar index ->
            ( model, switchStar model index )

        SwitchSelect index ->
            switchSelect model index

        Input newInput ->
            ( { model | inputText = Just newInput }, Cmd.none )


textPost : Model -> Cmd Msg
textPost m =
    Http.post
        { url = "http://localhost:8080/tasks"
        , body = Http.jsonBody (textPostEncoder m)
        , expect = Http.expectJson TasksReceived modelDecoder
        }


doneTasks : Model -> Cmd Msg
doneTasks m =
    Http.post
        { url = "http://localhost:8080/tasks/done"
        , body = Http.jsonBody (doneTasksEncoder m)
        , expect = Http.expectJson TasksReceived modelDecoder
        }


switchStar : Model -> Int -> Cmd Msg
switchStar m i =
    Http.post
        { url = "http://localhost:8080/tasks/star"
        , body = Http.jsonBody (switchStarEncoder m i)
        , expect = Http.expectWhatever (StarSwitched i)
        }


switchSelect : Model -> Int -> ( Model, Cmd Msg )
switchSelect model i =
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


focusTask : Model -> Int -> Cmd Msg
focusTask m i =
    Http.post
        { url = "http://localhost:8080/tasks/focus"
        , body = Http.jsonBody (focusTaskEncoder m i)
        , expect = Http.expectJson TasksReceived modelDecoder
        }


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


textPostEncoder : Model -> Encode.Value
textPostEncoder m =
    let
        content =
            case m.inputText of
                Nothing ->
                    ""

                Just c ->
                    c
    in
    Encode.object
        [ ( "textPostUser", Encode.int m.user )
        , ( "textPostContent", Encode.string content )
        ]


doneTasksEncoder : Model -> Encode.Value
doneTasksEncoder m =
    let
        ids =
            List.map .id <| List.filter .isSelected <| m.tasks
    in
    Encode.object
        [ ( "doneTasksUser", Encode.int m.user )
        , ( "doneTasksIds", Encode.list Encode.int ids )
        ]


switchStarEncoder : Model -> Int -> Encode.Value
switchStarEncoder m i =
    let
        taskId =
            case Array.get i (Array.fromList m.tasks) of
                Nothing ->
                    0

                Just task ->
                    .id task
    in
    Encode.object
        [ ( "switchStarUser", Encode.int m.user )
        , ( "switchStarId", Encode.int taskId )
        ]


focusTaskEncoder : Model -> Int -> Encode.Value
focusTaskEncoder m i =
    let
        taskId =
            case Array.get i (Array.fromList m.tasks) of
                Nothing ->
                    0

                Just task ->
                    .id task
    in
    Encode.object
        [ ( "focusTaskUser", Encode.int m.user )
        , ( "focusTaskId", Encode.int taskId )
        ]


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed Model
        |> required "elmModelUser" int
        |> required "elmModelTasks" (list taskDecoder)
        |> required "elmModelInputText" (nullable string)
        |> required "elmModelBarLeftEdgeTime" (nullable string)
        |> optional "elmModelDpy" int dayPerY
        |> optional "elmModelIndicator" int 0
        |> optional "elmModelErrorMessage" (nullable string) Nothing


taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "elmTaskId" int
        |> required "elmTaskIsDone" bool
        |> required "elmTaskIsStarred" bool
        |> required "elmTaskTitle" string
        |> required "elmTaskLink" string
        |> required "elmTaskStart" string
        |> required "elmTaskDeadline" string
        |> required "elmTaskWeight" float
        |> required "elmTaskSecUntilStart" int
        |> required "elmTaskSecUntilDeadline" int
        |> optional "elmTaskIsSelected" bool False


getTasksAll : Cmd Msg
getTasksAll =
    Http.get
        { url = "http://localhost:8080/tasks/all"
        , expect = Http.expectJson TasksReceived modelDecoder
        }


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "header" ]
            [ div [ id "logo" ] []
            , div [ class "inputField" ]
                [ textarea [ cols 80, rows 2, placeholder "...", onInput Input ] [] ]
            , div [ id "submission" ]
                [ button [ type_ "button", onClick TextPost ] [ text "Submit" ] ]
            , div [ id "account" ] []
            ]
        , div [ id "body" ]
            [ div [ id "lSideBar" ] []
            , div [ id "mainContainer" ]
                [ div [ id "mainHeader" ]
                    [ div [ id "selectionCmd" ]
                        [ div [ id "inverseSelect" ] []
                        , div [ id "eliminate" ] []
                        , div [ id "create" ] []
                        , div [ id "reschedule" ] []
                        , div [ id "linksOpen" ] []
                        ]
                    , div [ class "middleCmd" ]
                        [ text (em model) ]
                    , div [ id "viewCmd" ]
                        [ div [ id "focus" ] []
                        , div [ id "archives" ] []
                        , div [ id "trunk" ] []
                        , div [ id "buds" ] []
                        , div [ id "home" ] []
                        , div [ id "criticalPath" ] []
                        ]
                    ]
                , div [ id "mainBody" ]
                    [ viewTaskHeader model
                    , div [ id "tasks" ]
                        (List.map
                            (viewTask model)
                            (List.indexedMap Tuple.pair model.tasks)
                        )
                    ]
                ]
            , div [ id "rSideBar" ] []
            ]
        , div [ id "fotter" ] []
        ]


em : Model -> String
em model =
    case model.errorMessage of
        Just e ->
            e

        Nothing ->
            ""


viewTaskHeader : Model -> Html Msg
viewTaskHeader m =
    div [ id "taskHeader" ]
        [ div [ class "selection" ] []
        , div [ class "star" ] []
        , div [ class "title" ] []
        , div [ class "start" ] []
        , div [ class "bar" ]
            [ text
                ("As of "
                    ++ (case m.barLeftEdgeTime of
                            Nothing ->
                                "UNKNOWN TIME"

                            Just t ->
                                t
                       )
                )
            , text (", " ++ String.fromInt m.dpy ++ " dpy")
            ]
        , div [ class "deadline" ] []
        , div [ class "weight" ]
            [ text "W" ]
        , div [ class "done" ] []
        ]


viewTask : Model -> ( Int, Task ) -> Html Msg
viewTask model ( idx, task ) =
    div
        [ class "task"
        , if idx == model.indicator then
            style "background-color" "#C2DBFF"

          else
            style "background-color" "#EEF4F2"
        ]
        [ div
            [ class "selection"
            , onClick (SwitchSelect idx)
            ]
            [ if task.isSelected then
                text "SEL"

              else
                text "---"
            ]
        , div
            [ class "star"
            , onClick (SwitchStar idx)
            ]
            [ if task.isStarred then
                text "★"

              else
                text "☆"
            ]
        , div [ class "title" ]
            [ a [ href task.link ] [ text task.title ] ]
        , div [ class "start" ]
            [ text (viewTimeByDpy task.start model.dpy) ]
        , div [ class "bar" ]
            [ text (barString model.dpy task.weight task.secUntilStart task.secUntilDeadline) ]
        , div [ class "deadline" ]
            [ text (viewTimeByDpy task.deadline model.dpy) ]
        , div [ class "weight" ]
            [ text (String.fromFloat task.weight) ]
        , div [ class "done" ]
            [ if task.isDone then
                text "DONE"

              else
                text "----"
            ]
        ]


viewTimeByDpy : String -> Int -> String
viewTimeByDpy time dpy =
    -- "YYYY/MM/DD HH:MM'SS"
    if dpy <= yeaPerY then
        fill 7 " " <| String.left 4 time

    else if dpy <= monPerY then
        fill 7 " " <| String.slice 2 7 time

    else if dpy <= dayPerY then
        fill 7 " " <| String.slice 5 10 time

    else if dpy <= houPerY then
        fill 7 " " <| String.slice 7 14 time

    else if dpy <= minPerY then
        fill 7 " " <| String.slice 11 16 time

    else
        fill 7 " " <| String.right 5 time


fill : Int -> String -> String -> String
fill n putty target =
    String.left n <| target ++ String.repeat n putty


barString : Int -> Float -> Int -> Int -> String
barString dpy weight secUS secUD =
    let
        dot =
            sec2dot dpy secUS

        sha =
            case weight2sec weight of
                0 ->
                    0

                s ->
                    sec2dot dpy s + 1

        exc =
            case secUD of
                0 ->
                    -1

                _ ->
                    sec2dot dpy secUD
    in
    String.repeat dot "."
        ++ String.repeat sha "#"
        |> fill 50 "."
        |> String.toList
        |> Array.fromList
        |> Array.set exc '!'
        |> Array.toList
        |> String.fromList


sec2dot : Int -> Int -> Int
sec2dot dpy sec =
    floor <| toFloat (dpy * sec) / toFloat (60 * 60 * 24 * 365)


weight2sec : Float -> Int
weight2sec w =
    floor (60 * 60 * w)



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
