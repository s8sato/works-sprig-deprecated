module Main exposing (main)

import Array exposing (fromList, get, set, toList)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, a, div, text, textarea)
import Html.Attributes exposing (class, href, id, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode


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
    , title : Maybe String
    , link : Maybe String
    , start : Maybe String -- "YYYY/MM/DD HH:MM'SS"
    , deadline : Maybe String
    , weight : Maybe Float
    , secUntilStart : Maybe Int
    , secUntilDeadline : Maybe Int
    , isSelected : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { user = 1
      , tasks = []
      , inputText = Nothing
      , barLeftEdgeTime = Nothing
      , dpy = dayPerY
      , indicator = 0
      , errorMessage = Nothing
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
        --
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

        CharacterKey '1' ->
            ( { model | dpy = yeaPerY }, Cmd.none )

        CharacterKey '2' ->
            ( { model | dpy = quaPerY }, Cmd.none )

        CharacterKey '3' ->
            ( { model | dpy = monPerY }, Cmd.none )

        CharacterKey '4' ->
            ( { model | dpy = weePerY }, Cmd.none )

        CharacterKey '5' ->
            ( { model | dpy = dayPerY }, Cmd.none )

        CharacterKey '6' ->
            ( { model | dpy = houPerY }, Cmd.none )

        CharacterKey '7' ->
            ( { model | dpy = minPerY }, Cmd.none )

        CharacterKey '8' ->
            ( { model | dpy = secPerY }, Cmd.none )

        CharacterKey 'a' ->
            ( model, Cmd.none )

        CharacterKey 't' ->
            ( model, Cmd.none )

        CharacterKey 'h' ->
            ( model, Cmd.none )

        CharacterKey 'b' ->
            ( model, Cmd.none )

        CharacterKey 'p' ->
            ( model, Cmd.none )

        CharacterKey 'r' ->
            ( model, Cmd.none )

        CharacterKey 'i' ->
            ( { model
                | tasks =
                    List.map
                        (\task ->
                            { task | isSelected = not task.isSelected }
                        )
                        model.tasks
              }
            , Cmd.none
            )

        CharacterKey 'l' ->
            ( model, Cmd.none )

        CharacterKey '#' ->
            -- Delete selected tasks
            ( model, Cmd.none )

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
        |> optional "elmModelInputText" (nullable string) Nothing
        |> optional "elmModelBarLeftEdgeTime" (nullable string) Nothing
        |> optional "elmModelDpy" int dayPerY
        |> optional "elmModelIndicator" int 0
        |> optional "elmModelErrorMessage" (nullable string) Nothing


taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "elmTaskId" int
        |> required "elmTaskIsDone" bool
        |> required "elmTaskIsStarred" bool
        |> optional "elmTaskTitle" (nullable string) Nothing
        |> optional "elmTaskLink" (nullable string) Nothing
        |> optional "elmTaskStart" (nullable string) Nothing
        |> optional "elmTaskDeadline" (nullable string) Nothing
        |> optional "elmTaskWeight" (nullable float) Nothing
        |> optional "elmTaskSecUntilStart" (nullable int) Nothing
        |> optional "elmTaskSecUntilDeadline" (nullable int) Nothing
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
            , div [ id "inputBox" ]
                [ textarea
                    [ id "inputArea"
                    , placeholder textPlaceholder
                    , onInput Input
                    ]
                    []
                ]
            , div [ id "submission" ]
                [ div [ id "submitButton", onClick TextPost ] [] ]
            , div [ id "account" ] []
            ]
        , div [ id "body" ]
            [ div [ id "lSideBar" ]
                [ div [ id "staticIconsBox" ]
                    [ div [ id "upAndDown" ] []
                    , div [ id "setSelected" ] []
                    , div [ id "setStarred" ] []
                    , div [ id "setDpy" ] []
                    ]
                ]
            , div [ id "mainContainer" ]
                [ div [ id "mainHeader" ]
                    [ div [ id "selectionCmdBox" ]
                        [ div [ id "inverseSelect", onClick (CharacterKey 'i') ] []
                        , div [ id "eliminate", onClick (CharacterKey 'e') ] []
                        , div [ id "create" ] []
                        , div [ id "reschedule" ] []
                        , div [ id "linksOpen" ] []
                        , div [ id "criticalPath" ] []
                        ]
                    , div [ class "middleCmdBox" ]
                        [ text (em model) ]
                    , div [ id "viewCmdBox" ]
                        [ div [ id "focus", onClick (CharacterKey 'f') ] []
                        , div [ id "archives" ] []
                        , div [ id "trunk" ] []
                        , div [ id "buds" ] []
                        , div [ id "home" ] []
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
        , div [ class "weight" ] []
        , div [ class "done" ] []
        ]


viewTask : Model -> ( Int, Task ) -> Html Msg
viewTask model ( idx, task ) =
    div
        [ class "task"
        , style "background-color"
            (if idx == model.indicator then
                "#C2DBFF"

             else if task.isSelected then
                "#FFF1BC"

             else
                "#EEF4F2"
            )
        ]
        [ div
            [ class "selection"
            , onClick (SwitchSelect idx)
            ]
            [ if task.isSelected then
                text "SEL"

              else
                text "-"
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
            [ case ( task.title, task.link ) of
                ( Just t, Just l ) ->
                    a [ href l ] [ text t ]

                ( Just t, Nothing ) ->
                    text t

                ( Nothing, Just l ) ->
                    a [ href l ] [ text "LINK ONLY" ]

                ( Nothing, Nothing ) ->
                    div [] []
            ]
        , div [ class "start" ]
            [ text (viewTimeByDpy task.start model.dpy) ]
        , div [ class "bar" ]
            [ text (barString model.dpy task.weight task.secUntilStart task.secUntilDeadline) ]
        , div [ class "deadline" ]
            [ text (viewTimeByDpy task.deadline model.dpy) ]
        , div [ class "weight" ]
            [ text
                (case task.weight of
                    Nothing ->
                        "-"

                    Just w ->
                        String.fromFloat w
                )
            ]
        , div [ class "done" ]
            [ if task.isDone then
                text "DONE"

              else
                text "-"
            ]
        ]


viewTimeByDpy : Maybe String -> Int -> String
viewTimeByDpy time dpy =
    case time of
        Nothing ->
            "-"

        Just t ->
            -- "YYYY/MM/DD HH:MM'SS"
            if dpy <= yeaPerY then
                fill 7 " " <| String.left 4 t

            else if dpy <= monPerY then
                fill 7 " " <| String.slice 2 7 t

            else if dpy <= dayPerY then
                fill 7 " " <| String.slice 5 10 t

            else if dpy <= houPerY then
                fill 7 " " <| String.slice 7 14 t

            else if dpy <= minPerY then
                fill 7 " " <| String.slice 11 16 t

            else
                fill 7 " " <| String.right 5 t


fill : Int -> String -> String -> String
fill n putty target =
    String.left n <| target ++ String.repeat n putty


barString : Int -> Maybe Float -> Maybe Int -> Maybe Int -> String
barString dpy weight secUS secUD =
    let
        dot =
            case secUS of
                Nothing ->
                    0

                Just s ->
                    dotsFromSec dpy s

        sha =
            case weight of
                Nothing ->
                    0

                Just w ->
                    case secFromWeight w of
                        0 ->
                            0

                        s ->
                            dotsFromSec dpy s + 1

        exc =
            case secUD of
                Nothing ->
                    -1

                Just s ->
                    dotsFromSec dpy s
    in
    String.repeat dot "."
        ++ String.repeat sha "#"
        |> fill 48 "."
        |> String.toList
        |> Array.fromList
        |> Array.set exc '!'
        |> Array.toList
        |> String.fromList


dotsFromSec : Int -> Int -> Int
dotsFromSec dpy sec =
    floor <| toFloat (dpy * sec) / toFloat (60 * 60 * 24 * 365)


secFromWeight : Float -> Int
secFromWeight w =
    floor (60 * 60 * w)


textPlaceholder : String
textPlaceholder =
    "ENTER TASKS OR A COMMAND:\n\njump\n    step\n        hop\n\nA task to complete by the end of 2020 -2020/12/31\n    A task expected to take 300 hours $300\n        A task you can start in the beginning of 2020 2020/1/1-\n\nA time-critical task 2020/01/1- 23:59:59- $0.001 -0:0:3 -2020/1/02\n\ntrunk\n    branch Alice\n        bud \n    branch Bob\n        bud\n        bud\n\njump\n    step\n        hop2 dependent on hop1 [key\n    step\n        key] hop1\n\n# A task to register as completed\n* A task to register as starred\n\nA linked task e.g. slack permalink &https://\n\n@777 The task with ID 777 whose weight will be updated to 30 $30\n\n@777 The complex task\n    A simpler task\n    A simpler task\n\nA new emerging task dependent on existing @777 and @888\n    @777\n    @888\n\nYOU CAN ALSO ENTER ONE OF THE FOLLOWING SLASH COMMANDS:\n\n/dpy 1\nSet dpy (dots per year) 1, that is, a dot represents a year.\n\n/asof 2020/01/01_12:0:0\nSet the time corresponding to the left edge of the bar to noon on January 1, 2020.\n\n/sel -t word\nSelect tasks whose title contains 'word'.\n\n/sel -s 2020/1/1_12:0:0< <2020/1/2\nSelect tasks whose start is in the afternoon of January 1, 2020.\n\n/sel -d <23:59:59\nSelect tasks whose deadline is today's end.\n\n/sel -w 30< <300\nSelect tasks whose weight is between 30 and 300 hours.\n\n/sel -arc\nSelect archived tasks.\n\n/sel -star\nSelect starred tasks.\n\n/sel -trunk\nSelect trunk, namely, tasks with no successor.\n\n/sel -buds\nSelect buds, namely, tasks with no predecessor.\n\n/sel -t word -s 2020/1/1< -d <23:59:59 -w 30< <300 -arc -star\nSpecify multiple conditions.\n\nTHANK YOU FOR READING!\n"



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
