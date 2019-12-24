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
import Maybe
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, millisToPosix, posixToMillis)


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
    { sub : SubModel
    , zone : Zone
    , asOfTime : Posix
    , indicator : Int
    }


type alias SubModel =
    { user : User
    , tasks : List Task
    , inputText : Maybe String
    , dpy : Maybe Int
    , message : Maybe String
    }


type alias User =
    { id : Int
    , name : String
    , admin : Bool
    }


type alias Task =
    { id : Int
    , isDone : Bool
    , isStarred : Bool
    , title : Maybe String
    , link : Maybe String
    , start : Maybe Int -- POSIX seconds
    , deadline : Maybe Int
    , weight : Maybe Float
    , user : String
    , isSelected : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSub =
            { user = User 1 "no_one" False
            , tasks = []
            , inputText = Nothing
            , dpy = Nothing
            , message = Nothing
            }
    in
    ( { sub = initSub
      , zone = Time.utc
      , asOfTime = Time.millisToPosix 0
      , indicator = 0
      }
    , Task.perform AdjustTimeZone Time.here
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



-- UPDATE


type Msg
    = SubModelReceived (Result Http.Error SubModel)
    | TasksReceived (Result Http.Error (List Task))
    | CharacterKey Char
    | ControlKey String
    | SwitchSelect Int
    | Input String
    | TextPost
    | StarSwitched Int (Result Http.Error ())
    | SwitchStar Int
    | AdjustTimeZone Time.Zone



-- | Sort Param Direction
-- | Reschedule
-- | Configure
-- | Next
-- | Prev
-- | Search (List Condition)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubModelReceived (Ok newSubModel) ->
            ( { model
                | sub = newSubModel
              }
            , Cmd.none
            )

        SubModelReceived (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        TasksReceived (Ok newTasks) ->
            ( let
                sub =
                    model.sub

                newSub =
                    { sub | tasks = newTasks }
              in
              { model
                | sub = newSub
              }
            , Cmd.none
            )

        TasksReceived (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

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
                    if model.indicator < List.length model.sub.tasks - 1 then
                        model.indicator + 1

                    else
                        model.indicator
              }
            , Cmd.none
            )

        CharacterKey 'e' ->
            ( model, doneTasks model.sub )

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
            ( model, focusTask model.sub model.indicator )

        -- APIに送る：[taskId]
        -- APIからもらう：String
        CharacterKey 'c' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 's' ->
            ( model, switchStar model.sub model.indicator )

        CharacterKey 'x' ->
            ( switchSelect model model.indicator, Cmd.none )

        CharacterKey '1' ->
            ( changeDpy model yeaPerY, Cmd.none )

        CharacterKey '2' ->
            ( changeDpy model quaPerY, Cmd.none )

        CharacterKey '3' ->
            ( changeDpy model monPerY, Cmd.none )

        CharacterKey '4' ->
            ( changeDpy model weePerY, Cmd.none )

        CharacterKey '5' ->
            ( changeDpy model dayPerY, Cmd.none )

        CharacterKey '6' ->
            ( changeDpy model houPerY, Cmd.none )

        CharacterKey '7' ->
            ( changeDpy model minPerY, Cmd.none )

        CharacterKey '8' ->
            ( changeDpy model secPerY, Cmd.none )

        CharacterKey 'a' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 't' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 'h' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 'b' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 'p' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 'r' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 'i' ->
            ( inverseSelect model, Cmd.none )

        CharacterKey 'l' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey '#' ->
            -- Delete selected tasks?  -- TODO
            ( model, Cmd.none )

        CharacterKey _ ->
            -- TODO
            ( model, initialize model.sub )

        ControlKey _ ->
            -- TODO
            ( model, Cmd.none )

        TextPost ->
            ( model, textPost model.sub )

        StarSwitched i (Ok _) ->
            ( starSwitched model i, Cmd.none )

        StarSwitched _ (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        SwitchStar index ->
            ( model, switchStar model.sub index )

        SwitchSelect index ->
            ( switchSelect model index, Cmd.none )

        Input newInput ->
            ( let
                sub =
                    model.sub

                newSub =
                    { sub | inputText = Just newInput }
              in
              { model | sub = newSub }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )


starSwitched : Model -> Int -> Model
starSwitched model i =
    case Array.get i (Array.fromList model.sub.tasks) of
        Nothing ->
            model

        Just task ->
            let
                newTask =
                    { task | isStarred = not task.isStarred }

                sub =
                    model.sub

                newSub =
                    { sub
                        | tasks =
                            Array.toList <|
                                Array.set i newTask (Array.fromList model.sub.tasks)
                    }
            in
            { model
                | sub = newSub
            }


inverseSelect : Model -> Model
inverseSelect model =
    let
        sub =
            model.sub

        newSub =
            { sub
                | tasks =
                    List.map
                        (\task ->
                            { task | isSelected = not task.isSelected }
                        )
                        model.sub.tasks
            }
    in
    { model
        | sub = newSub
    }


messageEH : Model -> Http.Error -> Model
messageEH model httpError =
    let
        sub =
            model.sub

        newSub =
            { sub | message = Just (buildMessageEH httpError) }
    in
    { model
        | sub = newSub
    }


changeDpy : Model -> Int -> Model
changeDpy model dpy =
    let
        sub =
            model.sub

        newSub =
            { sub | dpy = Just dpy }
    in
    { model | sub = newSub }


switchSelect : Model -> Int -> Model
switchSelect model i =
    case Array.get i (Array.fromList model.sub.tasks) of
        Nothing ->
            model

        Just task ->
            let
                newTask =
                    { task | isSelected = not task.isSelected }

                sub =
                    model.sub

                newSub =
                    { sub
                        | tasks =
                            Array.toList <|
                                Array.set i newTask (Array.fromList model.sub.tasks)
                    }
            in
            { model
                | sub = newSub
            }



-- getDevModel : Model -> Cmd Msg
-- getDevModel m =
--     Http.get
--         { url = "http://localhost:8080/dev/model" ++ String.fromInt m.user.id
--         , expect = Http.expectJson ModelReceived modelDecoder
--         }


initialize : SubModel -> Cmd Msg
initialize m =
    Http.post
        { url = "http://localhost:8080/tasks/init"
        , body = Http.jsonBody (initialEncoder m)
        , expect = Http.expectJson SubModelReceived subModelDecoder
        }


textPost : SubModel -> Cmd Msg
textPost m =
    Http.post
        { url = "http://localhost:8080/tasks/post"
        , body = Http.jsonBody (textPostEncoder m)
        , expect = Http.expectJson TasksReceived tasksDecoder
        }


doneTasks : SubModel -> Cmd Msg
doneTasks m =
    Http.post
        { url = "http://localhost:8080/tasks/done"
        , body = Http.jsonBody (doneTasksEncoder m)
        , expect = Http.expectJson TasksReceived tasksDecoder
        }


switchStar : SubModel -> Int -> Cmd Msg
switchStar m i =
    Http.post
        { url = "http://localhost:8080/tasks/star"
        , body = Http.jsonBody (switchStarEncoder m i)
        , expect = Http.expectWhatever (StarSwitched i)
        }


focusTask : SubModel -> Int -> Cmd Msg
focusTask m i =
    Http.post
        { url = "http://localhost:8080/tasks/focus"
        , body = Http.jsonBody (focusTaskEncoder m i)
        , expect = Http.expectJson TasksReceived tasksDecoder
        }


buildMessageEH : Http.Error -> String
buildMessageEH httpError =
    case httpError of
        Http.BadUrl errorMessage ->
            errorMessage

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody errorMessage ->
            errorMessage


initialEncoder : SubModel -> Encode.Value
initialEncoder m =
    Encode.object
        [ ( "initialUser", Encode.int m.user.id )
        ]


textPostEncoder : SubModel -> Encode.Value
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
        [ ( "textPostUser", Encode.int m.user.id )
        , ( "textPostContent", Encode.string content )
        ]


doneTasksEncoder : SubModel -> Encode.Value
doneTasksEncoder m =
    let
        ids =
            List.map .id <| List.filter .isSelected <| m.tasks
    in
    Encode.object
        [ ( "doneTasksUser", Encode.int m.user.id )
        , ( "doneTasksIds", Encode.list Encode.int ids )
        ]


switchStarEncoder : SubModel -> Int -> Encode.Value
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
        [ ( "switchStarId", Encode.int taskId )
        ]


focusTaskEncoder : SubModel -> Int -> Encode.Value
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
        [ ( "focusTaskId", Encode.int taskId )
        ]


subModelDecoder : Decoder SubModel
subModelDecoder =
    Decode.succeed SubModel
        |> required "elmSubModelUser" userDecoder
        |> required "elmSubModelTasks" (list taskDecoder)
        |> optional "elmSubModelInputText" (nullable string) Nothing
        |> optional "elmSubModelDpy" (nullable int) Nothing
        |> optional "elmSubModelMessage" (nullable string) Nothing


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "elmUserId" int
        |> required "elmUserName" string
        |> required "elmUserAdmin" bool



-- |> optional "elmUserDefaultDpy" (nullable int) Nothing
-- |> optional "elmUserLookUp" (nullable int) Nothing
-- |> optional "elmUserLookDown" (nullable int) Nothing


tasksDecoder : Decoder (List Task)
tasksDecoder =
    list taskDecoder


taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "elmTaskId" int
        |> required "elmTaskIsDone" bool
        |> required "elmTaskIsStarred" bool
        |> optional "elmTaskTitle" (nullable string) Nothing
        |> optional "elmTaskLink" (nullable string) Nothing
        |> optional "elmTaskStart" (nullable int) Nothing
        |> optional "elmTaskDeadline" (nullable int) Nothing
        |> optional "elmTaskWeight" (nullable float) Nothing
        |> required "elmTaskUser" string
        -- |> optional "elmTaskSecUntilStart" (nullable int) Nothing
        -- |> optional "elmTaskSecUntilDeadline" (nullable int) Nothing
        |> optional "elmTaskIsSelected" bool False



-- VIEW


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
            , div [ id "account" ]
                [ text model.sub.user.name ]
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
                        , div [ id "create", onClick (CharacterKey 'c') ] []
                        , div [ id "reschedule", onClick (CharacterKey 'r') ] []
                        , div [ id "linksOpen", onClick (CharacterKey 'l') ] []
                        , div [ id "criticalPath", onClick (CharacterKey 'p') ] []
                        ]
                    , div [ class "messageBox" ]
                        [ text (viewMessage model.sub) ]
                    , div [ id "viewCmdBox" ]
                        [ div [ id "focus", onClick (CharacterKey 'f') ] []
                        , div [ id "archives", onClick (CharacterKey 'a') ] []
                        , div [ id "trunk", onClick (CharacterKey 't') ] []
                        , div [ id "buds", onClick (CharacterKey 'b') ] []
                        , div [ id "home", onClick (CharacterKey 'h') ] []
                        ]
                    ]
                , div [ id "mainBody" ]
                    [ viewTaskHeader model
                    , div [ id "tasks" ]
                        (List.map
                            (viewTask model)
                            (List.indexedMap Tuple.pair model.sub.tasks)
                        )
                    ]
                ]
            , div [ id "rSideBar" ] []
            ]
        , div [ id "fotter" ] []
        ]


viewMessage : SubModel -> String
viewMessage m =
    case m.message of
        Just me ->
            me

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
                    ++ strFromPosix m.zone m.asOfTime
                    ++ ", "
                    ++ (case m.sub.dpy of
                            Nothing ->
                                "-"

                            Just d ->
                                String.fromInt d
                       )
                    ++ " dpy"
                )
            ]
        , div [ class "deadline" ] []
        , div [ class "weight" ] []
        , div [ class "done" ] []
        ]


viewTask : Model -> ( Int, Task ) -> Html Msg
viewTask m ( idx, task ) =
    div
        [ class "task"
        , style "background-color"
            (if idx == m.indicator then
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
            [ text (viewTimeByDpy m.sub.dpy m.zone task.start) ]
        , div [ class "bar" ]
            [ text (barString m task) ]
        , div [ class "deadline" ]
            [ text (viewTimeByDpy m.sub.dpy m.zone task.deadline) ]
        , div [ class "weight" ]
            [ text (viewWeight task) ]
        , div [ class "assign" ]
            [ text (viewAssign m.sub.user task)
            ]
        ]


viewAssign : User -> Task -> String
viewAssign u t =
    if u.name == t.user then
        "me"

    else
        t.user


viewWeight : Task -> String
viewWeight task =
    if task.isDone then
        "DONE"

    else
        case task.weight of
            Nothing ->
                "-"

            Just w ->
                String.fromFloat w


viewTimeByDpy : Maybe Int -> Zone -> Maybe Int -> String
viewTimeByDpy mdpy z mt =
    case mdpy of
        Nothing ->
            "-"

        Just dpy ->
            case mt of
                Nothing ->
                    "-"

                Just t ->
                    let
                        p =
                            t |> (*) (10 ^ 3) |> millisToPosix

                        yea =
                            String.fromInt <| Time.toYear z p

                        mon =
                            strFromMonth <| Time.toMonth z p

                        day =
                            String.fromInt <| Time.toDay z p

                        hou =
                            String.fromInt <| Time.toHour z p

                        min =
                            String.fromInt <| Time.toMinute z p

                        sec =
                            String.fromInt <| Time.toSecond z p
                    in
                    if dpy <= yeaPerY then
                        yea

                    else if dpy <= monPerY then
                        String.right 2 yea ++ "/" ++ mon

                    else if dpy <= dayPerY then
                        mon ++ "/" ++ day

                    else if dpy <= houPerY then
                        "/" ++ day ++ " " ++ hou ++ ":"

                    else if dpy <= minPerY then
                        hou ++ ":" ++ min

                    else
                        min ++ "'" ++ sec


fill : Int -> String -> String -> String
fill n putty target =
    String.left n <| target ++ String.repeat n putty


barString : Model -> Task -> String
barString model task =
    let
        secUS =
            secUntil (.asOfTime model) (.start task)

        secUD =
            secUntil (.asOfTime model) (.deadline task)
    in
    barString_ model.sub.dpy task.weight secUS secUD


barString_ : Maybe Int -> Maybe Float -> Maybe Int -> Maybe Int -> String
barString_ mdpy weight secUS secUD =
    case mdpy of
        Nothing ->
            "-"

        Just dpy ->
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


secUntil : Posix -> Maybe Int -> Maybe Int
secUntil now target =
    let
        nowSec =
            now |> posixToMillis |> (//) (10 ^ 3)
    in
    Maybe.map ((-) nowSec) target


strFromPosix : Zone -> Posix -> String
strFromPosix z p =
    let
        yea =
            String.fromInt <| Time.toYear z p

        mon =
            strFromMonth <| Time.toMonth z p

        day =
            String.fromInt <| Time.toDay z p

        wed =
            strFromWeekday <| Time.toWeekday z p

        hou =
            String.fromInt <| Time.toHour z p

        min =
            String.fromInt <| Time.toMinute z p

        sec =
            String.fromInt <| Time.toSecond z p

        date =
            String.join "/" [ yea, mon, day ]

        time =
            hou ++ ":" ++ min ++ "'" ++ sec
    in
    String.join " " [ date, wed, time ]


strFromMonth : Month -> String
strFromMonth month =
    case month of
        Jan ->
            "1"

        Feb ->
            "2"

        Mar ->
            "3"

        Apr ->
            "4"

        May ->
            "5"

        Jun ->
            "6"

        Jul ->
            "7"

        Aug ->
            "8"

        Sep ->
            "9"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


strFromWeekday : Weekday -> String
strFromWeekday weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


textPlaceholder : String
textPlaceholder =
    "ENTER TASKS OR A COMMAND:\n\njump\n    step\n        hop\n\nA task to complete by the end of 2020 -2020/12/31\n    A task expected to take 300 hours $300\n        A task you can start in the beginning of 2020 2020/1/1-\n\nA time-critical task 2020/01/1- 23:59:59- $0.001 -0:0:3 -2020/1/02\n\ntrunk\n    branch Alice\n        bud \n    branch Bob\n        bud\n        bud\n\njump\n    step\n        hop2 dependent on hop1 [key\n    step\n        ]key hop1\n\n% A task to register as completed\n* A task to register as starred\n\nA linked task e.g. slack permalink &https://\n\n#777 The task with ID 777 whose weight will be updated to 30 $30\n\n#777 The complex task\n    A simpler task\n    A simpler task\n\nA new emerging task dependent on existing #777 and #888\n    #777\n    #888\n\nYOU CAN ALSO ENTER ONE OF THE FOLLOWING SLASH COMMANDS:\n\n/dpy 1\nSet default dpy (dots per year) to 1, that is, a dot represents a year.\n\n/asof 2020/01/01_12:0:0\nSet the time corresponding to the left edge of the bar to noon on January 1, 2020.\n\n/sel -t word\nSelect tasks whose title contains 'word'.\n\n/sel -s 2020/1/1_12:0:0< <2020/1/2\nSelect tasks whose start is in the afternoon of January 1, 2020.\n\n/sel -d <23:59:59\nSelect tasks whose deadline is today's end.\n\n/sel -w 30< <300\nSelect tasks whose weight is between 30 and 300 hours.\n\n/sel -arc\nSelect archived tasks.\n\n/sel -star\nSelect starred tasks.\n\n/sel -trunk\nSelect trunk, namely, tasks with no successor.\n\n/sel -buds\nSelect buds, namely, tasks with no predecessor.\n\n/sel -t word -s 2020/1/1< -d <23:59:59 -w 30< <300 -arc -star\nSpecify multiple conditions.\n\n/care 1 2\nCare from parents to grandchildren, namely, watch their tasks too,\nprovided you have permission for each.\n\nCOMMANDS FOR ADMINISTRATORS:\n\n/allow albert edit sci_team\nAllow Albert to edit sci_team tasks; create, update, and perform.\nAutomatically allow to view.\n\n/ban pisces_dep view albert\nBan pisces_dep from viewing Albert tasks.\nAutomatically ban from editing.\n\n/connect zodiac_inc pisces_dep\nGive zodiac_inc and pisces_dep a parent-child relationship.\nYou can, by default,\nview direct parents and all descendants and\nedit direct children.\n\nTHANK YOU FOR READING!\n"



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
