module Main exposing (..)

-- module Main exposing (main)

import Array exposing (fromList, get, set, toList)
import Browser
import Browser.Dom exposing (blur, focus)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp)
import Html exposing (Html, a, div, text, textarea)
import Html.Attributes exposing (class, classList, href, id, placeholder, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Maybe
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, ZoneName(..), millisToPosix, posixToMillis)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Index =
    Int


type alias Model =
    { sub : SubModel
    , zone : Zone
    , currentTime : Posix
    , asOfTime : Posix
    , indicator : Index
    , underTyping : Bool
    , underControl : Bool
    , asOfCurrentTime : Bool
    , dpy : Int
    }


type alias SubModel =
    { user : User
    , tasks : List Task
    , inputText : Maybe String
    , message : Maybe String
    }


type alias Millis =
    Int


type alias Duration =
    { left : Millis
    , right : Millis
    }


type alias User =
    { id : Int
    , name : String
    , admin : Bool
    , durs : List Duration
    , defaultDpy : Maybe Int
    , zoneName : Maybe String
    , zoneOffset : Maybe Int
    }


type alias Task =
    { id : Int
    , isDummy : Bool
    , isDone : Bool
    , isStarred : Bool
    , title : Maybe String
    , link : Maybe String
    , startable : Maybe Millis -- POSIX milliseconds
    , begin : Maybe Millis
    , end : Maybe Millis
    , deadline : Maybe Millis
    , weight : Maybe Float
    , user : String
    , isSelected : Bool
    , isExpired : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSub =
            let
                saraly =
                    [ { left = 30600000, right = 43200000 }
                    , { left = 46800000, right = 63000000 }
                    ]
            in
            { user = User 1 "ANONYMOUS" False saraly Nothing Nothing Nothing
            , tasks = []
            , inputText = Nothing
            , message = Nothing
            }
    in
    ( { sub = initSub
      , zone = Time.utc
      , currentTime = Time.millisToPosix 0
      , asOfTime = Time.millisToPosix 0
      , indicator = 0
      , underTyping = False
      , underControl = False
      , asOfCurrentTime = True
      , dpy = dayPerY
      }
      -- , Cmd.none
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


sxhPerY : Int
sxhPerY =
    1460


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
    = NoOp
    | SubModelReceived (Result Http.Error SubModel)
    | TasksReceived (Result Http.Error (List Task))
    | CharacterKey Char
    | ControlKey String
    | SwitchSelect Index
    | Input String
    | TextPost
    | StarSwitched Index (Result Http.Error ())
    | SwitchStar Index
    | TaskFocused Int (Result Http.Error (List Task))
    | AdjustTimeZone Zone
    | SetZoneName ZoneName
    | UpdateStatus Posix
    | SetAsOfTime Posix
    | ReturnedHome (Result Http.Error SubModel)
    | ReadyTyping
    | ReleaseTyping
    | CharacterKeyUT Char
    | ControlKeyUT String
    | CharacterKeyUTRelease Char
    | ControlKeyUTRelease String
    | Tick Time.Posix



-- | Sort Param Direction
-- | Reschedule
-- | Configure
-- | Next
-- | Prev
-- | Search (List Condition)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubModelReceived (Ok newSubModel) ->
            ( returnedHome
                { model
                    | sub = newSubModel
                }
                newSubModel
            , Cmd.none
            )

        SubModelReceived (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        TasksReceived (Ok newTasks) ->
            ( let
                sub =
                    model.sub

                newSub =
                    { sub
                        | tasks = newTasks
                        , inputText = Nothing
                        , message = Nothing
                    }
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

        CharacterKey 'f' ->
            ( model, focusTask model.sub model.indicator )

        -- APIに送る：[taskId]
        -- APIからもらう：String
        CharacterKey 'c' ->
            -- TODO
            ( model
            , Task.attempt (\_ -> NoOp) (focus "inputArea")
            )

        CharacterKey '/' ->
            ( model
            , Task.attempt (\_ -> NoOp) (focus "inputArea")
            )

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
            ( changeDpy model sxhPerY, Cmd.none )

        CharacterKey '7' ->
            ( changeDpy model houPerY, Cmd.none )

        CharacterKey '8' ->
            ( changeDpy model minPerY, Cmd.none )

        CharacterKey '9' ->
            ( changeDpy model secPerY, Cmd.none )

        CharacterKey '0' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 't' ->
            -- TODO
            ( model, Cmd.none )

        CharacterKey 'h' ->
            -- TODO
            ( model, goHome model.sub )

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

        CharacterKey _ ->
            ( model, Cmd.none )

        ControlKey _ ->
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

        TaskFocused id (Ok newTasks) ->
            ( taskFocused model id newTasks, Cmd.none )

        TaskFocused _ (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Task.perform SetZoneName Time.getZoneName
            )

        SetZoneName newZoneName ->
            ( setZoneName model newZoneName
            , initialize model.sub
            )

        Tick newTime ->
            ( { model | currentTime = newTime }
            , Task.perform UpdateStatus (Task.succeed newTime)
            )

        UpdateStatus currentTime ->
            ( updateStatus model currentTime
            , if model.asOfCurrentTime then
                Task.perform SetAsOfTime (Task.succeed model.currentTime)

              else
                Cmd.none
            )

        SetAsOfTime aTime ->
            ( { model | asOfTime = aTime }, Cmd.none )

        ReturnedHome (Ok newSubModel) ->
            ( returnedHome model newSubModel
            , Task.perform SetAsOfTime (Task.succeed model.currentTime)
            )

        ReturnedHome (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        ReadyTyping ->
            ( { model | underTyping = True }, Cmd.none )

        ReleaseTyping ->
            ( { model | underTyping = False }, Cmd.none )

        CharacterKeyUT _ ->
            ( model, Cmd.none )

        ControlKeyUT "Control" ->
            ( { model | underControl = True }, Cmd.none )

        ControlKeyUT "Enter" ->
            ( model
            , if model.underControl then
                textPost model.sub

              else
                Cmd.none
            )

        ControlKeyUT "Escape" ->
            ( model
            , Task.attempt (\_ -> NoOp) (blur "inputArea")
            )

        ControlKeyUT _ ->
            ( model, Cmd.none )

        CharacterKeyUTRelease _ ->
            ( model, Cmd.none )

        ControlKeyUTRelease "Control" ->
            ( { model | underControl = False }, Cmd.none )

        ControlKeyUTRelease _ ->
            ( model, Cmd.none )



-- HELPER FUNCTIONS


returnedHome : Model -> SubModel -> Model
returnedHome model m =
    let
        sub =
            model.sub

        newSub =
            { sub
                | tasks = m.tasks
                , message = m.message
            }
    in
    case model.sub.user.defaultDpy of
        Nothing ->
            { model | sub = newSub }

        Just dpy ->
            changeDpy { model | sub = newSub } dpy



-- barRedraw : Model -> Posix -> Model
-- barRedraw model asOfTime =
--     let
--         sub =
--             model.sub
--         newSub =
--             { sub |  }
--     in
--     { model | sub = newSub }


updateStatus : Model -> Posix -> Model
updateStatus model now =
    let
        sub =
            model.sub

        checkedTasks =
            sub.tasks
                |> List.map
                    (\task ->
                        let
                            pastDeadline =
                                case millisUntil task.deadline now of
                                    Nothing ->
                                        False

                                    Just millis ->
                                        millis < 0
                        in
                        if not task.isDone && pastDeadline then
                            { task | isExpired = True }

                        else
                            { task | isExpired = False }
                    )

        checkedSub =
            { sub | tasks = checkedTasks }
    in
    { model | sub = checkedSub }


setZoneName : Model -> ZoneName -> Model
setZoneName model zn =
    let
        sub =
            model.sub

        newSub =
            let
                user =
                    sub.user

                newUser =
                    case zn of
                        Name name ->
                            { user | zoneName = Just name }

                        Offset offset ->
                            { user | zoneOffset = Just offset }
            in
            { sub | user = newUser }
    in
    { model
        | sub = newSub
    }


taskFocused : Model -> Int -> List Task -> Model
taskFocused model id tasks =
    let
        enumeratedTasks =
            List.indexedMap Tuple.pair tasks

        enumeratedIds =
            List.map (Tuple.mapSecond .id) enumeratedTasks

        newIndicator =
            case List.filter ((==) id << Tuple.second) enumeratedIds of
                [] ->
                    0

                shouldSingleton ->
                    case List.head shouldSingleton of
                        Nothing ->
                            0

                        Just pair ->
                            Tuple.first pair

        sub =
            model.sub

        newSub =
            { sub | tasks = tasks }
    in
    { model
        | sub = newSub
        , indicator = newIndicator
    }


starSwitched : Model -> Index -> Model
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


changeDpy : Model -> Int -> Model
changeDpy model dpy =
    let
        sub =
            model.sub

        newSub =
            { sub
                | message = Just (String.fromInt dpy ++ " dpy")
            }
    in
    { model
        | sub = newSub
        , dpy = dpy
    }


switchSelect : Model -> Index -> Model
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
-- COMMANDS


initialize : SubModel -> Cmd Msg
initialize m =
    Http.post
        { url = "http://localhost:8080/tasks/init"
        , body = Http.jsonBody (initialEncoder m)
        , expect = Http.expectJson SubModelReceived subModelDecoder
        }


textPost : SubModel -> Cmd Msg
textPost m =
    case m.inputText of
        Nothing ->
            Cmd.none

        Just content ->
            Http.post
                { url = "http://localhost:8080/tasks/post"
                , body = Http.jsonBody (textPostEncoder m content)
                , expect = Http.expectJson TasksReceived tasksDecoder
                }


doneTasks : SubModel -> Cmd Msg
doneTasks m =
    Http.post
        { url = "http://localhost:8080/tasks/done"
        , body = Http.jsonBody (doneTasksEncoder m)
        , expect = Http.expectJson TasksReceived tasksDecoder
        }


switchStar : SubModel -> Index -> Cmd Msg
switchStar m i =
    case Array.get i (Array.fromList m.tasks) of
        Nothing ->
            Cmd.none

        Just task ->
            Http.post
                { url = "http://localhost:8080/tasks/star"
                , body = Http.jsonBody (switchStarEncoder task.id)
                , expect = Http.expectWhatever (StarSwitched i)
                }


focusTask : SubModel -> Index -> Cmd Msg
focusTask m i =
    case Array.get i (Array.fromList m.tasks) of
        Nothing ->
            Cmd.none

        Just task ->
            Http.post
                { url = "http://localhost:8080/tasks/focus"
                , body = Http.jsonBody (focusTaskEncoder task.id)
                , expect = Http.expectJson (TaskFocused task.id) tasksDecoder
                }


goHome : SubModel -> Cmd Msg
goHome m =
    Http.post
        { url = "http://localhost:8080/tasks/home"
        , body = Http.jsonBody (goHomeEncoder m)
        , expect = Http.expectJson ReturnedHome subModelDecoder
        }



-- ENCODER


initialEncoder : SubModel -> Encode.Value
initialEncoder m =
    Encode.object
        [ ( "initialUser", Encode.int m.user.id )
        ]


textPostEncoder : SubModel -> String -> Encode.Value
textPostEncoder m content =
    Encode.object
        [ ( "textPostUser", userEncoder m.user )
        , ( "textPostContent", Encode.string content )
        ]


userEncoder : User -> Encode.Value
userEncoder u =
    -- let
    --     defaultDpy =
    --         case u.defaultDpy of
    --             Nothing ->
    --                 Encode.null
    --             Just dpy ->
    --                 Encode.int
    --     zoneName =
    --         case u.zoneName of
    --             Nothing ->
    --                 Encode.null
    --             Just name ->
    --                 Encode.string name
    --     zoneOffset =
    --         case u.zoneOffset of
    --             Nothing ->
    --                 Encode.null
    --             Just offset ->
    --                 Encode.int offset
    -- in
    Encode.object
        [ ( "elmUserId", Encode.int u.id )
        , ( "elmUserName", Encode.string u.name )
        , ( "elmUserAdmin", Encode.bool u.admin )
        , ( "elmUserDurations", Encode.list durationEncoder u.durs )
        , ( "elmUserDefaultDpy", maybe Encode.int u.defaultDpy )
        , ( "elmUserZoneName", maybe Encode.string u.zoneName )
        , ( "elmUserZoneOffset", maybe Encode.int u.zoneOffset )
        ]


durationEncoder : Duration -> Encode.Value
durationEncoder dur =
    Encode.object
        [ ( "elmDurationLeft", Encode.int dur.left )
        , ( "elmDurationRight", Encode.int dur.right )
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


switchStarEncoder : Int -> Encode.Value
switchStarEncoder i =
    Encode.object
        [ ( "switchStarId", Encode.int i )
        ]


focusTaskEncoder : Int -> Encode.Value
focusTaskEncoder i =
    Encode.object
        [ ( "focusTaskId", Encode.int i )
        ]


goHomeEncoder : SubModel -> Encode.Value
goHomeEncoder m =
    Encode.object
        [ ( "goHomeUser", Encode.int m.user.id )
        ]



-- DECODER


subModelDecoder : Decoder SubModel
subModelDecoder =
    Decode.succeed SubModel
        |> required "elmSubModelUser" userDecoder
        |> required "elmSubModelTasks" (list taskDecoder)
        |> optional "elmSubModelInputText" (nullable string) Nothing
        |> optional "elmSubModelMessage" (nullable string) Nothing


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "elmUserId" int
        |> required "elmUserName" string
        |> required "elmUserAdmin" bool
        |> required "elmUserDurations" durationsDecoder
        |> optional "elmUserDefaultDpy" (nullable int) Nothing
        |> optional "elmUserZoneName" (nullable string) Nothing
        |> optional "elmUserZoneOffset" (nullable int) Nothing



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
        |> required "elmTaskIsDummy" bool
        |> required "elmTaskIsDone" bool
        |> required "elmTaskIsStarred" bool
        |> optional "elmTaskTitle" (nullable string) Nothing
        |> optional "elmTaskLink" (nullable string) Nothing
        |> optional "elmTaskStartable" (nullable int) Nothing
        |> optional "elmTaskBegin" (nullable int) Nothing
        |> optional "elmTaskEnd" (nullable int) Nothing
        |> optional "elmTaskDeadline" (nullable int) Nothing
        |> optional "elmTaskWeight" (nullable float) Nothing
        |> required "elmTaskUser" string
        |> optional "elmTaskIsSelected" bool False
        |> optional "elmTaskIsExpired" bool False


durationsDecoder : Decoder (List Duration)
durationsDecoder =
    list durationDecoder


durationDecoder : Decoder Duration
durationDecoder =
    Decode.succeed Duration
        |> required "elmDurationLeft" int
        |> required "elmDurationRight" int



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "header" ]
            [ div [ id "logo" ] []
            , div [ id "inputBox" ]
                [ textarea
                    [ id "inputArea"
                    , value (viewInputValue model)
                    , onInput Input
                    , placeholder textPlaceholder
                    , onFocus ReadyTyping
                    , onBlur ReleaseTyping
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


viewInputValue : Model -> String
viewInputValue model =
    case model.sub.inputText of
        Nothing ->
            ""

        Just str ->
            str


viewDateTimeUnit : Int -> String
viewDateTimeUnit dpy =
    if dpy == yeaPerY then
        "Y"

    else if dpy == quaPerY then
        "Q"

    else if dpy == monPerY then
        "M"

    else if dpy == weePerY then
        "W"

    else if dpy == dayPerY then
        "D"

    else if dpy == sxhPerY then
        "6h"

    else if dpy == houPerY then
        "h"

    else if dpy == minPerY then
        "m"

    else if dpy == secPerY then
        "s"

    else
        "CSTM"


viewDateTimeGuide : Int -> String
viewDateTimeGuide dpy =
    if dpy <= yeaPerY then
        "Y"

    else if dpy <= monPerY then
        "Y/M"

    else if dpy <= dayPerY then
        "M/D"

    else if dpy <= houPerY then
        "/D h:"

    else if dpy <= minPerY then
        "h:m"

    else
        "m's"


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
        [ div [ class "indicator" ] []
        , div [ class "selection" ] []
        , div [ class "star" ] []
        , div [ class "title" ]
            [ div [ class "dev" ]
                [ text (strFromPosix m.zone m.currentTime) ]
            ]
        , div [ class "startable" ]
            [ text (viewDateTimeUnit m.dpy) ]
        , div [ class "bar" ]
            [ text
                ("As of "
                    ++ strFromPosix m.zone m.asOfTime
                )
            ]
        , div [ class "deadline" ]
            [ text (viewDateTimeGuide m.dpy) ]
        , div [ class "weight" ] []
        , div [ class "assign" ] []
        ]


viewTask : Model -> ( Index, Task ) -> Html Msg
viewTask m ( i, task ) =
    div
        [ classList
            [ ( "task", True )
            , ( "focused ", i == m.indicator )
            , ( "selected", task.isSelected )
            , ( "expired", task.isExpired )
            ]
        ]
        [ div [ class "indicator" ] []
        , div
            [ class "selection"
            , onClick (SwitchSelect i)
            ]
            [ if task.isSelected then
                text "SEL"

              else
                text "-"
            ]
        , div
            [ class "star"
            , onClick (SwitchStar i)
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
        , div [ class "startable" ]
            [ text (viewTimeByDpy m.dpy m.zone task.startable) ]
        , div [ class "bar" ]
            [ text (barString m task) ]
        , div [ class "deadline" ]
            [ text (viewTimeByDpy m.dpy m.zone task.deadline) ]
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


viewTimeByDpy : Int -> Zone -> Maybe Millis -> String
viewTimeByDpy dpy z mt =
    case mt of
        Nothing ->
            "-"

        Just t ->
            let
                p =
                    millisToPosix t

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


fillR : Int -> String -> String -> String
fillR n putty target =
    String.left n <| target ++ String.repeat n putty


fillL : Int -> String -> String -> String
fillL n putty target =
    String.right n <| String.repeat n putty ++ target


millisFromWeight : Float -> Millis
millisFromWeight w =
    floor (60 * 60 * 10 ^ 3 * w)


millisUntil : Maybe Millis -> Posix -> Maybe Millis
millisUntil target now =
    let
        nowMillis =
            now |> posixToMillis
    in
    Maybe.map (\t -> t - nowMillis) target


barString : Model -> Task -> String
barString m t =
    let
        s =
            position m.dpy m.asOfTime t.startable

        d =
            position m.dpy m.asOfTime t.deadline
    in
    preString m.dpy m.asOfTime t m.sub.user.durs
        |> replace s '['
        |> replace d ']'
        |> String.fromList


position : Int -> Posix -> Maybe Millis -> Int
position dpy asof mt =
    case mt of
        Nothing ->
            -1

        Just t ->
            let
                millis =
                    t - posixToMillis asof
            in
            (dpy * millis)
                // (secPerY * 10 ^ 3)


replace : Int -> Char -> List Char -> List Char
replace pos char target =
    target
        |> Array.fromList
        |> Array.set pos char
        |> Array.toList


preString : Int -> Posix -> Task -> List Duration -> List Char
preString dpy asof task durs =
    case ( task.begin, task.end ) of
        ( Just b, Just e ) ->
            preString_ 0 dpy (posixToMillis asof) b e durs []

        _ ->
            List.repeat 48 '.'


preString_ : Int -> Int -> Millis -> Millis -> Millis -> List Duration -> List Char -> List Char
preString_ count dpy asof begin end durs store =
    if count >= 48 then
        store

    else
        let
            char =
                if isInDur asof durs then
                    if begin < asof && asof < end then
                        '#'

                    else
                        '-'

                else
                    '.'
        in
        preString_ (count + 1) dpy (next dpy asof) begin end durs (char :: store)


next : Int -> Millis -> Millis
next dpy t =
    t + (secPerY * 10 ^ 3 // dpy)


isInDur : Millis -> List Duration -> Bool
isInDur time durs =
    List.any (\d -> d.left < time && time < d.right) durs


strFromPosix : Zone -> Posix -> String
strFromPosix z p =
    let
        yea =
            String.fromInt <| Time.toYear z p

        mon =
            fillL 2 "0" <| strFromMonth <| Time.toMonth z p

        day =
            fillL 2 "0" <| String.fromInt <| Time.toDay z p

        wed =
            strFromWeekday <| Time.toWeekday z p

        hou =
            fillL 2 "0" <| String.fromInt <| Time.toHour z p

        min =
            fillL 2 "0" <| String.fromInt <| Time.toMinute z p

        sec =
            fillL 2 "0" <| String.fromInt <| Time.toSecond z p

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
    "ENTER TASKS OR A COMMAND:\n\njump\n    step\n        hop\n\nA task to complete by the end of 2020 -2020/12/31\n    A task expected to take 300 hours $300\n        A task you can start in the beginning of 2020 2020/1/1-\n\nA time-critical task 2020/01/1- 23:59:59- $0.001 -0:0:3 -2020/1/02\n\ntrunk\n    branch Alice\n        bud \n    branch Bob\n        bud\n        bud\n\njump\n    step\n        hop2 dependent on hop1 [key\n    step\n        ]key hop1\n\n% A task to register as completed\n* A task to register as starred\n\nA linked task e.g. slack permalink &https://\n\n#777 The task with ID 777 whose weight will be updated to 30 $30\n\n#777 The complex task\n    A simpler task\n    A simpler task\n\nA new emerging task dependent on existing #777 and #888\n    #777\n    #888\n\nYOU CAN ALSO ENTER ONE OF THE FOLLOWING SLASH COMMANDS:\n\n/dpy 1\nSet default dpy (dots per year) to 1, that is, a dot represents a year.\n\n/asof 2020/01/01_12:0:0\nSet the time corresponding to the left edge of dots to noon on January 1, 2020.\n\n/pause\nFix the chart.\n\n/tick\nUpdate the chart every second.\n\n/sel -t word\nSelect tasks whose title contains 'word'.\n\n/sel -s 2020/1/1_12:0:0< <2020/1/2\nSelect tasks whose start is in the afternoon of January 1, 2020.\n\n/sel -d <23:59:59\nSelect tasks whose deadline is today's end.\n\n/sel -w 30< <300\nSelect tasks whose weight is between 30 and 300 hours.\n\n/sel -arc\nSelect archived tasks.\n\n/sel -star\nSelect starred tasks.\n\n/sel -trunk\nSelect trunk, namely, tasks with no successor.\n\n/sel -buds\nSelect buds, namely, tasks with no predecessor.\n\n/sel -t word -s 2020/1/1< -d <23:59:59 -w 30< <300 -arc -star\nSpecify multiple conditions.\n\n/care 1 2\nCare from parents to grandchildren, namely, watch their tasks too,\nprovided you have permission for each.\n\nCOMMANDS FOR ADMINISTRATORS:\n\n/allow albert edit sci_team\nAllow Albert to edit sci_team tasks; create, update, and perform.\nAutomatically allow to view.\n\n/ban pisces_dep view albert\nBan pisces_dep from viewing Albert tasks.\nAutomatically ban from editing.\n\n/connect zodiac_inc pisces_dep\nGive zodiac_inc and pisces_dep a parent-child relationship.\nYou can, by default,\nview direct parents and all descendants and\nedit direct children.\n\nTHANK YOU FOR READING!\n"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.underTyping then
        Sub.batch
            [ Time.every 1000 Tick
            , onKeyDown keyDecoderUT
            , onKeyUp keyDecoderUTRelease
            ]

    else
        Sub.batch
            [ Time.every 1000 Tick
            , onKeyPress keyDecoder

            -- , onClick (Decode.succeed MouseClick)
            ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


keyDecoderUT : Decode.Decoder Msg
keyDecoderUT =
    Decode.map toMsgUT (Decode.field "key" Decode.string)


keyDecoderUTRelease : Decode.Decoder Msg
keyDecoderUTRelease =
    Decode.map toMsgUTRelease (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue


toMsgUT : String -> Msg
toMsgUT keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyUT char

        _ ->
            ControlKeyUT keyValue


toMsgUTRelease : String -> Msg
toMsgUTRelease keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKeyUTRelease char

        _ ->
            ControlKeyUTRelease keyValue
