module Main exposing (main)

import Array exposing (fromList, get, set, toList)
import Browser
import Browser.Dom as Dom exposing (blur, focus, getViewportOf, setViewportOf)
import Browser.Events exposing (onKeyDown, onKeyPress, onKeyUp)
import Config
import Html exposing (Html, a, div, text, textarea)
import Html.Attributes exposing (class, classList, href, id, placeholder, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
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
    , underHardTyping : Bool
    , underControl : Bool
    , asOfCurrentTime : Bool
    , dpy : Int
    , selectedTasks : List Int
    }


type alias SubModel =
    { user : User
    , tasks : List Task
    , inputText : Maybe String
    , message : Maybe Message
    }


type alias Millis =
    Int


type alias Schedule =
    { begin : Millis
    , end : Millis
    }


type alias User =
    { id : Int
    , name : String
    , admin : Bool
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
    , deadline : Maybe Millis
    , weight : Maybe Float
    , user : String
    , schedule : List Schedule
    }


type alias Message =
    { code : Int
    , body : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSub =
            { user = User 1 "ANONYMOUS" False Nothing Nothing Nothing
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
      , underHardTyping = False
      , underControl = False
      , asOfCurrentTime = True
      , dpy = dayPerY
      , selectedTasks = []
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


tnmPerY : Int
tnmPerY =
    52560


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
    | SetAsOfTime Posix
    | ReturnedHome (Result Http.Error SubModel)
    | ReadyTyping
    | ReleaseTyping
    | CharacterKeyUT Char
    | ControlKeyUT String
    | CharacterKeyUTRelease Char
    | ControlKeyUTRelease String
    | Tick Time.Posix
    | TasksCloned (Result Http.Error SubModel)
    | TextPosted (Result Http.Error SubModel)
    | TasksDoneOrUndone (Result Http.Error SubModel)
    | TasksShown (Result Http.Error SubModel)



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
            ( returnedHome model newSubModel
            , Cmd.none
            )

        SubModelReceived (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        CharacterKey 'k' ->
            ( { model
                | indicator =
                    if model.indicator > 0 then
                        model.indicator - 1

                    else
                        model.indicator
              }
            , followUp model 40
            )

        CharacterKey 'j' ->
            ( { model
                | indicator =
                    if model.indicator < List.length model.sub.tasks - 1 then
                        model.indicator + 1

                    else
                        model.indicator
              }
            , followDown model 40
            )

        CharacterKey 'e' ->
            ( model, doneTasks model )

        CharacterKey 'f' ->
            ( model, focusTask model model.indicator )

        CharacterKey 'c' ->
            ( model, cloneTasks model )

        CharacterKey '/' ->
            ( model
            , Task.attempt (\_ -> NoOp) (Dom.focus "inputArea")
            )

        CharacterKey 's' ->
            ( model, switchStar model model.indicator )

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
            ( changeDpy model tnmPerY, Cmd.none )

        CharacterKey '9' ->
            ( changeDpy model minPerY, Cmd.none )

        CharacterKey 'a' ->
            ( model, showArchives model )

        CharacterKey 't' ->
            ( model, showTrunk model )

        CharacterKey 'h' ->
            ( model, goHome model )

        CharacterKey 'b' ->
            ( model, showBuds model )

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
            ( model, textPost model )

        StarSwitched i (Ok _) ->
            ( starSwitched model i, Cmd.none )

        StarSwitched _ (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        SwitchStar index ->
            ( model, switchStar model index )

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
            , initialize model
            )

        Tick newTime ->
            ( { model
                | currentTime = newTime
                , asOfTime =
                    if model.asOfCurrentTime then
                        model.currentTime

                    else
                        model.asOfTime
              }
            , Cmd.none
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
                textPost model

              else
                Cmd.none
            )

        ControlKeyUT "Escape" ->
            ( model
            , Task.attempt (\_ -> NoOp) (Dom.blur "inputArea")
            )

        ControlKeyUT "ArrowUp" ->
            ( if model.underControl then
                { model | underHardTyping = False }

              else
                model
            , Cmd.none
            )

        ControlKeyUT "ArrowDown" ->
            ( if model.underControl then
                { model | underHardTyping = True }

              else
                model
            , Cmd.none
            )

        ControlKeyUT _ ->
            ( model, Cmd.none )

        CharacterKeyUTRelease _ ->
            ( model, Cmd.none )

        ControlKeyUTRelease "Control" ->
            ( { model | underControl = False }, Cmd.none )

        ControlKeyUTRelease _ ->
            ( model, Cmd.none )

        TasksCloned (Ok newSubModel) ->
            ( tasksCloned model newSubModel
            , Task.attempt (\_ -> NoOp) (Dom.focus "inputArea")
            )

        TasksCloned (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        TextPosted (Ok newSubModel) ->
            ( textPosted model newSubModel, Cmd.none )

        TextPosted (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        TasksDoneOrUndone (Ok newSubModel) ->
            ( tasksDoneOrUndone model newSubModel, Cmd.none )

        TasksDoneOrUndone (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        TasksShown (Ok newSubModel) ->
            ( tasksShown model newSubModel, Cmd.none )

        TasksShown (Err httpError) ->
            ( messageEH model httpError, Cmd.none )



-- HELPER FUNCTIONS


followUp : Model -> Float -> Cmd Msg
followUp m tH =
    let
        indPosY =
            tH * toFloat m.indicator
    in
    Dom.getViewportOf "tasks"
        |> Task.andThen
            (\info ->
                if indPosY < info.viewport.y + tH then
                    Dom.setViewportOf "tasks" 0 (indPosY - (info.viewport.height / 2))

                else
                    Dom.blur ""
            )
        |> Task.attempt (\_ -> NoOp)


followDown : Model -> Float -> Cmd Msg
followDown m tH =
    let
        indPosY =
            tH * toFloat m.indicator
    in
    Dom.getViewportOf "tasks"
        |> Task.andThen
            (\info ->
                if info.viewport.y + info.viewport.height - 3 * tH < indPosY then
                    Dom.setViewportOf "tasks" 0 (indPosY - (info.viewport.height / 2) + 2 * tH)

                else
                    Dom.blur ""
            )
        |> Task.attempt (\_ -> NoOp)


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

        newModel =
            { model
                | sub = newSub
                , selectedTasks = []
            }
    in
    case model.sub.user.defaultDpy of
        Nothing ->
            newModel

        Just dpy ->
            changeDpy newModel dpy


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
    { model
        | selectedTasks =
            let
                presence =
                    List.map .id model.sub.tasks

                imaginarySel =
                    List.filter (\id -> not <| List.member id presence) model.selectedTasks

                realSel =
                    List.filter (\id -> List.member id presence) model.selectedTasks

                inversedRealSel =
                    List.filter (\id -> not <| List.member id realSel) presence
            in
            List.concat [ inversedRealSel, imaginarySel ]
    }


messageEH : Model -> Http.Error -> Model
messageEH model httpError =
    let
        sub =
            model.sub

        newSub =
            { sub | message = Just <| Message 400 (buildMessageEH httpError) }
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
                | message = Just <| Message 200 (String.fromInt dpy ++ " dpy")
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
            { model
                | selectedTasks =
                    if List.member task.id model.selectedTasks then
                        List.filter ((/=) task.id) model.selectedTasks

                    else
                        List.append model.selectedTasks [ task.id ]
            }


tasksCloned : Model -> SubModel -> Model
tasksCloned model m =
    let
        sub =
            model.sub

        newSub =
            { sub
                | inputText = m.inputText
                , message = m.message
            }
    in
    { model
        | sub = newSub
        , selectedTasks = []
    }


textPosted : Model -> SubModel -> Model
textPosted model m =
    let
        sub =
            model.sub

        newSub =
            { sub
                | tasks =
                    case m.message of
                        Just me ->
                            case me.code of
                                200 ->
                                    m.tasks

                                _ ->
                                    sub.tasks

                        _ ->
                            sub.tasks
                , message = m.message
                , inputText =
                    case m.message of
                        Just me ->
                            case me.code of
                                200 ->
                                    Nothing

                                _ ->
                                    sub.inputText

                        _ ->
                            sub.inputText
            }
    in
    { model
        | sub = newSub
    }


tasksDoneOrUndone : Model -> SubModel -> Model
tasksDoneOrUndone model m =
    let
        sub =
            model.sub

        newSub =
            { sub
                | tasks =
                    case m.tasks of
                        [] ->
                            sub.tasks

                        _ ->
                            m.tasks
                , message = m.message
            }
    in
    { model
        | sub = newSub
        , selectedTasks = []
    }


tasksShown : Model -> SubModel -> Model
tasksShown model m =
    let
        sub =
            model.sub

        newSub =
            { sub
                | tasks =
                    case m.tasks of
                        [] ->
                            sub.tasks

                        _ ->
                            m.tasks
                , message = m.message
            }
    in
    { model
        | sub = newSub
    }



-- COMMANDS


postJson : String -> Encode.Value -> (Result Http.Error a -> Msg) -> Decoder a -> Cmd Msg
postJson path encoder resulting decoder =
    Http.post
        { url = "http://" ++ Config.host ++ ":8080/tasks/" ++ path
        , body = Http.jsonBody encoder
        , expect = Http.expectJson resulting decoder
        }


initialize : Model -> Cmd Msg
initialize m =
    postJson "init" (userEncoder m.sub.user) SubModelReceived subModelDecoder


textPost : Model -> Cmd Msg
textPost m =
    case m.sub.inputText of
        Nothing ->
            Cmd.none

        Just content ->
            postJson "post" (textPostEncoder m content) TextPosted subModelDecoder


switchStar : Model -> Index -> Cmd Msg
switchStar m i =
    case Array.get i (Array.fromList m.sub.tasks) of
        Nothing ->
            Cmd.none

        Just task ->
            Http.post
                { url = "http://" ++ Config.host ++ ":8080/tasks/star"
                , body = Http.jsonBody (userTaskIdEncoder m task.id)
                , expect = Http.expectWhatever (StarSwitched i)
                }


focusTask : Model -> Index -> Cmd Msg
focusTask m i =
    case Array.get i (Array.fromList m.sub.tasks) of
        Nothing ->
            Cmd.none

        Just task ->
            postJson "focus" (userTaskIdEncoder m task.id) (TaskFocused task.id) tasksDecoder


goHome : Model -> Cmd Msg
goHome m =
    postJson "home" (userEncoder m.sub.user) ReturnedHome subModelDecoder


doneTasks : Model -> Cmd Msg
doneTasks m =
    postJson "done" (userSelTasksEncoder m) TasksDoneOrUndone subModelDecoder


cloneTasks : Model -> Cmd Msg
cloneTasks m =
    postJson "clone" (userSelTasksEncoder m) TasksCloned subModelDecoder


showArchives : Model -> Cmd Msg
showArchives m =
    postJson "arch" (userEncoder m.sub.user) TasksShown subModelDecoder


showTrunk : Model -> Cmd Msg
showTrunk m =
    postJson "trunk" (userEncoder m.sub.user) TasksShown subModelDecoder


showBuds : Model -> Cmd Msg
showBuds m =
    postJson "buds" (userEncoder m.sub.user) TasksShown subModelDecoder



-- ENCODER


textPostEncoder : Model -> String -> Encode.Value
textPostEncoder m content =
    Encode.object
        [ ( "textPostUser", userEncoder m.sub.user )
        , ( "textPostContent", Encode.string content )
        ]


userEncoder : User -> Encode.Value
userEncoder u =
    Encode.object
        [ ( "elmUserId", Encode.int u.id )
        , ( "elmUserName", Encode.string u.name )
        , ( "elmUserAdmin", Encode.bool u.admin )

        -- , ( "elmUserDurations", Encode.list durationEncoder u.durs )
        , ( "elmUserDefaultDpy", maybe Encode.int u.defaultDpy )
        , ( "elmUserZoneName", maybe Encode.string u.zoneName )
        , ( "elmUserZoneOffset", maybe Encode.int u.zoneOffset )
        ]


userSelTasksEncoder : Model -> Encode.Value
userSelTasksEncoder m =
    Encode.object
        [ ( "userSelTasksUser", userEncoder m.sub.user )
        , ( "userSelTasksIds", Encode.list Encode.int m.selectedTasks )
        ]


userTaskIdEncoder : Model -> Int -> Encode.Value
userTaskIdEncoder m i =
    Encode.object
        [ ( "userTaskIdUser", userEncoder m.sub.user )
        , ( "userTaskIdTaskId", Encode.int i )
        ]



-- DECODER


subModelDecoder : Decoder SubModel
subModelDecoder =
    Decode.succeed SubModel
        |> required "elmSubModelUser" userDecoder
        |> required "elmSubModelTasks" (list taskDecoder)
        |> optional "elmSubModelInputText" (nullable string) Nothing
        |> optional "elmSubModelMessage" (nullable messageDecoder) Nothing


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "elmUserId" int
        |> required "elmUserName" string
        |> required "elmUserAdmin" bool
        |> optional "elmUserDefaultDpy" (nullable int) Nothing
        |> optional "elmUserZoneName" (nullable string) Nothing
        |> optional "elmUserZoneOffset" (nullable int) Nothing


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
        |> optional "elmTaskDeadline" (nullable int) Nothing
        |> optional "elmTaskWeight" (nullable float) Nothing
        |> required "elmTaskUser" string
        |> required "elmTaskSchedule" schedulesDecoder


messageDecoder : Decoder Message
messageDecoder =
    Decode.succeed Message
        |> required "elmMessageCode" int
        |> required "elmMessageBody" string


schedulesDecoder : Decoder (List Schedule)
schedulesDecoder =
    list scheduleDecoder


scheduleDecoder : Decoder Schedule
scheduleDecoder =
    Decode.succeed Schedule
        |> required "elmScheduleBegin" int
        |> required "elmScheduleEnd" int



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
                    , classList
                        [ ( "hard", model.underHardTyping )
                        ]
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
                        , div [ id "clone", onClick (CharacterKey 'c') ] []
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
        , div [ id "footer" ] []
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

    else if dpy == tnmPerY then
        "10m"

    else if dpy == minPerY then
        "m"

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
            me.body

        Nothing ->
            ""


viewTaskHeader : Model -> Html Msg
viewTaskHeader m =
    div [ id "taskHeader" ]
        [ div [ class "indicator" ] []
        , div [ class "selection" ]
            [ if List.isEmpty m.selectedTasks then
                div [] []

              else
                text (String.fromInt (List.length m.selectedTasks))
            ]
        , div [ class "star" ] []
        , div [ class "title" ] []
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
        , div [ class "scroll" ] []
        ]


isExpired : Model -> Task -> Bool
isExpired m task =
    let
        pastDeadline =
            case task.deadline of
                Nothing ->
                    False

                Just d ->
                    d < Time.posixToMillis m.currentTime
    in
    if not task.isDone && pastDeadline then
        True

    else
        False


isExecutable : Model -> Task -> Bool
isExecutable m task =
    let
        pastStartable =
            case task.startable of
                Nothing ->
                    False

                Just s ->
                    s < Time.posixToMillis m.currentTime
    in
    if not task.isDone && pastStartable then
        True

    else
        False


isOverload : Model -> Task -> Bool
isOverload _ task =
    let
        overBegin =
            case task.startable of
                Just s ->
                    List.any (\sche -> sche.begin < s) task.schedule

                _ ->
                    False

        overEnd =
            case task.deadline of
                Just d ->
                    List.any (\sche -> d < sche.end) task.schedule

                _ ->
                    False
    in
    if not task.isDone && (overBegin || overEnd) then
        True

    else
        False


viewTask : Model -> ( Index, Task ) -> Html Msg
viewTask m ( i, task ) =
    div
        [ classList
            [ ( "task", True )
            , ( "focused ", i == m.indicator )
            , ( "selected", List.member task.id m.selectedTasks )
            , ( "expired", isExpired m task )
            , ( "executable", isExecutable m task )
            , ( "overload", isOverload m task )
            ]
        ]
        [ div [ class "indicator" ] []
        , div
            [ class "selection"
            , onClick (SwitchSelect i)
            ]
            [ if List.member task.id m.selectedTasks then
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


fillL : Int -> String -> String -> String
fillL n putty target =
    String.right n <| String.repeat n putty ++ target


barString : Model -> Task -> String
barString m t =
    let
        s =
            position m.dpy m.asOfTime t.startable

        d =
            position m.dpy m.asOfTime t.deadline
    in
    preString m.dpy m.asOfTime t
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
            if millis < 0 then
                -1

            else
                (dpy * millis)
                    // (secPerY * 10 ^ 3)


replace : Int -> Char -> List Char -> List Char
replace pos char target =
    target
        |> Array.fromList
        |> Array.set pos char
        |> Array.toList


preString : Int -> Posix -> Task -> List Char
preString dpy asofP task =
    let
        asofM =
            Time.posixToMillis asofP

        dot =
            (10 ^ 3) * secPerY // dpy
    in
    List.reverse <| preString_ 0 asofM dot task []


preString_ : Int -> Millis -> Millis -> Task -> List Char -> List Char
preString_ count left dot task out =
    if count >= 48 then
        out

    else
        let
            right =
                left + dot

            char =
                if List.any (\sch -> sch.begin < right && left < sch.end) task.schedule then
                    '#'

                else
                    '.'
        in
        preString_ (count + 1) right dot task (char :: out)


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

        date =
            String.join "/" [ yea, mon, day ]

        time =
            hou ++ ":" ++ min
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
    "ENTER TASKS OR A COMMAND:\n\n\n\njump\n    step\n        hop\n\nA task to complete by the end of 2020 -2020/12/31\n    A task expected to take 300 hours $300\n        A task you can start in the beginning of 2020 2020/1/1-\n\nA time-critical task 2020/01/1- 23:59- $0.1 -0:5 -2020/1/02\n\ntrunk\n    branch Alice\n        bud \n    branch Bob\n        bud\n        bud\n\njump\n    step\n        hop2 dependent on hop1 [key\n    step\n        key] hop1\n\n% A task to register as completed\n* A task to register as starred\n\nA linked task e.g. slack permalink &https://\n\nA task to be registered being assigned to Bob, if allowed @Bob\n\n#777 The task with ID 777 whose weight will be updated to 30 $30\n\n#777 The complex task\n    A simpler task\n    A simpler task\n\nA new emerging task dependent on existing #777 and #888\n    #777\n    #888\n\n\n\nFOLLOW THESE 3 RULES WHEN USING THE # SIGN:\n\n\n\n1.\n    Each #ID can only be used once per post:\n\n        NG1\n            step\n                hop2\n                    #400 duplicate\n            step\n                #400 duplicate\n        OK1\n            step\n                hop2 [key\n            step\n                key] #200 unique\n\n2.\n    #IDs can only be placed as trunks or buds:\n\n        NG2\n            #400 NOT a trunk NOR a bud\n                hop\n\n3.\n    If placed as a bud, it must be an existing trunk:\n\n        NG3\n            #200 OK existing as a trunk\n            #400 NG existing NOT as a trunk\n\n\n\nYOU CAN ALSO ENTER ONE OF THE FOLLOWING SLASH COMMANDS:\n\n\n\n/dpy 1\nSet default dpy (dots per year) to 1, that is, a dot represents a year.\n\n/asof 2020/01/01 12:0\nSet the time corresponding to the left edge of dots to noon on January 1, 2020.\n\n/pause\nFix the chart.\n\n/tick\nUpdate the chart every second (default).\n\n/sel -l foo \nSelect tasks whose title LIKEs 'foo'.\n\n/sel -nl %b_a_r%\nSelect tasks whose title does NOT LIKE '%b_a_r%'.\n\n/sel -s 2020/1/1< 12:0< <2020/1/2\nSelect tasks whose startable is in the afternoon of January 1, 2020.\n\n/sel -d <23:59\nSelect tasks whose deadline is today's end.\n\n/sel -w 30< <300\nSelect tasks whose weight is between 30 and 300 hours.\n\n/sel -a Bob\nSelect tasks assigned to Bob.\n\n/sel -arch\nSelect archived tasks.\n\n/sel -star\nSelect starred tasks.\n\n/sel -trunk\nSelect trunks, namely, tasks with no successor.\n\n/sel -bud\nSelect buds, namely, tasks with no predecessor.\n\n/sel #777< <#888\nSelect intersection of #777's successor and #888's predecessor.\n\n/sel -l %baz% -s 2020/1/1< -d <23:59 -w 30< <300 -arch -star\nSpecify multiple conditions.\n\n/care 1 2\nCare from parents to grandchildren, namely, watch their tasks too,\nprovided you have permission for each.\n\n\n\nCOMMANDS FOR ADMINISTRATORS:\n\n\n\n/allow Albert edit sci_team\nAllow Albert to edit sci_team tasks; create, update, and perform.\nAutomatically allow to view.\n\n/ban pisces_dep view Albert\nBan pisces_dep from viewing Albert tasks.\nAutomatically ban from editing.\n\n/connect zodiac_inc pisces_dep\nGive zodiac_inc and pisces_dep a parent-child relationship.\nYou can, by default,\nview direct parents and all descendants and\nedit direct children.\n\n\n\nTHANK YOU FOR READING!\n"



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
