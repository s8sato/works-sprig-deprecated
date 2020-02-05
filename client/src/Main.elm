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


type alias Model =
    { sub : SubModel
    , zone : Zone
    , currentTime : Posix
    , asOfTime : Posix
    , dot : Dot
    , indicator : Index
    , selectedTasks : List Int
    , asOfCurrentTime : Bool
    , underTyping : Bool
    , underHardTyping : Bool
    , underControl : Bool
    }


type alias Index =
    Int


type alias SubModel =
    { user : User
    , tasks : List Task
    , inputText : Maybe String
    , message : Message
    }


type alias Message =
    { code : Int
    , body : String
    }


type alias User =
    { id : Int
    , name : String
    , defaultDot : String
    , zoneName : Maybe String
    , zoneOffset : Maybe Int
    }


type alias Task =
    { id : Int
    , isDone : Bool
    , isStarred : Bool
    , title : Maybe String
    , link : Maybe String
    , startable : Maybe Millis -- POSIX milliseconds
    , deadline : Maybe Millis
    , weight : Maybe Float
    , assign : String
    , schedule : List Schedule
    }


type alias Millis =
    Int


type alias Schedule =
    { begin : Millis
    , end : Millis
    }


type Signature
    = Plus
    | Minus


type Dot
    = Yea
    | Qua
    | Mnt
    | Wee
    | Day
    | Sxh
    | Hou
    | Twm
    | Min
    | Sec


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initSub =
            { user = User 1 "ANONYMOUS" "D" Nothing Nothing
            , tasks = []
            , inputText = Nothing
            , message = Message 0 ""
            }
    in
    ( { sub = initSub
      , zone = Time.utc
      , currentTime = Time.millisToPosix 0
      , asOfTime = Time.millisToPosix 0
      , dot = Day
      , indicator = 0
      , selectedTasks = []
      , underTyping = False
      , underHardTyping = False
      , underControl = False
      , asOfCurrentTime = True
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = NoOp
    | SubModelReceived (Result Http.Error SubModel)
    | CharacterKey Char
    | ControlKey String
    | SwitchSelect Index
    | Input String
    | TextPost
    | StarSwitched Index (Result Http.Error SubModel)
    | SwitchStar Index
    | FocusTask Index
    | TaskFocused Int (Result Http.Error SubModel)
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

        CharacterKey 'u' ->
            ( model, undoneTasks model )

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
            ( changeDot model Yea, Cmd.none )

        CharacterKey '2' ->
            ( changeDot model Qua, Cmd.none )

        CharacterKey '3' ->
            ( changeDot model Mnt, Cmd.none )

        CharacterKey '4' ->
            ( changeDot model Wee, Cmd.none )

        CharacterKey '5' ->
            ( changeDot model Day, Cmd.none )

        CharacterKey '6' ->
            ( changeDot model Sxh, Cmd.none )

        CharacterKey '7' ->
            ( changeDot model Hou, Cmd.none )

        CharacterKey '8' ->
            ( changeDot model Twm, Cmd.none )

        CharacterKey '9' ->
            ( changeDot model Min, Cmd.none )

        CharacterKey 'a' ->
            ( model, showArchives model )

        CharacterKey 't' ->
            ( model, showTrunks model )

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

        CharacterKey 'w' ->
            ( asOfShift model Minus, Cmd.none )

        CharacterKey 'o' ->
            ( asOfShift model Plus, Cmd.none )

        CharacterKey _ ->
            ( model, Cmd.none )

        ControlKey _ ->
            ( model, Cmd.none )

        TextPost ->
            ( model, textPost model )

        StarSwitched i (Ok newSubModel) ->
            ( starSwitched model i newSubModel, Cmd.none )

        StarSwitched _ (Err httpError) ->
            ( messageEH model httpError, Cmd.none )

        SwitchStar index ->
            ( model, switchStar model index )

        FocusTask index ->
            ( model, focusTask model index )

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

        TaskFocused id (Ok newSubModel) ->
            ( taskFocused model id newSubModel, Cmd.none )

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


eval : Signature -> Int
eval sign =
    case sign of
        Plus ->
            1

        Minus ->
            -1


fromDot : Dot -> Int
fromDot dot =
    case dot of
        Yea ->
            1

        Qua ->
            4

        Mnt ->
            12

        Wee ->
            52

        Day ->
            365

        Sxh ->
            1460

        Hou ->
            8760

        Twm ->
            43800

        Min ->
            525600

        Sec ->
            31536000


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
                | user = m.user
                , tasks = m.tasks
                , message = m.message
            }

        newModel =
            { model
                | sub = newSub
                , selectedTasks = []
                , asOfCurrentTime = True
            }
    in
    case m.user.defaultDot of
        "Y" ->
            changeDot newModel Yea

        "Q" ->
            changeDot newModel Qua

        "M" ->
            changeDot newModel Mnt

        "W" ->
            changeDot newModel Wee

        "D" ->
            changeDot newModel Day

        "6h" ->
            changeDot newModel Sxh

        "h" ->
            changeDot newModel Hou

        "12m" ->
            changeDot newModel Twm

        "m" ->
            changeDot newModel Min

        "s" ->
            changeDot newModel Sec

        _ ->
            newModel


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


taskFocused : Model -> Int -> SubModel -> Model
taskFocused model id m =
    let
        enumeratedTasks =
            List.indexedMap Tuple.pair m.tasks

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
            { sub | tasks = m.tasks }
    in
    { model
        | sub = newSub
        , indicator = newIndicator
    }


starSwitched : Model -> Index -> SubModel -> Model
starSwitched model i m =
    case m.message.code of
        400 ->
            let
                sub =
                    model.sub

                newSub =
                    { sub | message = m.message }
            in
            { model | sub = newSub }

        200 ->
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

        _ ->
            model


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
            { sub | message = Message 400 (buildMessageEH httpError) }
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


changeDot : Model -> Dot -> Model
changeDot model dot =
    { model
        | dot = dot
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
                | user = m.user
                , tasks =
                    case m.message.code of
                        200 ->
                            m.tasks

                        _ ->
                            sub.tasks
                , message = m.message
                , inputText =
                    case m.message.code of
                        200 ->
                            Nothing

                        300 ->
                            Nothing

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
                    case m.message.code of
                        200 ->
                            m.tasks

                        _ ->
                            sub.tasks
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


asOfShift : Model -> Signature -> Model
asOfShift model sign =
    let
        shifted =
            if model.dot == Yea then
                dailyShift model.asOfTime sign 365

            else if model.dot == Qua then
                dailyShift model.asOfTime sign 90

            else if model.dot == Mnt then
                dailyShift model.asOfTime sign 30

            else
                let
                    diff =
                        eval sign * (10 ^ 3) * fromDot Sec // fromDot model.dot
                in
                posixToMillis model.asOfTime
                    + diff
                    |> millisToPosix
    in
    { model
        | asOfTime = shifted
        , asOfCurrentTime = False
    }


dailyShift : Posix -> Signature -> Int -> Posix
dailyShift asof sign days =
    dailyShift_ sign 0 days asof


dailyShift_ : Signature -> Int -> Int -> Posix -> Posix
dailyShift_ sign count days out =
    if count >= days then
        out

    else
        let
            daily =
                eval sign * (10 ^ 3) * fromDot Sec // fromDot Day
        in
        dailyShift_ sign (count + 1) days (millisToPosix <| daily + posixToMillis out)



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
            postJson "star" (userTaskEncoder m task.id) (StarSwitched i) subModelDecoder


focusTask : Model -> Index -> Cmd Msg
focusTask m i =
    case Array.get i (Array.fromList m.sub.tasks) of
        Nothing ->
            Cmd.none

        Just task ->
            postJson "focus" (userTaskEncoder m task.id) (TaskFocused task.id) subModelDecoder


goHome : Model -> Cmd Msg
goHome m =
    postJson "home" (userEncoder m.sub.user) ReturnedHome subModelDecoder


doneTasks : Model -> Cmd Msg
doneTasks m =
    postJson "done" (userTasksEncoder m) TasksDoneOrUndone subModelDecoder


undoneTasks : Model -> Cmd Msg
undoneTasks m =
    postJson "undone" (userTasksEncoder m) TasksDoneOrUndone subModelDecoder


cloneTasks : Model -> Cmd Msg
cloneTasks m =
    postJson "clone" (userTasksEncoder m) TasksCloned subModelDecoder


showArchives : Model -> Cmd Msg
showArchives m =
    postJson "arch" (userEncoder m.sub.user) TasksShown subModelDecoder


showTrunks : Model -> Cmd Msg
showTrunks m =
    postJson "trunks" (userEncoder m.sub.user) TasksShown subModelDecoder


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
        , ( "elmUserDefaultDot", Encode.string u.defaultDot )
        , ( "elmUserZoneName", maybe Encode.string u.zoneName )
        , ( "elmUserZoneOffset", maybe Encode.int u.zoneOffset )
        ]


userTasksEncoder : Model -> Encode.Value
userTasksEncoder m =
    Encode.object
        [ ( "elmUserTasksUser", userEncoder m.sub.user )
        , ( "elmUserTasksTasks", Encode.list Encode.int m.selectedTasks )
        ]


userTaskEncoder : Model -> Int -> Encode.Value
userTaskEncoder m i =
    Encode.object
        [ ( "elmUserTaskUser", userEncoder m.sub.user )
        , ( "elmUserTaskTask", Encode.int i )
        ]



-- DECODER


subModelDecoder : Decoder SubModel
subModelDecoder =
    Decode.succeed SubModel
        |> required "elmSubModelUser" userDecoder
        |> required "elmSubModelTasks" (list taskDecoder)
        |> optional "elmSubModelInputText" (nullable string) Nothing
        |> optional "elmSubModelMessage" messageDecoder (Message 0 "")


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "elmUserId" int
        |> required "elmUserName" string
        |> required "elmUserDefaultDot" string
        |> optional "elmUserZoneName" (nullable string) Nothing
        |> optional "elmUserZoneOffset" (nullable int) Nothing


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
        |> optional "elmTaskStartable" (nullable int) Nothing
        |> optional "elmTaskDeadline" (nullable int) Nothing
        |> optional "elmTaskWeight" (nullable float) Nothing
        |> required "elmTaskAssign" string
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
                    [ div [ id "changeDot" ] []
                    , div [ id "asOfShift" ] []
                    , div [ id "upAndDown" ] []
                    , div [ id "switchSelect" ] []
                    , div [ id "switchStar" ] []
                    , div [ id "focus" ] []
                    ]
                ]
            , div [ id "mainContainer" ]
                [ div [ id "mainHeader" ]
                    [ div [ id "selectionCmdBox" ]
                        [ div [ id "inverseSelect", onClick (CharacterKey 'i') ] []
                        , div [ id "eliminate", onClick (CharacterKey 'e') ] []
                        , div [ id "clone", onClick (CharacterKey 'c') ] []
                        ]
                    , div [ class "messageBox" ]
                        [ text model.sub.message.body ]
                    , div [ id "viewCmdBox" ]
                        [ div [ id "archives", onClick (CharacterKey 'a') ] []
                        , div [ id "trunks", onClick (CharacterKey 't') ] []
                        , div [ id "buds", onClick (CharacterKey 'b') ] []
                        , div [ id "home", onClick (CharacterKey 'h') ] []
                        ]
                    , div [ class "scroll" ] []
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


viewDateTimeUnit : Dot -> String
viewDateTimeUnit dot =
    if dot == Yea then
        "Y"

    else if dot == Qua then
        "Q"

    else if dot == Mnt then
        "M"

    else if dot == Wee then
        "W"

    else if dot == Day then
        "D"

    else if dot == Sxh then
        "6h"

    else if dot == Hou then
        "h"

    else if dot == Twm then
        "12m"

    else if dot == Min then
        "m"

    else if dot == Sec then
        "s"

    else
        "CSTM"


viewDateTimeGuide : Dot -> String
viewDateTimeGuide dot =
    if List.member dot [ Yea ] then
        "Y"

    else if List.member dot [ Qua, Mnt ] then
        "Y/M"

    else if List.member dot [ Wee, Day ] then
        "M/D"

    else if List.member dot [ Sxh, Hou ] then
        "/D h:"

    else if List.member dot [ Twm, Min ] then
        "h:m"

    else
        "m's"


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
            [ text (viewDateTimeUnit m.dot) ]
        , div [ class "bar" ]
            [ text
                ("As of "
                    ++ strFromPosix m.zone m.asOfTime
                )
            ]
        , div [ class "deadline" ]
            [ text (viewDateTimeGuide m.dot) ]
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
            [ text (viewTimeByDot m.dot m.zone task.startable) ]
        , div
            [ class "bar"
            , onClick (FocusTask i)
            ]
            [ text (barString m task) ]
        , div [ class "deadline" ]
            [ text (viewTimeByDot m.dot m.zone task.deadline) ]
        , div [ class "weight" ]
            [ text (viewWeight task) ]
        , div [ class "assign" ]
            [ text (viewAssign m.sub.user task)
            ]
        ]


viewAssign : User -> Task -> String
viewAssign u t =
    if u.name == t.assign then
        "me"

    else
        t.assign


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


viewTimeByDot : Dot -> Zone -> Maybe Millis -> String
viewTimeByDot dot z mt =
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
            if List.member dot [ Yea ] then
                yea

            else if List.member dot [ Qua, Mnt ] then
                String.right 2 yea ++ "/" ++ mon

            else if List.member dot [ Wee, Day ] then
                mon ++ "/" ++ day

            else if List.member dot [ Sxh, Hou ] then
                "/" ++ day ++ " " ++ hou ++ ":"

            else if List.member dot [ Twm, Min ] then
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
            position m.dot m.asOfTime t.startable

        d =
            position m.dot m.asOfTime t.deadline
    in
    preString m.dot m.asOfTime t
        |> replace s '['
        |> replace d ']'
        |> String.fromList


position : Dot -> Posix -> Maybe Millis -> Int
position dot asof mt =
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
                (fromDot dot * millis)
                    // (fromDot Sec * 10 ^ 3)


replace : Int -> Char -> List Char -> List Char
replace pos char target =
    target
        |> Array.fromList
        |> Array.set pos char
        |> Array.toList


preString : Dot -> Posix -> Task -> List Char
preString dot asofP task =
    let
        asofM =
            Time.posixToMillis asofP

        millisPerDot =
            (10 ^ 3) * fromDot Sec // fromDot dot
    in
    List.reverse <| preString_ 0 asofM millisPerDot task []


preString_ : Int -> Millis -> Millis -> Task -> List Char -> List Char
preString_ count left millisPerDot task out =
    if count >= 48 then
        out

    else
        let
            right =
                left + millisPerDot

            char =
                if List.any (\sch -> sch.begin < right && left < sch.end) task.schedule then
                    '#'

                else
                    '.'
        in
        preString_ (count + 1) right millisPerDot task (char :: out)


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
    "ENTER TASKS OR A COMMAND:\n\n\n\njump\n    step\n        hop\n\nA task to complete by the end of 2020 -2020/12/31\n    A task expected to take 300 hours $300\n        A task you can start in the beginning of 2020 2020/1/1-\n\nA time-critical task 2020/01/1- 23:59- $0.1 -0:5 -2020/1/02\n\ntrunk\n    branch Alice\n        bud \n    branch Bob\n        bud\n        bud\n\njump\n    step\n        hop2 dependent on hop1 [key\n    step\n        key] hop1\n\n% A task to register as completed\n* A task to register as starred\n\nA linked task e.g. slack permalink &https://\n\nA task to be registered being assigned to Bob, if allowed @Bob\n\n#777 The task with ID 777 whose weight will be updated to 30 $30\n\n#777 The complex task\n    A simpler task\n    A simpler task\n\nA new emerging task dependent on existing #777 and #888\n    #777\n    #888\n\n\n\nFOLLOW THESE 3 RULES WHEN USING THE # SIGN:\n\n\n\n1.\n    Each #ID can only be used once per post:\n\n        NG1\n            step\n                hop2\n                    #400 duplicate\n            step\n                #400 duplicate\n        OK1\n            step\n                hop2 [key\n            step\n                key] #200 unique\n\n2.\n    #IDs can only be placed as trunks or buds:\n\n        NG2\n            #400 NOT a trunk NOR a bud\n                hop\n\n3.\n    If placed as a bud, it must be an existing trunk:\n\n        NG3\n            #200 OK existing as a trunk\n            #400 NG existing NOT as a trunk\n\n\n\nYOU CAN ALSO ENTER ONE OF THE FOLLOWING SLASH COMMANDS:\n\n\n\n/dot h\nSet the default time unit represented by a dot to an hour.\n\n/sel -t foo bar\nSelect tasks whose title contains 'foo' OR 'bar'.\n\n/sel -nt foo bar\nSelect tasks whose title does not contains 'foo' NOR 'bar'.\n\n/sel -s <2020/1/1_0:0\nSelect tasks whose startable is before 2020.\n\n/sel -d 2020/1/1_0:0<\nSelect tasks whose deadline is in 2020 or later.\n\n/sel -w 30< -w <300\nSelect tasks whose weight is between 30 and 300 hours.\n\n/sel -a Alice Bob\nSelect tasks assigned to Alice OR Bob.\n\n/sel -na Alice Bob\nSelect tasks not assigned to Alice NOR Bob.\n\n/sel -arc\nSelect archived tasks.\n\n/sel -und\nSelect undone tasks.\n\n/sel -sta\nSelect starred tasks.\n\n/sel -tru\nSelect trunks, namely, tasks with no successor.\n\n/sel -bud\nSelect buds, namely, tasks with no predecessor.\n\n/sel -r 777< -r <888\nSelect the intersection of #777's successors and #888's predecessors.\n\n/sel -t foo bar -t baz -d <2020/1/1_0:0 -w 30< -und -tru\nSpecify multiple conditions.\n\n\n\nCOMMANDS FOR ADMINISTRATORS:\n\n\n\n/allow Albert edit sci_team\nAllow Albert to edit sci_team tasks; create, update, and perform.\nAutomatically allow to view.\n\n/ban pisces_dep view zodiac_inc\nBan pisces_dep from viewing zodiac_inc tasks.\nAutomatically ban from editing.\n\n\n\nTHANK YOU FOR READING!\n"



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
