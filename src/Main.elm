module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)


type Status
    = Completed
    | Incomplete
    | NeedHelp


type alias Task =
    { id : Int
    , title : String
    , status : Status
    }


type alias Model =
    { tasks : List Task
    , nextId : Int
    , newTitle : String
    , targetId : String
    , error : Maybe String
    }


type Msg
    = NewTitleChanged String
    | AddTask
    | TargetIdChanged String
    | RemoveTask
    | SetStatus Status


init : Model
init =
    { tasks = []
    , nextId = 1
    , newTitle = ""
    , targetId = ""
    , error = Nothing
    }


addTask : String -> Int -> List Task -> List Task
addTask title id tasks =
    { id = id
    , title = title
    , status = Incomplete
    }
        :: tasks


removeTask : Int -> List Task -> Maybe (List Task)
removeTask targetId tasks =
    case tasks of
        [] ->
            Nothing

        task :: rest ->
            if task.id == targetId then
                Just rest

            else
                case removeTask targetId rest of
                    Just newRest ->
                        Just (task :: newRest)

                    Nothing ->
                        Nothing


updateStatus : Int -> Status -> List Task -> Maybe (List Task)
updateStatus targetId newStatus tasks =
    case tasks of
        [] ->
            Nothing

        task :: rest ->
            if task.id == targetId then
                Just ({ task | status = newStatus } :: rest)

            else
                case updateStatus targetId newStatus rest of
                    Just newRest ->
                        Just (task :: newRest)

                    Nothing ->
                        Nothing


renderTasks : List Task -> Html Msg
renderTasks tasks =
    case tasks of
        [] ->
            div [ class "empty" ] [ text "No tasks yet." ]

        task :: rest ->
            div []
                [ renderTask task
                , renderTasks rest
                ]


renderTask : Task -> Html Msg
renderTask task =
    div [ class "task" ]
        [ div [ class "task-id" ] [ text ("#" ++ String.fromInt task.id) ]
        , div [ class "task-title" ] [ text task.title ]
        , div [ class ("status " ++ statusClass task.status) ]
            [ text (statusToString task.status) ]
        ]


statusToString : Status -> String
statusToString status =
    case status of
        Completed ->
            "Completed"

        Incomplete ->
            "Incomplete"
        NeedHelp -> 
            "Need Help"


statusClass : Status -> String
statusClass status =
    case status of
        Completed ->
            "status-completed"

        Incomplete ->
            "status-incomplete"

        NeedHelp -> 
            "status-needHelp"


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewTitleChanged title ->
            { model | newTitle = title }

        AddTask ->
            if String.isEmpty (String.trim model.newTitle) then
                { model | error = Just "Task title cannot be empty." }

            else
                { model
                    | tasks = addTask model.newTitle model.nextId model.tasks
                    , nextId = model.nextId + 1
                    , newTitle = ""
                    , error = Nothing
                }

        TargetIdChanged idText ->
            { model | targetId = idText }

        RemoveTask ->
            case String.toInt model.targetId of
                Nothing ->
                    { model | error = Just "ID must be a number." }

                Just id ->
                    case removeTask id model.tasks of
                        Just newTasks ->
                            { model
                                | tasks = newTasks
                                , targetId = ""
                                , error = Nothing
                            }

                        Nothing ->
                            { model | error = Just "No task with that ID." }

        SetStatus status ->
            case String.toInt model.targetId of
                Nothing ->
                    { model | error = Just "ID must be a number." }

                Just id ->
                    case updateStatus id status model.tasks of
                        Just newTasks ->
                            { model
                                | tasks = newTasks
                                , targetId = ""
                                , error = Nothing
                            }

                        Nothing ->
                            { model | error = Just "No task with that ID." }


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , div [ class "container" ]
            [ h1 [ class "title" ] [ text "Todo List" ]

            , div [ class "row" ]
                [ input
                    [ class "input"
                    , placeholder "New task"
                    , value model.newTitle
                    , onInput NewTitleChanged
                    ]
                    []
                , button [ class "btn primary", onClick AddTask ] [ text "Add" ]
                ]

            , div [ class "row" ]
                [ input
                    [ class "input small"
                    , placeholder "Task ID"
                    , value model.targetId
                    , onInput TargetIdChanged
                    ]
                    []
                , button [ class "btn", onClick RemoveTask ] [ text "Remove" ]
                , button [ class "btn", onClick (SetStatus Incomplete) ] [ text "Incomplete" ]
                , button [ class "btn", onClick (SetStatus Completed) ] [ text "Completed" ]
                , button [ class "btn", onClick (SetStatus NeedHelp) ] [ text "Need Help" ]
                ]

            , case model.error of
                Just err ->
                    div [ class "error" ] [ text err ]

                Nothing ->
                    text ""

            , div [ class "list" ] [ renderTasks model.tasks ]
            ]
        ]


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """
        body {
            margin: 0;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
            background: #f4f5f7;
            color: #222;
        }
        .container {
            max-width: 520px;
            margin: 40px auto;
            padding: 24px;
            background: #fff;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.06);
        }
        .title {
            margin: 0 0 16px;
            font-size: 22px;
        }
        .row {
            display: flex;
            gap: 8px;
            margin-bottom: 10px;
            flex-wrap: wrap;
        }
        .input {
            flex: 1;
            padding: 8px 10px;
            font-size: 14px;
            border: 1px solid #ccc;
            border-radius: 6px;
            outline: none;
        }
        .input.small {
            max-width: 100px;
        }
        .input:focus {
            border-color: #4a90e2;
        }
        .btn {
            padding: 8px 12px;
            font-size: 14px;
            border: 1px solid #ccc;
            border-radius: 6px;
            background: #fafafa;
            cursor: pointer;
        }
        .btn:hover {
            background: #f0f0f0;
        }
        .btn.primary {
            background: #4a90e2;
            border-color: #4a90e2;
            color: white;
        }
        .btn.primary:hover {
            background: #3a7bc8;
        }
        .error {
            color: #c0392b;
            font-size: 13px;
            margin: 6px 0;
        }
        .list {
            margin-top: 16px;
            border-top: 1px solid #eee;
        }
        .task {
            display: flex;
            align-items: center;
            gap: 10px;
            padding: 10px 0;
            border-bottom: 1px solid #eee;
        }
        .task-id {
            font-size: 12px;
            color: #999;
            min-width: 32px;
        }
        .task-title {
            flex: 1;
            font-size: 14px;
        }
        .status {
            font-size: 12px;
            padding: 3px 8px;
            border-radius: 999px;
        }
        .status-incomplete {
            background: #eee;
            color: #555;
        }
        .status-completed {
            background: #d4edda;
            color: #2e7d32;
        }
        .status-needHelp{
            background: #light-red;
            color: #green;
        }
        .empty {
            color: #999;
            font-size: 14px;
            padding: 12px 0;
        }
        """ ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }