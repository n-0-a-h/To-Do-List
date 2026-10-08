module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, hr, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)


type Status
    = Completed
    | NeedHelp
    | Incomplete


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


{-| Add a new task to the front of the list.
-}
addTask : String -> Int -> List Task -> List Task
addTask title id tasks =
    { id = id
    , title = title
    , status = Incomplete
    }
        :: tasks


{-| Remove the first task with the given ID.

Recursion idea:
- If the list is empty, the ID was not found -> Nothing.
- If the first task matches, return the rest of the list.
- Otherwise, recurse on the rest, then rebuild the list with the first task.

-}
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


{-| Update the status of the first task with the given ID.

This uses the same recursive shape as removeTask, but instead of dropping the
matching task, it replaces it with an updated record.

-}
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


{-| Render all tasks recursively. No List.map here.
-}
renderTasks : List Task -> Html Msg
renderTasks tasks =
    case tasks of
        [] ->
            text "No tasks yet."

        task :: rest ->
            div []
                [ renderTask task
                , renderTasks rest
                ]


renderTask : Task -> Html Msg
renderTask task =
    div []
        [ text
            (String.fromInt task.id
                ++ ": "
                ++ task.title
                ++ " ["
                ++ statusToString task.status
                ++ "]"
            )
        ]


statusToString : Status -> String
statusToString status =
    case status of
        Completed ->
            "Completed"

        NeedHelp ->
            "Need Help"

        Incomplete ->
            "Incomplete"


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
        [ h1 [] [ text "Todo List" ]

        -- Add task
        , input
            [ placeholder "New task"
            , value model.newTitle
            , onInput NewTitleChanged
            ]
            []
        , button [ onClick AddTask ] [ text "Add" ]

        , hr [] []

        -- Target ID for remove/update
        , input
            [ placeholder "Task ID"
            , value model.targetId
            , onInput TargetIdChanged
            ]
            []
        , button [ onClick RemoveTask ] [ text "Remove by ID" ]

        -- Status buttons
        , div []
            [ button [ onClick (SetStatus Incomplete) ] [ text "Set Incomplete" ]
            , button [ onClick (SetStatus NeedHelp) ] [ text "Set Need Help" ]
            , button [ onClick (SetStatus Completed) ] [ text "Set Completed" ]
            ]

        -- Error message
        , case model.error of
            Just err ->
                div [ style "color" "red" ] [ text err ]

            Nothing ->
                text ""

        , hr [] []

        -- Task list, rendered recursively
        , renderTasks model.tasks
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }