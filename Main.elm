module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

-- MODEL


type Visualize =
    All
    | Completed
    | Active


type alias TodoItem =
    { title : String
    , completed : Bool
    , id : Int
    }

type alias Model =
    { todoItems : List TodoItem
    , currentTodo : String
    , show : Visualize
    , currentIndex : Int
    }


initialModel : Model
initialModel =
    { todoItems = []
    , currentTodo = ""
    , show = All
    , currentIndex = 0
    }


-- UPDATE

type Msg =
    SwitchShowMode Visualize
    | UpdateCurrentTodo String
    | AddTodo
    | RemoveTodo Int
    | RemoveAll
    | ToggleCompleted Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SwitchShowMode mode ->
            case mode of
                All ->
                    ({ model | show = All}, Cmd.none)
                Completed ->
                    ({ model | show = Completed}, Cmd.none)
                Active ->
                    ({ model | show = Active}, Cmd.none)
        UpdateCurrentTodo currentTodo ->
            ( { model | currentTodo = currentTodo }, Cmd.none)
        AddTodo ->
            if not (String.isEmpty model.currentTodo) then
                ( { model | currentTodo = ""
                 , todoItems = (TodoItem model.currentTodo False model.currentIndex) :: model.todoItems
                 , currentIndex = (model.currentIndex + 1)
                 }, Cmd.none )
            else
                ( model, Cmd.none )
        RemoveTodo id ->
            let
                removeItemWithId : Int -> TodoItem -> Maybe TodoItem
                removeItemWithId id todoItem =
                    if id == todoItem.id then
                        Nothing
                    else
                        Just todoItem
            in
                ( { model | todoItems = (List.filterMap (removeItemWithId id) model.todoItems) }, Cmd.none )
        RemoveAll ->
            ( { model | todoItems = [] }, Cmd.none )
        ToggleCompleted id ->
            let
                toggleCompleted : Int -> TodoItem -> TodoItem
                toggleCompleted id todoItem =
                    if id == todoItem.id then
                        { todoItem | completed = not todoItem.completed }
                    else
                        todoItem
            in
                ( { model | todoItems = (List.map (toggleCompleted id) model.todoItems) }, Cmd.none)



-- VIEW

view : Model -> (Html Msg)
view model =
    div []
    [ input [ placeholder "Todo item", value model.currentTodo, onInput UpdateCurrentTodo ] [ ]
    , button [ onClick AddTodo ] [ text "Add" ]
    , br [] []
    , ul [] ( listView model )
    , controlView model
    ]


listView : Model -> List (Html Msg)
listView model =
    case model.show of
        All ->
            List.map itemView model.todoItems
        Active ->
            List.filter (\ todoItem -> not todoItem.completed ) model.todoItems
            |> List.map itemView
        Completed ->
            List.filter (\ todoItem -> todoItem.completed ) model.todoItems
                        |> List.map itemView



itemView : TodoItem -> (Html Msg)
itemView todoItem =
    li []
    [ input [ type_ "checkbox", checked todoItem.completed, onClick (ToggleCompleted todoItem.id) ] []
    , text (toString(todoItem.id) ++ " " ++ todoItem.title ++ " " ++ toString(todoItem.completed) )
    , button [ onClick (RemoveTodo todoItem.id) ] [ text "X" ]
    ]


controlView : Model -> (Html Msg)
controlView model =
    if List.length model.todoItems /= 0 then
        div []
        [ text ( "items: " ++ toString(List.length model.todoItems) )
        , br [] []
        , text "Show: "
        , button [ onClick (SwitchShowMode Active) ] [ text "Actives" ]
        , button [ onClick (SwitchShowMode Completed) ] [ text "Completed" ]
        , button [ onClick (SwitchShowMode All) ] [ text "All" ]
        , br [] []
        , button [ onClick RemoveAll ] [ text "Remove All" ]
        ]
    else
        text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none