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
    }

type alias Model =
    { todoItems : List TodoItem
    , currentTodo : String
    , show : Visualize
    }


initialModel : Model
initialModel =
    { todoItems = []
    , currentTodo = ""
    , show = All
    }


-- UPDATE

type Msg =
    SwitchShowMode Visualize
    | UpdateCurrentTodo String
    | AddTodo
    | RemoveTodo


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
                ( { model | currentTodo = "", todoItems = (TodoItem model.currentTodo False) :: model.todoItems }, Cmd.none )
            else
                ( model, Cmd.none )
        RemoveTodo ->
            ( model, Cmd.none )



-- VIEW

view : Model -> (Html Msg)
view model =
    div []
    [ input [ placeholder "Todo item", value model.currentTodo, onInput UpdateCurrentTodo ] [ ]
    , button [ onClick AddTodo ] [ text "Add" ]
    , br [] []
    , ul [] ( List.map itemView model.todoItems )
    , controlView model
    ]


itemView : TodoItem -> (Html Msg)
itemView todoItem =
    li []
    [ input [ type_ "checkbox" ] []
    , text todoItem.title
    , button [ onClick RemoveTodo ] [ text "X" ]
    ]


controlView : Model -> (Html Msg)
controlView model =
    if List.length model.todoItems /= 0 then
        text ( "items: " ++ toString(List.length model.todoItems) )
    else
        text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none