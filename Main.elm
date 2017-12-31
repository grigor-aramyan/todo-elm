port module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Json.Encode exposing (encode, Value, string, list)
import Styled exposing (..)
import Styled.Colors exposing (pink, lightGray, lightPink, white, lightBlue)


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


-- DECODERS

modelDecoder : Json.Decoder Model
modelDecoder =
    Json.map4 Model
        (Json.at ["todoItems"] (Json.list todoItemDecoder) )
        (Json.at ["currentTodo"] Json.string)
        (Json.at ["show"] Json.string |> Json.andThen visualizeDecoder )
        (Json.at ["currentIndex"] Json.int)

todoItemDecoder : Json.Decoder TodoItem
todoItemDecoder =
    Json.map3 TodoItem
        (Json.at ["title"] Json.string)
        (Json.at ["completed"] Json.bool)
        (Json.at ["id"] Json.int)

visualizeDecoder : String -> Json.Decoder Visualize
visualizeDecoder tag =
    case tag of
        "All" -> Json.succeed All
        "Completed" -> Json.succeed Completed
        "Active" -> Json.succeed Active
        _ -> Json.fail(tag ++ " is not recognized as visualize value")


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
    Json.Encode.object
        [
            ("todoItems", Json.Encode.list (List.map todoItemToValue model.todoItems) ),
            ("currentTodo", Json.Encode.string model.currentTodo ),
            ("show", visualizeToValue model.show ),
            ("currentIndex", Json.Encode.int model.currentIndex )
        ]

todoItemToValue : TodoItem -> Json.Encode.Value
todoItemToValue todoItem =
    Json.Encode.object
        [
            ("title", Json.Encode.string todoItem.title ),
            ("completed", Json.Encode.bool todoItem.completed ),
            ("id", Json.Encode.int todoItem.id )
        ]

visualizeToValue : Visualize -> Json.Encode.Value
visualizeToValue visualize =
    case visualize of
        All -> Json.Encode.string "All"
        Completed -> Json.Encode.string "Completed"
        Active -> Json.Encode.string "Active"


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
    | RemoveTodo Int
    | RemoveAll
    | ToggleCompleted Int
    | KeyDown Int
    | Load Model
    | Save
    | NoOp


onKeyDown : ( Int -> msg ) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


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
                ( { model | todoItems = (List.map (toggleCompleted id) model.todoItems) }, Cmd.none )
        KeyDown keyCode ->
            if (keyCode == 13) && (not (String.isEmpty model.currentTodo)) then
                ( { model | currentTodo = ""
                    , todoItems = (TodoItem model.currentTodo False model.currentIndex) :: model.todoItems
                    , currentIndex = (model.currentIndex + 1)
                    }, Cmd.none )
            else
                ( model, Cmd.none )
        Save ->
            ( model, save (modelToValue model) )
        Load model ->
            ( model, Cmd.none )
        NoOp ->
            ( model, Cmd.none )


-- STYLED COMPONENTS

container =
    styled div
        [ Styled.height (percent 100)
        , display flex_
        , justifyContent center
        , alignItems center
        , backgroundColor lightGray
        , flexDirection column
         ]


innerContainer =
    styled div
        [ display flex_
        , alignItems center
        , Styled.width (percent 50)
        , backgroundColor white
        , flexDirection column
        , padding (px 4)
        ]


title =
    styled h1
        [ fontSize (Styled.em 3.5)
        , textAlign center
        , color lightPink
        , fontFamily monospace
        ]


styledInput =
    styled input
        [ padding (Styled.em 1)
        , border (px 2) solid pink
        , borderRadius (px 30)
        , fontSize (Styled.em 1)
        ]

styledUl =
    styled ul
        [ listStyleType none
         , padding (px 0)
         , margin (px 0)
         ]

styledLi =
    styled li
        [ Styled.width (px 200)
        , display flex_
        , justifyContent spaceBetween
        , alignItems center
        , borderRadius (px 20)
        , border (px 1) solid lightGray
        , margin (px 2)
        , padding (px 4)
        , fontSize (Styled.em 1.5)
        ]

styledDelete =
    styled button
        [ border (px 0) solid white
        , backgroundColor white
        ]


styledControlButtons =
    styled button
        [ backgroundColor white
        , border (px 1) solid lightGray
        , margin (px 2)
        , padding (px 5)
        ]
styledActiveControlButtons =
    styled button
        [ backgroundColor white
        , border (px 1) solid pink
        , margin (px 2)
        , padding (px 5)
        ]


styledSaveButton =
    styled button
        [ backgroundColor white
        , border (px 2) solid lightBlue
        , padding (px 8)
        , borderRadius (px 20)
        , float right_
        ]



-- VIEW

view : Model -> (Html Msg)
view model =
    container []
        [ title [] [ text "todos" ]
        , innerContainer []
            [ div []
                [ styledInput [ placeholder "Todo item", value model.currentTodo,
                    onInput UpdateCurrentTodo, onKeyDown KeyDown ] [ ]
                ]
                , br [] []
                , styledUl [ ] ( listView model )
                , br [] []
                , controlView model
            ]
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
    styledLi []
    [ input [ type_ "checkbox", checked todoItem.completed, onClick (ToggleCompleted todoItem.id) ] []
    , text ( todoItem.title )
    , styledDelete [ onClick (RemoveTodo todoItem.id) ] [ text "X" ]
    ]


controlView : Model -> (Html Msg)
controlView model =
    if List.length model.todoItems /= 0 then
        div []
        [ text ( "items: " ++ toString(List.length model.todoItems) )
        , br [] []
        , text "Show: "
        , (if (model.show == Active) then styledActiveControlButtons else styledControlButtons)
            [ onClick (SwitchShowMode Active) ] [ text "Actives" ]
        , (if (model.show == Completed) then styledActiveControlButtons else styledControlButtons)
            [ onClick (SwitchShowMode Completed) ] [ text "Completed" ]
        , (if (model.show == All) then styledActiveControlButtons else styledControlButtons)
            [ onClick (SwitchShowMode All) ] [ text "All" ]
        , br [] [], br [] []
        , styledControlButtons [ onClick RemoveAll ] [ text "Remove All" ]
        , styledSaveButton [ onClick Save ] [ text "Save" ]
        ]
    else
        text ""


decodeModel : Json.Value -> Msg
decodeModel value =
    let
        result =
            Json.decodeValue modelDecoder value
    in
        case result of
            Ok model ->
                Load model
            Err _ ->
                NoOp



-- PORTS
port save : Json.Value -> Cmd msg

port load : ( Json.Value -> msg ) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    load (decodeModel)