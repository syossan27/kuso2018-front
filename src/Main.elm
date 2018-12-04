module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


---- MODEL ----


type alias Model =
    { title : String, todo : List String }


model : Model
model =
    Model "" []



---- UPDATE ----


type Msg
    = Title String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Title inputTitle ->
            { model | title = inputTitle }

        Submit ->
            { model | todo = model.title :: model.todo }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "TODO List" ]
        , input [ placeholder "タスク名", onInput Title ] []
        , button [ onClick Submit ] [ text "追加" ]
        , ul [] (todoList model.todo)
        ]


todoList : List String -> List (Html Msg)
todoList todo =
    List.map (\w -> li [] [ text w ]) todo



---- PROGRAM ----


main : Program Never Model Msg
main =
    Browser.element
    { view = view
        , model = model
        , update = update
        }
