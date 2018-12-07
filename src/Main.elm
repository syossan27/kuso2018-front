module Main exposing (Actress, Model, Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Actress =
    { name : String, image : String, height : Int, age : Int, bust : Int, cup : String, west : Int, hip : Int }


type alias Model =
    { actresses : List Actress, searchParams : List String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actresses = [], searchParams = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = AddSearchParam String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- 検索パラメータの追加 --
        AddSearchParam searchParam ->
            ( { model | searchParams = searchParam :: model.searchParams }, Cmd.none )

        -- 検索の実行 --
        Submit ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ text "hoge" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
