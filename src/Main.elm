module Main exposing (Actress, Model, Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)



---- MODEL ----


type alias Actress =
    { name : String, image : String, height : String, age : String, bust : String, cup : String, west : String, hip : String }


type alias Model =
    { actresses : List Actress, searchParams : List String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actresses = [], searchParams = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = AddSearchParam String
    | GetActresses
    | NewActresses (Result Http.Error (List Actress))
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- 検索パラメータの追加 --
        AddSearchParam searchParam ->
            ( { model | searchParams = searchParam :: model.searchParams }, Cmd.none )

        GetActresses ->
            ( model, getActresses )

        NewActresses (Ok newactresses) ->
            ( { model | actresses = newactresses }, Cmd.none )

        NewActresses (Err _) ->
            ( model, Cmd.none )

        -- 検索の実行 --
        Submit ->
            ( model, Cmd.none )



---- HTTP ----


url =
    "https://qbvj3e9oib.execute-api.ap-northeast-1.amazonaws.com/prod"


requestActresses : Http.Request (List Actress)
requestActresses =
    Http.get url (Decode.list actress)


actress : Decode.Decoder Actress
actress =
    Decode.succeed Actress
        |> required "name" string
        |> required "image" string
        |> optional "height" string "-"
        |> optional "age" string "-"
        |> optional "bust" string "-"
        |> optional "cup" string "-"
        |> optional "west" string "-"
        |> optional "hip" string "-"


getActresses : Cmd Msg
getActresses =
    Http.send NewActresses requestActresses



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetActresses ] [ text "Get Actresses" ] ]


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
