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
    { actresses : List Actress
    , searchParams : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actresses = []
      , searchParams = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetSearchParam String
    | NewActresses (Result Http.Error (List Actress))
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- 検索パラメータの追加 --
        SetSearchParam param ->
            if List.member param model.searchParams then
                ( { model | searchParams = List.filter (\p -> p /= param) model.searchParams }, Cmd.none )

            else
                ( { model | searchParams = param :: model.searchParams }, Cmd.none )

        NewActresses (Ok newactresses) ->
            ( { model | actresses = newactresses }, Cmd.none )

        NewActresses (Err _) ->
            ( model, Cmd.none )

        -- 検索の実行 --
        Submit ->
            ( model, getActresses model )



---- HTTP ----


url =
    "https://qbvj3e9oib.execute-api.ap-northeast-1.amazonaws.com/prod"


requestActresses : Model -> Http.Request (List Actress)
requestActresses model =
    let
        createQueryString : List String -> String
        createQueryString searchParams =
            url ++ "?params=" ++ String.join "," searchParams

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
    in
    Http.get (createQueryString model.searchParams) (Decode.list actress)


getActresses : Model -> Cmd Msg
getActresses model =
    Http.send NewActresses (requestActresses model)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        expectParams =
            [ "貧乳", "普乳", "巨乳", "爆乳", "超乳", "若手", "熟女", "貧尻", "巨尻", "低身長", "高身長" ]

        createButtons =
            List.map (\p -> button [ onClick (SetSearchParam p) ] [ text p ]) expectParams

        actresses2li actresses =
            List.map (\a -> li [] [ text a.name, img [ src a.image, width 150, height 150 ] [] ]) actresses
    in
    div []
        [ div [] createButtons
        , button [ onClick Submit ] [ text "検索" ]
        , ul [] (actresses2li model.actresses)
        ]


viewContentItem : String -> Html Msg
viewContentItem item =
    p [] [ text item ]


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
