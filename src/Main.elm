module Main exposing (Actress, Model, Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Paginate exposing (..)



---- MODEL ----


type alias Actress =
    { name : String, image : String, height : String, age : String, bust : String, cup : String, west : String, hip : String }


type alias Model =
    { actresses : List Actress
    , searchParams : List String
    , paginatedActresses : PaginatedList Actress
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { actresses = []
      , searchParams = []
      , paginatedActresses = Paginate.fromList 10 []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetSearchParam String
    | NewActresses (Result Http.Error (List Actress))
    | Submit
    | Next
    | Prev
    | First
    | Last
    | GoTo Int


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
            ( { model | actresses = newactresses, paginatedActresses = Paginate.fromList 10 newactresses }, Cmd.none )

        NewActresses (Err _) ->
            ( model, Cmd.none )

        -- 検索の実行 --
        Submit ->
            ( model, getActresses model )

        Next ->
            ( { model | paginatedActresses = Paginate.next model.paginatedActresses }, Cmd.none )

        Prev ->
            ( { model | paginatedActresses = Paginate.prev model.paginatedActresses }, Cmd.none )

        First ->
            ( { model | paginatedActresses = Paginate.first model.paginatedActresses }, Cmd.none )

        Last ->
            ( { model | paginatedActresses = Paginate.last model.paginatedActresses }, Cmd.none )

        GoTo index ->
            ( { model | paginatedActresses = Paginate.goTo index model.paginatedActresses }, Cmd.none )



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

        prevButtons =
            [ button [ onClick First, disabled <| Paginate.isFirst model.paginatedActresses ] [ text "<<" ]
            , button [ onClick Prev, disabled <| Paginate.isFirst model.paginatedActresses ] [ text "<" ]
            ]

        nextButtons =
            [ button [ onClick Next, disabled <| Paginate.isLast model.paginatedActresses ] [ text ">" ]
            , button [ onClick Last, disabled <| Paginate.isLast model.paginatedActresses ] [ text ">>" ]
            ]

        pagerButtonView index isActive =
            button
                [ style "font-weight"
                    (if isActive then
                        "bold"

                     else
                        "normal"
                    )
                , onClick <| GoTo index
                ]
                [ text <| String.fromInt index ]
    in
    div [] <|
        [ div [] createButtons
        , button [ onClick Submit ] [ text "検索" ]
        ]
            ++ [ p [] [ text ((String.fromInt <| Paginate.length model.paginatedActresses) ++ "人") ] ]
            ++ prevButtons
            ++ [ span []
                    [ text
                        (String.join " "
                            [ String.fromInt <| Paginate.currentPage model.paginatedActresses
                            , "/"
                            , String.fromInt <| Paginate.totalPages model.paginatedActresses
                            ]
                        )
                    ]
               ]
            ++ nextButtons
            ++ [ ul [] (actresses2li <| Paginate.page model.paginatedActresses) ]


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
