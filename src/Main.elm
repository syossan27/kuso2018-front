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
      , paginatedActresses = Paginate.fromList 40 []
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
                ( { model | searchParams = List.filter (\p -> p /= param) model.searchParams }, getActresses { model | searchParams = List.filter (\p -> p /= param) model.searchParams } )

            else
                ( { model | searchParams = param :: model.searchParams }, getActresses { model | searchParams = param :: model.searchParams } )

        NewActresses (Ok newactresses) ->
            ( { model | actresses = newactresses, paginatedActresses = Paginate.fromList 40 newactresses }, Cmd.none )

        NewActresses (Err _) ->
            ( { model | actresses = [], paginatedActresses = Paginate.fromList 40 [] }, Cmd.none )

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
            List.map (\p -> div [ class "column" ] [ button [ class "button is-success", onClick (SetSearchParam p) ] [ text p ] ]) expectParams

        prevButtons =
            [ button [ class "pagination-previous", onClick First, disabled <| Paginate.isFirst model.paginatedActresses ] [ text "<<" ]
            , button [ class "pagination-previous", onClick Prev, disabled <| Paginate.isFirst model.paginatedActresses ] [ text "<" ]
            ]

        nextButtons =
            [ button [ class "pagination-next", onClick Next, disabled <| Paginate.isLast model.paginatedActresses ] [ text ">" ]
            , button [ class "pagination-next", onClick Last, disabled <| Paginate.isLast model.paginatedActresses ] [ text ">>" ]
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

        actresses2li actresses =
            List.map (\a -> div [ class "column is-one-quarter" ] [ profileCard a ]) actresses

        profileCard actress =
            div [ class "card" ]
                [ div [ class "card-content" ]
                    [ div [ class "media" ]
                        [ div [ class "media-left" ]
                            [ figure [ class "image is-128x128" ]
                                [ img [ alt "Placeholder image", src actress.image ]
                                    []
                                ]
                            ]
                        , div [ class "media-content" ]
                            [ p [ class "title is-4" ]
                                [ text actress.name ]
                            , p [ class "is-6" ]
                                [ text ("バスト：" ++ actress.bust) ]
                            , p [ class "is-6" ]
                                [ text ("ウェスト：" ++ actress.west) ]
                            , p [ class "is-6" ]
                                [ text ("ヒップ：" ++ actress.hip) ]
                            , p [ class "is-6" ]
                                [ text ("カップ数：" ++ actress.cup) ]
                            , p [ class "is-6" ]
                                [ text ("年齢：" ++ actress.age) ]
                            ]
                        ]
                    , div [ class "content" ]
                        [ a [ target "_blank", href ("https://www.xvideos.com/?k=" ++ actress.name) ]
                            [ text "xvideos" ]
                        , br [] []
                        , a [ target "_blank", href ("http://www.dmm.co.jp/search/=/searchstr=" ++ actress.name ++ "/analyze=V1EBAVcEUQs_/limit=30/n1=FgRCTw9VBA4GAVhfWkIHWw__/n2=Aw1fVhQKX1ZRAlhMUlo5QQgBU1lR/sort=ranking/") ]
                            [ text "DMM" ]
                        ]
                    ]
                ]
    in
    div []
        [ div [ class "box" ] <|
            [ div [ class "columns is-centered" ]
                [ div [ class "field is-grouped is-grouped-multiline" ] createButtons
                ]
            , div [ class "columns is-centered" ]
                [ div [ class "column" ]
                    [ p [] [ text ("検索条件：" ++ String.join " , " model.searchParams) ]
                    , p [] [ text ("検索結果：" ++ (String.fromInt <| Paginate.length model.paginatedActresses) ++ "人") ]
                    ]
                ]
            , div [ class "columns is-centered" ]
                [ div [ class "column is-one-fifth" ]
                    [ nav [ class "pagination is-centered" ]
                        [ button [ class "pagination-previous", onClick First, disabled <| Paginate.isFirst model.paginatedActresses ] [ text "<<" ]
                        , button [ class "pagination-previous", onClick Prev, disabled <| Paginate.isFirst model.paginatedActresses ] [ text "<" ]
                        , ul [ class "pagination-list" ]
                            [ text
                                (String.join " "
                                    [ String.fromInt <| Paginate.currentPage model.paginatedActresses
                                    , "/"
                                    , String.fromInt <| Paginate.totalPages model.paginatedActresses
                                    ]
                                )
                            ]
                        , button
                            [ class "pagination-next", onClick Next, disabled <| Paginate.isLast model.paginatedActresses ]
                            [ text ">" ]
                        , button [ class "pagination-next", onClick Last, disabled <| Paginate.isLast model.paginatedActresses ] [ text ">>" ]
                        ]
                    ]
                ]
            ]
        , div [ class "columns is-multiline" ] (actresses2li <| Paginate.page model.paginatedActresses)
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
