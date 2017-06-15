port module Capiguess exposing (..)

import Html exposing (text, Html, div, button)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (int, string, list, array, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, optionalAt)
import Random
import Random.List
import Array
import Types exposing (..)
import ZipList exposing (ZipList)


type alias Model =
    { countries : Countries
    , roundCountries : ZipList Country
    }


type Msg
    = NoOp
    | GetAllCountries (Result Http.Error Countries)
    | CountriesShuffled Countries
    | NextCountry
    | PreviousCountry


init =
    ( { countries = []
      , roundCountries = ZipList.init []
      }
    , getAllCountries
    )


decodeCountry : Decoder Country
decodeCountry =
    decode Country
        |> required "name" string
        |> required "capital" string
        |> optionalAt ["translations", "fr"] string ""


decodeCountries : Decoder Countries
decodeCountries =
    list decodeCountry


getAllCountries : Cmd Msg
getAllCountries =
    let
        url =
            "countries.json"

        request =
            Http.get url decodeCountries
    in
        Http.send GetAllCountries request


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


displayHeader : Model -> Html Msg
displayHeader model =
    div
        []
        [ text <| "Countries count: " ++ (toString <| List.length model.countries)
        , Html.hr [] []
        ]


displayCountry : Country -> Html Msg
displayCountry country =
    div
        []
        [ text ("What is the capital of " ++ country.fr ++ "? (" ++ country.capital ++ ")") ]


displayCountries : ZipList Country -> Html Msg
displayCountries countries =
    div
        []
    <|
        List.map displayCountry <|
            ZipList.toList countries


displayCurrent : ZipList Country -> Html Msg
displayCurrent countries =
    div
        []
    <|
        case ZipList.current countries of
            Nothing ->
                []

            Just country ->
                [ displayCountry country ]


displayButtons : Html Msg
displayButtons =
    div
        []
        [ button
            [ onClick PreviousCountry ]
            [ text "Previous" ]
        , button
            [ onClick NextCountry ]
            [ text "Next" ]
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ displayHeader model
        , displayCurrent model.roundCountries
        , displayButtons
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAllCountries (Ok countries) ->
            ( { model | countries = countries }
            , Random.generate CountriesShuffled (shuffleList countries)
            )

        GetAllCountries (Err _) ->
            ( model, Cmd.none )

        CountriesShuffled shuffledCountries ->
            ( { model | roundCountries = ZipList.init shuffledCountries }, Cmd.none )

        NextCountry ->
            ( { model | roundCountries = ZipList.next model.roundCountries }, Cmd.none )

        PreviousCountry ->
            ( { model | roundCountries = ZipList.previous model.roundCountries }, Cmd.none )

        NoOp ->
            model ! []


shuffleList : Countries -> Random.Generator Countries
shuffleList countries =
    Random.List.shuffle countries


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
