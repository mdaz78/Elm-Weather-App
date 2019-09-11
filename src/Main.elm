module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



---- MODEL ----


type alias Model =
    { temperatureInfo : TemperatureInfo
    , city : String
    }


type alias TemperatureInfo =
    { name : String
    , windSpeed : Float
    , temperature : Float
    , pressure : Float
    , humidity : Float
    }


init =
    ( Model (TemperatureInfo "Did not load" 0 0 0 0) "", Cmd.none )



---- UPDATE ----


type Msg
    = GetTemp
    | CityInput String
    | NewTemp (Result Http.Error String)


update msg model =
    case msg of
        GetTemp ->
            ( model, getTemperature model.city )

        NewTemp (Ok json) ->
            let
                newTemperatureInfo =
                    decodeTemperatureInfo json
            in
            ( { model | temperatureInfo = newTemperatureInfo }, Cmd.none )

        NewTemp (Err _) ->
            ( model, Cmd.none )

        CityInput city ->
            ( { model | city = city }, Cmd.none )


getTemperature city =
    let
        url =
            "http://api.openweathermap.org/data/2.5/weather?q=" ++ city ++ "&APPID=d172dc99a5e30b034410018f07063660"
    in
    Http.get { url = url, expect = Http.expectString NewTemp }


decodeTemperatureInfo : String -> TemperatureInfo
decodeTemperatureInfo json =
    let
        name =
            Result.withDefault "Error decoding data!" (Decode.decodeString (Decode.field "name" Decode.string) json)

        windSpeed =
            Result.withDefault 0 (Decode.decodeString (Decode.at [ "wind", "speed" ] Decode.float) json)

        temperature =
            Result.withDefault 0 (Decode.decodeString (Decode.at [ "main", "temp" ] Decode.float) json)

        pressure =
            Result.withDefault 0 (Decode.decodeString (Decode.at [ "main", "pressure" ] Decode.float) json)

        humidity =
            Result.withDefault 0 (Decode.decodeString (Decode.at [ "main", "humidity" ] Decode.float) json)
    in
    TemperatureInfo name windSpeed temperature pressure humidity



---- VIEW ----


view model =
    div []
        [ input [ placeholder "City", onInput CityInput ] []
        , br [] []
        , button [ onClick GetTemp ] [ text "Get temperature" ]
        , br [] []
        , div [] [ text "Name: ", text model.temperatureInfo.name ]
        , div [] [ text "Temp: ", text (Debug.toString model.temperatureInfo.temperature) ]
        , div [] [ text "Wind: ", text (Debug.toString model.temperatureInfo.windSpeed) ]
        , div [] [ text "Pressure: ", text (Debug.toString model.temperatureInfo.pressure) ]
        , div [] [ text "Humidity: ", text (Debug.toString model.temperatureInfo.humidity) ]
        ]



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
