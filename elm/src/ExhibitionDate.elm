module ExhibitionDate exposing (..)

import Browser
import Date
import DatePicker exposing (DateEvent(..), defaultSettings, getInitialDate)
import Html exposing (Html)
import Html.Attributes
import Json.Decode



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Flags =
    { exhibitionList : Json.Decode.Value
    }


type alias Model =
    { exhibitionList : List Exhibition
    , selectedExhibition : String
    , date : Maybe Date.Date
    , datePicker : DatePicker.DatePicker
    }


type alias Exhibition =
    { title : String
    , startDate : String
    , endDate : String
    }


datePickerSettings : DatePicker.DatePicker -> DatePicker.Settings
datePickerSettings datePicker =
    let
        isDisabled : Date.Date -> Date.Date -> Bool
        isDisabled today date =
            Date.compare today date /= LT
    in
    { defaultSettings | isDisabled = isDisabled (getInitialDate datePicker) }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        decodedExhibitionList =
            case Json.Decode.decodeValue exhibitionListDecoder flags.exhibitionList of
                Ok goodExhibitionData ->
                    goodExhibitionData

                Err _ ->
                    [ { title = ""
                      , startDate = ""
                      , endDate = ""
                      }
                    ]

        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { exhibitionList = decodedExhibitionList
      , selectedExhibition = "My Exhibition"
      , date = Nothing
      , datePicker = datePicker
      }
    , Cmd.map ToDatePickerMsg datePickerCmd
    )


exhibitionDecoder : Json.Decode.Decoder Exhibition
exhibitionDecoder =
    Json.Decode.map3
        Exhibition
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "start_date" Json.Decode.string)
        (Json.Decode.field "end_date" Json.Decode.string)


exhibitionListDecoder : Json.Decode.Decoder (List Exhibition)
exhibitionListDecoder =
    Json.Decode.list exhibitionDecoder



-- UPDATE


type Msg
    = ToDatePickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToDatePickerMsg subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (datePickerSettings model.datePicker) subMsg model.datePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.date
            in
            ( { model
                | date = newDate
                , datePicker = newDatePicker
              }
            , Cmd.none
            )



-- HELPERS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h3 [] [ Html.text "Exhibitions" ]
        , Html.ul []
            (List.map
                (\{ title, startDate, endDate } ->
                    Html.li []
                        [ Html.b [] [ Html.text title ]
                        , Html.text (String.join " " [ " from", startDate, "to", endDate ])
                        ]
                )
                model.exhibitionList
            )
        , case model.date of
            Nothing ->
                Html.h1 [] [ Html.text "Select visit date" ]

            Just date ->
                Html.h1 [] [ Html.text (Date.format "MMM d, yyyy" date) ]
        , DatePicker.view model.date (datePickerSettings model.datePicker) model.datePicker
            |> Html.map ToDatePickerMsg
        ]
