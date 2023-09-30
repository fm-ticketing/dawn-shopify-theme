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
    , closedDateList : Json.Decode.Value
    }


type alias Model =
    { exhibitionList : List Exhibition
    , closedDateList : List ClosedDate
    , date : Maybe Date.Date
    , datePicker : DatePicker.DatePicker
    }


type alias Exhibition =
    { title : String
    , startDate : String
    , endDate : String
    }


type alias ClosedDate =
    { closedOn : List Date.Date
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

        decodedClosedDateList =
            case Json.Decode.decodeValue closedDateListDecoder flags.closedDateList of
                Ok goodClosedDateData ->
                    goodClosedDateData

                Err _ ->
                    [ { closedOn = []
                      }
                    ]

        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { exhibitionList = decodedExhibitionList
      , closedDateList = decodedClosedDateList
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


closedDateDecoder : Json.Decode.Decoder ClosedDate
closedDateDecoder =
    Json.Decode.map
        ClosedDate
        (Json.Decode.field "museum_closed_on" (Json.Decode.list Json.Decode.string)
            |> Json.Decode.andThen dateFromStringList
        )


closedDateListDecoder : Json.Decode.Decoder (List ClosedDate)
closedDateListDecoder =
    Json.Decode.list closedDateDecoder


dateFromString : String -> Date.Date
dateFromString maybeDate =
    case Date.fromIsoString maybeDate of
        Ok aDate ->
            aDate

        Err _ ->
            Date.fromRataDie 1


dateFromStringList : List String -> Json.Decode.Decoder (List Date.Date)
dateFromStringList maybeDateStrings =
    Json.Decode.succeed (List.map (\maybeDate -> dateFromString maybeDate) maybeDateStrings)



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
        , Html.ul []
            (List.map
                (\date ->
                    Html.li []
                        [ Html.text (Date.format "d MMM yyy" date) ]
                )
                (List.concat
                    (List.map (\{ closedOn } -> closedOn)
                        model.closedDateList
                    )
                )
            )
        , case model.date of
            Nothing ->
                Html.h1 [] [ Html.text "Select visit date" ]

            Just date ->
                Html.h1 []
                    [ Html.text (String.join " " [ maybeExhibitionTitle model, Date.format "d MMM yyyy" date ])
                    ]
        , DatePicker.view model.date (datePickerSettings model.datePicker) model.datePicker
            |> Html.map ToDatePickerMsg
        ]


maybeExhibitionTitle : Model -> String
maybeExhibitionTitle { date, exhibitionList } =
    List.map
        (\exhibition ->
            let
                startDate =
                    dateFromString exhibition.startDate

                endDate =
                    dateFromString exhibition.endDate

                selectedDate =
                    case date of
                        Just aSelectedDate ->
                            aSelectedDate

                        Nothing ->
                            Date.fromRataDie 2
            in
            if Date.isBetween startDate endDate selectedDate then
                exhibition.title

            else
                ""
        )
        exhibitionList
        |> List.filter (\maybeDate -> maybeDate /= "")
        |> List.head
        |> Maybe.withDefault ""
