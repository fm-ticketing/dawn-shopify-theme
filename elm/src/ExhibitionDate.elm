module ExhibitionDate exposing (..)

import Browser
import Date
import DatePicker exposing (DateEvent(..), defaultSettings, getInitialDate)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode



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
    , productDetails : Json.Decode.Value
    }


type alias Model =
    { exhibitionList : List Exhibition
    , closedDateList : List ClosedDate
    , productDetails : ProductDetails
    , date : Maybe Date.Date
    , datePicker : DatePicker.DatePicker
    }


type alias Exhibition =
    { title : String
    , startDate : Date.Date
    , endDate : Date.Date
    }


type alias ClosedDate =
    { closedOn : List Date.Date
    }


type alias ProductDetails =
    { id : Int, variants : List ProductVariant }


type alias ProductVariant =
    { id : Int, title : String }


fmDateFormat : String
fmDateFormat =
    "d MMM yyyy"


datePickerSettings : Model -> DatePicker.Settings
datePickerSettings model =
    let
        isDisabled : Date.Date -> Date.Date -> Bool
        isDisabled today date =
            Date.compare today date
                /= LT
                || List.member date
                    (List.concat (List.map (\{ closedOn } -> closedOn) model.closedDateList))
    in
    { defaultSettings | isDisabled = isDisabled (getInitialDate model.datePicker) }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        decodedExhibitionList =
            case Json.Decode.decodeValue exhibitionListDecoder flags.exhibitionList of
                Ok goodExhibitionData ->
                    goodExhibitionData

                Err _ ->
                    [ { title = ""
                      , startDate = Date.fromRataDie 1
                      , endDate = Date.fromRataDie 1
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

        decodedProductDetails =
            case Json.Decode.decodeValue productDetailsDecoder flags.productDetails of
                Ok goodProductDetails ->
                    { id = goodProductDetails.id, variants = goodProductDetails.variants }

                Err _ ->
                    { id = 0, variants = [] }

        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { exhibitionList = decodedExhibitionList
      , closedDateList = decodedClosedDateList
      , productDetails = decodedProductDetails
      , date = Nothing
      , datePicker = datePicker
      }
    , Cmd.map ToDatePickerMsg datePickerCmd
    )


productDetailsDecoder : Json.Decode.Decoder ProductDetails
productDetailsDecoder =
    Json.Decode.map2
        ProductDetails
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "variants" (Json.Decode.list productVariantDecoder))


productVariantDecoder : Json.Decode.Decoder ProductVariant
productVariantDecoder =
    Json.Decode.map2 ProductVariant
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)


productIdDecoder : Json.Decode.Decoder Int
productIdDecoder =
    Json.Decode.field "id" Json.Decode.int


exhibitionDecoder : Json.Decode.Decoder Exhibition
exhibitionDecoder =
    Json.Decode.map3
        Exhibition
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "start_date" Json.Decode.string
            |> Json.Decode.map dateFromString
        )
        (Json.Decode.field "end_date" Json.Decode.string
            |> Json.Decode.map dateFromString
        )


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
    | ClickedResetDatePicker
    | LineItemUpdated (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToDatePickerMsg subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (datePickerSettings model) subMsg model.datePicker

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
              --, addLineItemPropertyPost { productId = 46485121106229 }
            )

        ClickedResetDatePicker ->
            ( { model | date = Nothing }, Cmd.none )

        LineItemUpdated _ ->
            ( model, Cmd.none )


type alias CartChangePost =
    { productId : Int }


addLineItemPropertyPost : CartChangePost -> Cmd Msg
addLineItemPropertyPost post =
    Http.post
        { url = "/cart/update.js"
        , body = Http.jsonBody (cartChangeEncoder post)
        , expect = Http.expectWhatever LineItemUpdated
        }


cartChangeEncoder : CartChangePost -> Json.Encode.Value
cartChangeEncoder post =
    Json.Encode.object
        [ ( "id", Json.Encode.int post.productId )
        , ( "properties", Json.Encode.object [ ( "Exhibition", Json.Encode.string "My line item" ) ] )
        ]



-- HELPERS
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "page-width" ]
        [ Html.h3 [] [ Html.text "Exhibitions" ]
        , Html.div [] [ Html.text (String.fromInt model.productDetails.id) ]
        , Html.ul []
            (List.map
                (\{ title, startDate, endDate } ->
                    Html.li []
                        [ Html.b [] [ Html.text title ]
                        , Html.text
                            (String.join " "
                                [ " from"
                                , Date.format fmDateFormat startDate
                                , "to"
                                , Date.format fmDateFormat endDate
                                ]
                            )
                        ]
                )
                model.exhibitionList
            )
        , case model.date of
            Nothing ->
                Html.div []
                    [ Html.h2 [] [ Html.text "Select visit date" ]
                    , DatePicker.view model.date (datePickerSettings model) model.datePicker
                        |> Html.map ToDatePickerMsg
                    ]

            Just date ->
                Html.div []
                    [ Html.h2 []
                        [ Html.text
                            (String.join " "
                                [ "Book visit for: "
                                , ticketDetailString model date
                                ]
                            )
                        ]
                    , Html.button [ Html.Attributes.class "button", Html.Events.onClick ClickedResetDatePicker ] [ Html.text "Choose a different date to visit" ]
                    , viewProductVariantSelector model.productDetails.variants
                    ]
        ]


viewProductVariantSelector : List ProductVariant -> Html Msg
viewProductVariantSelector productVariants =
    Html.table [ Html.Attributes.style "margin-top" "2rem" ]
        [ Html.thead []
            [ Html.th [] [ Html.text "Ticket type" ]
            , Html.th [] [ Html.text "Price per ticket" ]
            , Html.th [] [ Html.text "Quantity" ]
            ]
        , viewProductVariants productVariants
        ]


viewProductVariants : List ProductVariant -> Html Msg
viewProductVariants productVariants =
    Html.tbody []
        (List.map
            (\variant ->
                Html.tr []
                    [ Html.td [] [ Html.text variant.title ]
                    , Html.td [] [ Html.text "-TODO-" ]
                    , Html.td [] [ Html.text "-TODO-" ]
                    ]
            )
            productVariants
        )


ticketDetailString : Model -> Date.Date -> String
ticketDetailString model date =
    String.join " "
        [ maybeExhibitionTitle model
        , Date.format fmDateFormat date
        ]


maybeExhibitionTitle : Model -> String
maybeExhibitionTitle { date, exhibitionList } =
    List.map
        (\exhibition ->
            let
                selectedDate =
                    case date of
                        Just aSelectedDate ->
                            aSelectedDate

                        Nothing ->
                            Date.fromRataDie 2
            in
            if Date.isBetween exhibition.startDate exhibition.endDate selectedDate then
                exhibition.title

            else
                ""
        )
        exhibitionList
        |> List.filter (\maybeDate -> maybeDate /= "")
        |> List.head
        |> Maybe.withDefault ""
