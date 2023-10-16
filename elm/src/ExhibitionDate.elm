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
    , initialCart : Json.Decode.Value
    }


type alias Model =
    { exhibitionList : List Exhibition
    , closedDateList : List ClosedDate
    , productDetails : ProductDetails
    , date : Maybe Date.Date
    , datePicker : DatePicker.DatePicker
    , cartItems : List CartItem
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
    { id : Int, title : String, price : Int }


type alias CartItem =
    { lineItemKey : String, variantId : Int, quantity : Int }


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

        decodedInitialCartItems =
            case Json.Decode.decodeValue cartItemsDecoder flags.initialCart of
                Ok goodCartItems ->
                    goodCartItems

                Err _ ->
                    []

        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { exhibitionList = decodedExhibitionList
      , closedDateList = decodedClosedDateList
      , productDetails = decodedProductDetails
      , date = Nothing
      , datePicker = datePicker
      , cartItems = decodedInitialCartItems
      }
    , Cmd.map ToDatePickerMsg datePickerCmd
    )


cartItemDecoder : Json.Decode.Decoder CartItem
cartItemDecoder =
    Json.Decode.map3 CartItem
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "variant_id" Json.Decode.int)
        (Json.Decode.field "quantity" Json.Decode.int)


cartItemsDecoder : Json.Decode.Decoder (List CartItem)
cartItemsDecoder =
    Json.Decode.field "items" (Json.Decode.list cartItemDecoder)


productDetailsDecoder : Json.Decode.Decoder ProductDetails
productDetailsDecoder =
    Json.Decode.map2
        ProductDetails
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "variants" (Json.Decode.list productVariantDecoder))


productVariantDecoder : Json.Decode.Decoder ProductVariant
productVariantDecoder =
    Json.Decode.map3 ProductVariant
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "price" Json.Decode.int)


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
    | AddToCart Int


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
            )

        ClickedResetDatePicker ->
            ( { model | date = Nothing }, Cmd.none )

        LineItemUpdated _ ->
            ( model, Cmd.none )

        AddToCart variantId ->
            ( model
            , cartAddPost
                { id = variantId
                , lineItem = ticketDetailString model
                }
            )


type alias CartChangePost =
    { id : Int, lineItem : String }


cartAddPost : CartChangePost -> Cmd Msg
cartAddPost post =
    Http.post
        { url = "/cart/add.js"
        , body = Http.jsonBody (cartAddEncoder post)
        , expect = Http.expectWhatever LineItemUpdated
        }


cartAddEncoder : CartChangePost -> Json.Encode.Value
cartAddEncoder post =
    Json.Encode.object
        [ ( "id", Json.Encode.int post.id )
        , ( "properties", Json.Encode.object [ ( "Exhibition", Json.Encode.string post.lineItem ) ] )
        , ( "sections", Json.Encode.list Json.Encode.string [ "cart-icon-bubble" ] )
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
        , if model.date == Nothing then
            Html.div []
                [ Html.h2 [] [ Html.text "Select visit date" ]
                , DatePicker.view model.date (datePickerSettings model) model.datePicker
                    |> Html.map ToDatePickerMsg
                ]

          else
            Html.div []
                [ Html.h2 []
                    [ Html.text
                        (String.join " "
                            [ "Book visit for: "
                            , ticketDetailString model
                            ]
                        )
                    ]
                , Html.button [ Html.Attributes.class "button", Html.Events.onClick ClickedResetDatePicker ] [ Html.text "Choose a different date to visit" ]
                , viewProductVariantSelector model.cartItems model.productDetails.variants
                ]
        ]


viewProductVariantSelector : List CartItem -> List ProductVariant -> Html Msg
viewProductVariantSelector cartItems productVariants =
    Html.table [ Html.Attributes.style "margin-top" "2rem" ]
        [ Html.thead []
            [ Html.th [] [ Html.text "Ticket type" ]
            , Html.th [] [ Html.text "Price per ticket" ]
            , Html.th [] [ Html.text "Quantity" ]
            ]
        , viewProductVariants cartItems productVariants
        ]


viewProductVariants : List CartItem -> List ProductVariant -> Html Msg
viewProductVariants cartItems productVariants =
    Html.tbody []
        (List.map
            (\variant ->
                Html.tr []
                    [ Html.td [] [ Html.text variant.title ]
                    , Html.td [] [ Html.text (viewPrice variant.price) ]
                    , Html.td [] [ viewQuantity cartItems variant.id ]
                    ]
            )
            productVariants
        )


viewPrice : Int -> String
viewPrice priceInt =
    "£"
        ++ String.fromFloat (toFloat priceInt / 100)
        -- plus 2 to account for the £ and .
        |> String.padRight (String.length (String.fromInt priceInt) + 2) '0'


viewQuantity : List CartItem -> Int -> Html Msg
viewQuantity cartItems variantId =
    Html.div [ Html.Attributes.class "quantity" ]
        [ Html.button
            [ Html.Attributes.class "quantity__button"
            , Html.Events.onClick (AddToCart variantId)
            ]
            [ Html.text "-" ]
        , Html.input [ Html.Attributes.class "quantity__input", Html.Attributes.value (String.fromInt (quantityFromVariantId cartItems variantId)) ] []
        , Html.button
            [ Html.Attributes.class "quantity__button"
            , Html.Events.onClick (AddToCart variantId)
            ]
            [ Html.text "+" ]
        ]


quantityFromVariantId : List CartItem -> Int -> Int
quantityFromVariantId cartItems variantId =
    --todo
    List.filter (\item -> item.variantId == variantId) cartItems
        |> List.map (\itemWithVariantId -> itemWithVariantId.quantity)
        |> List.sum


ticketDetailString : Model -> String
ticketDetailString model =
    case model.date of
        Nothing ->
            ""

        Just aDate ->
            String.join " "
                [ maybeExhibitionTitle model
                , Date.format fmDateFormat aDate
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
