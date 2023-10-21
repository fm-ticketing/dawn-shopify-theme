module ExhibitionDate exposing (..)

import Browser
import Browser.Navigation
import Date
import DatePicker exposing (DateEvent(..), defaultSettings, getInitialDate)
import Dict
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
    , cartEmptyInShopify : Bool
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
    { lineItemKey : String
    , variantId : Int
    , quantity : Int
    , exhibitionDateTitle : String
    }


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
      , cartEmptyInShopify = List.length decodedInitialCartItems == 0
      }
    , Cmd.map ToDatePickerMsg datePickerCmd
    )


cartItemDecoder : Json.Decode.Decoder CartItem
cartItemDecoder =
    Json.Decode.map4 CartItem
        (Json.Decode.field "key" Json.Decode.string)
        (Json.Decode.field "variant_id" Json.Decode.int)
        (Json.Decode.field "quantity" Json.Decode.int)
        (Json.Decode.field "properties" exhibitionDateTitleDecoder
            |> Json.Decode.map (Maybe.withDefault "")
        )


cartItemsDecoder : Json.Decode.Decoder (List CartItem)
cartItemsDecoder =
    Json.Decode.field "items" (Json.Decode.list cartItemDecoder)


exhibitionDateTitleDecoder : Json.Decode.Decoder (Maybe String)
exhibitionDateTitleDecoder =
    Json.Decode.maybe (Json.Decode.at [ "Exhibition" ] Json.Decode.string)


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
    | ClickedAddVariant Int
    | ClickedRemoveVariant Int
    | InputVariantQuantity Int String
    | CartUpdated (Result Http.Error ())
    | ClickedUpdateCart


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

        ClickedAddVariant variantId ->
            ( { model | cartItems = addOneOfVariant model.cartItems variantId }
            , Cmd.none
            )

        ClickedRemoveVariant variantId ->
            ( { model | cartItems = removeOneOfVariant model.cartItems variantId }
            , Cmd.none
            )

        InputVariantQuantity variantId input ->
            ( { model | cartItems = updateVariantQuantity model.cartItems variantId input }
            , Cmd.none
            )

        CartUpdated _ ->
            -- todo parse returned cart for new cart items
            ( model, Browser.Navigation.load "/cart" )

        ClickedUpdateCart ->
            ( model
            , if model.cartEmptyInShopify then
                cartAddPost
                    (List.map
                        (\item ->
                            { id = item.variantId
                            , lineItem = ticketDetailString model
                            , quantity = item.quantity
                            }
                        )
                        model.cartItems
                    )

              else
                cartUpdatePost
                    (List.map (\item -> { id = item.variantId, quantity = item.quantity })
                        model.cartItems
                    )
            )


type alias CartAddPost =
    List { id : Int, lineItem : String, quantity : Int }


type alias CartUpdatePost =
    List { id : Int, quantity : Int }


cartAddPost : CartAddPost -> Cmd Msg
cartAddPost post =
    Http.post
        { url = "/cart/add.js"
        , body = Http.jsonBody (cartAddEncoder post)

        -- todo expect valid cart items
        , expect = Http.expectWhatever CartUpdated
        }


cartUpdatePost : CartUpdatePost -> Cmd Msg
cartUpdatePost post =
    Http.post
        { url = "/cart/update.js"
        , body = Http.jsonBody (cartUpdateEncoder post)

        -- todo expect valid cart items
        , expect = Http.expectWhatever CartUpdated
        }


cartAddEncoder : CartAddPost -> Json.Encode.Value
cartAddEncoder posts =
    Json.Encode.object
        [ ( "items"
          , Json.Encode.list
                (\post ->
                    Json.Encode.object
                        [ ( "id", Json.Encode.int post.id )
                        , ( "properties", Json.Encode.object [ ( "Exhibition", Json.Encode.string post.lineItem ) ] )
                        , ( "quantity", Json.Encode.int post.quantity )
                        , ( "sections", Json.Encode.list Json.Encode.string [ "cart-icon-bubble" ] )
                        ]
                )
                posts
          )
        ]


cartUpdateEncoder : CartUpdatePost -> Json.Encode.Value
cartUpdateEncoder posts =
    let
        updates =
            Dict.fromList (List.map (\post -> ( String.fromInt post.id, post.quantity )) posts)
    in
    Json.Encode.object
        [ ( "updates"
          , Json.Encode.dict identity Json.Encode.int updates
          )
        ]


variantsInCart : List CartItem -> List Int
variantsInCart cartItems =
    List.map (\item -> item.variantId) cartItems


addOneOfVariant : List CartItem -> Int -> List CartItem
addOneOfVariant initialCartItems variantId =
    if not (List.member variantId (variantsInCart initialCartItems)) then
        initialCartItems
            ++ [ { lineItemKey = ""
                 , variantId = variantId
                 , quantity = 1
                 , exhibitionDateTitle = "TODO get title from Model"
                 }
               ]

    else
        initialCartItems
            |> List.map
                (\item ->
                    if item.variantId == variantId then
                        { item | quantity = item.quantity + 1 }

                    else
                        item
                )


removeOneOfVariant : List CartItem -> Int -> List CartItem
removeOneOfVariant initialCartItems variantId =
    initialCartItems
        |> List.map
            (\item ->
                if item.variantId == variantId then
                    { item
                        | quantity =
                            if item.quantity > 1 then
                                item.quantity - 1

                            else
                                0
                    }

                else
                    item
            )


updateVariantQuantity : List CartItem -> Int -> String -> List CartItem
updateVariantQuantity initialCartItems variantId input =
    if not (List.member variantId (variantsInCart initialCartItems)) then
        initialCartItems
            ++ [ { lineItemKey = ""
                 , variantId = variantId
                 , quantity = 1
                 , exhibitionDateTitle = "TODO get title from Model"
                 }
               ]

    else
        initialCartItems
            |> List.map
                (\item ->
                    if item.variantId == variantId then
                        { item | quantity = Maybe.withDefault 0 (String.toInt input) }

                    else
                        item
                )



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
                    [ Html.text (ticketDetailString model)
                    ]
                , Html.button [ Html.Attributes.class "button button--secondary", Html.Events.onClick ClickedResetDatePicker ] [ Html.text "Choose another date" ]
                , viewProductVariantSelector model.cartItems model.productDetails.variants
                ]
        ]


viewProductVariantSelector : List CartItem -> List ProductVariant -> Html Msg
viewProductVariantSelector cartItems productVariants =
    Html.div []
        [ Html.table [ Html.Attributes.style "margin" "2rem 0" ]
            [ Html.thead []
                [ Html.th [] [ Html.text "Ticket type" ]
                , Html.th [] [ Html.text "Price per ticket" ]
                , Html.th [] [ Html.text "Quantity" ]
                ]
            , viewProductVariants cartItems productVariants
            ]
        , Html.button
            [ Html.Attributes.class "button"
            , Html.Events.onClick ClickedUpdateCart
            ]
            [ Html.text "Update basket" ]
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
    if priceInt == 0 then
        "Free"

    else
        "£"
            ++ String.fromFloat (toFloat priceInt / 100)
            -- plus 2 to account for the £ and .
            |> String.padRight (String.length (String.fromInt priceInt) + 2) '0'


viewQuantity : List CartItem -> Int -> Html Msg
viewQuantity cartItems variantId =
    let
        quantity =
            quantityFromVariantId cartItems variantId
    in
    Html.div [ Html.Attributes.class "quantity" ]
        [ Html.button
            [ Html.Attributes.class "quantity__button"
            , Html.Attributes.disabled (quantity < 1)
            , Html.Events.onClick (ClickedRemoveVariant variantId)
            ]
            [ Html.text "-" ]
        , Html.input
            [ Html.Attributes.class "quantity__input"
            , Html.Attributes.value (String.fromInt quantity)
            , Html.Attributes.type_ "number"
            , Html.Events.onInput (InputVariantQuantity variantId)
            ]
            []
        , Html.button
            [ Html.Attributes.class "quantity__button"
            , Html.Events.onClick (ClickedAddVariant variantId)
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
