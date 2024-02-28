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
    , giftAidCopy : Json.Decode.Value
    , initialCart : Json.Decode.Value
    }


type alias Model =
    { exhibitionList : List Exhibition
    , closedDateList : List ClosedDate
    , productDetails : ProductDetails
    , giftAidCopy : GiftAidCopy
    , date : Maybe Date.Date
    , datePicker : DatePicker.DatePicker
    , giftAidDeclaration : Bool
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
    { id : Int
    , variants : List ProductVariant
    , variantDescriptions : List String
    }


type alias ProductVariant =
    { id : Int, title : String, price : Int }


type alias GiftAidCopy =
    { heading : String, info : String, declaration : String }


type alias CartItem =
    { lineItemKey : String
    , variantId : Int
    , quantity : Int
    , exhibitionDateTitle : String
    }


fmDateFormat : String
fmDateFormat =
    "d MMM yyyy"


fmDateFormatWithWeekday : String
fmDateFormatWithWeekday =
    "E d MMM yyyy"


lastExhibitionEndDate : Model -> Date.Date
lastExhibitionEndDate model =
    List.map (\exhibition -> exhibition.endDate) model.exhibitionList
        |> List.sortWith Date.compare
        |> List.reverse
        |> List.head
        |> Maybe.withDefault (Date.add Date.Months 9 (getInitialDate model.datePicker))
        |> Date.add Date.Days 1


datePickerSettings : Model -> DatePicker.Settings
datePickerSettings model =
    let
        isDisabled : Date.Date -> Date.Date -> Bool
        isDisabled today date =
            Date.compare (Date.add Date.Days -1 today) date
                /= LT
                || List.member date
                    (List.concat (List.map (\{ closedOn } -> closedOn) model.closedDateList))
                || Date.compare (lastExhibitionEndDate model) date
                /= GT
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
                    { id = goodProductDetails.id
                    , variants = goodProductDetails.variants
                    , variantDescriptions = goodProductDetails.variantDescriptions
                    }

                Err error ->
                    { id = 0, variants = [], variantDescriptions = [] }

        decodedGiftAidCopy =
            case Json.Decode.decodeValue giftAidCopyDecoder flags.giftAidCopy of
                Ok goodGiftAidCopy ->
                    goodGiftAidCopy

                Err _ ->
                    { heading = "", info = "", declaration = "" }

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
      , giftAidCopy = decodedGiftAidCopy
      , date = Nothing
      , datePicker = datePicker
      , giftAidDeclaration = False
      , cartItems = decodedInitialCartItems
      , cartEmptyInShopify = List.length decodedInitialCartItems == 0
      }
    , Cmd.batch
        [ Cmd.map ToDatePickerMsg datePickerCmd
        , if List.length decodedInitialCartItems > 0 then
            cartInitialisePost

          else
            Cmd.none
        ]
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
    Json.Decode.map3
        ProductDetails
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "variants" (Json.Decode.list productVariantDecoder))
        (Json.Decode.field "variantDescriptions" (Json.Decode.list Json.Decode.string))


productVariantDecoder : Json.Decode.Decoder ProductVariant
productVariantDecoder =
    Json.Decode.map3 ProductVariant
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "price" Json.Decode.int)


productIdDecoder : Json.Decode.Decoder Int
productIdDecoder =
    Json.Decode.field "id" Json.Decode.int


giftAidCopyDecoder : Json.Decode.Decoder GiftAidCopy
giftAidCopyDecoder =
    Json.Decode.map3
        GiftAidCopy
        (Json.Decode.field "gift_aid_heading" Json.Decode.string)
        (Json.Decode.field "gift_aid_info" Json.Decode.string)
        (Json.Decode.field "gift_aid_declaration" Json.Decode.string)


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


orderExhibitionListByStartDate : List Exhibition -> List Exhibition
orderExhibitionListByStartDate exhibitionList =
    List.sortWith
        (\a b ->
            Date.compare a.startDate b.startDate
        )
        exhibitionList


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
    | ToggleGiftAidDeclaration
    | CartUpdated (Result Http.Error ())
    | CartInitialised (Result Http.Error ())
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
            let
                updatedCartItems : List CartItem
                updatedCartItems =
                    List.map
                        (\item ->
                            if item.variantId == variantId then
                                { item | quantity = item.quantity - 1 }

                            else
                                item
                        )
                        model.cartItems

                newCartContainsGiftAidTicket : Bool
                newCartContainsGiftAidTicket =
                    hasGiftAidTicket model.productDetails.variants updatedCartItems

                updatedGiftAidDeclaration : Bool
                updatedGiftAidDeclaration =
                    if not newCartContainsGiftAidTicket then
                        False

                    else
                        model.giftAidDeclaration
            in
            ( { model | cartItems = removeOneOfVariant model.cartItems variantId, giftAidDeclaration = updatedGiftAidDeclaration }
            , Cmd.none
            )

        InputVariantQuantity variantId input ->
            ( { model | cartItems = updateVariantQuantity model.cartItems variantId input }
            , Cmd.none
            )

        ToggleGiftAidDeclaration ->
            ( { model | giftAidDeclaration = not model.giftAidDeclaration }, Cmd.none )

        CartUpdated _ ->
            -- todo parse returned cart for new cart items
            ( model, Browser.Navigation.load "/cart" )

        CartInitialised _ ->
            ( { model | cartItems = [] }
            , -- Causes flash - need to figure how to update cart widget
              Browser.Navigation.reload
              -- Cmd.none
            )

        ClickedUpdateCart ->
            ( model
            , if model.cartEmptyInShopify then
                cartAddPost
                    (List.map
                        (\item ->
                            { id = item.variantId
                            , lineItem = ticketDetailString model
                            , giftAidDeclaration = model.giftAidDeclaration
                            , quantity = item.quantity
                            }
                        )
                        model.cartItems
                    )

              else
                cartUpdatePost
                    (List.map
                        (\item ->
                            { id = item.variantId
                            , quantity = item.quantity
                            }
                        )
                        model.cartItems
                    )
            )


type alias CartAddPost =
    List
        { id : Int
        , lineItem : String
        , giftAidDeclaration : Bool
        , quantity : Int
        }


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


cartInitialisePost : Cmd Msg
cartInitialisePost =
    Http.post
        { url = "/cart/clear.js"
        , body = Http.emptyBody

        -- todo expect valid cart items
        , expect = Http.expectWhatever CartInitialised
        }


cartAddEncoder : CartAddPost -> Json.Encode.Value
cartAddEncoder posts =
    Json.Encode.object
        [ ( "items"
          , Json.Encode.list
                (\post ->
                    Json.Encode.object
                        [ ( "id", Json.Encode.int post.id )
                        , ( "properties"
                          , if post.giftAidDeclaration then
                                Json.Encode.object
                                    [ ( "Exhibition", Json.Encode.string post.lineItem )
                                    , ( "GiftAidDeclaration", Json.Encode.bool post.giftAidDeclaration )
                                    ]

                            else
                                Json.Encode.object
                                    [ ( "Exhibition", Json.Encode.string post.lineItem )
                                    ]
                          )
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
                 , exhibitionDateTitle = ""
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
                 , exhibitionDateTitle = ""
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
    if not model.cartEmptyInShopify then
        Html.text ""

    else
        Html.div [ Html.Attributes.class "page-width" ]
            [ Html.ul [ Html.Attributes.class "exhibition-list" ]
                (List.map
                    (\{ title, startDate, endDate } ->
                        Html.li []
                            [ Html.h3 [] [ Html.text title ]
                            , Html.text
                                (String.join " "
                                    [ Date.format fmDateFormat startDate
                                    , "to"
                                    , Date.format fmDateFormat endDate
                                    ]
                                )
                            ]
                    )
                    (orderExhibitionListByStartDate model.exhibitionList)
                )
            , if model.date == Nothing then
                Html.div []
                    [ Html.h2 [] [ Html.text "Select visit date" ]
                    , DatePicker.view model.date (datePickerSettings model) model.datePicker
                        |> Html.map ToDatePickerMsg
                    ]

              else
                Html.div []
                    [ Html.button
                        [ Html.Attributes.class "button button--secondary"
                        , Html.Events.onClick ClickedResetDatePicker
                        ]
                        [ Html.text "Choose another date" ]
                    , Html.h2 []
                        [ Html.text (ticketDetailString model)
                        ]
                    , viewProductVariantSelector model
                    ]
            ]


viewProductVariantSelector : Model -> Html Msg
viewProductVariantSelector model =
    Html.div []
        [ Html.table [ Html.Attributes.style "margin" "2rem 0" ]
            [ Html.thead []
                [ Html.th [] [ Html.text "Ticket type" ]
                , Html.th [] [ Html.text "Price per ticket" ]
                , Html.th [] [ Html.text "Quantity" ]
                ]
            , viewProductVariants model.cartItems model.productDetails.variants model.productDetails.variantDescriptions
            ]
        , if hasGiftAidTicket model.productDetails.variants model.cartItems then
            viewGiftAidDeclaration model.giftAidCopy model.giftAidDeclaration

          else
            Html.text ""
        , Html.button
            [ Html.Attributes.class "button"
            , Html.Events.onClick ClickedUpdateCart
            ]
            [ Html.text "Update basket" ]
        ]


viewProductVariants : List CartItem -> List ProductVariant -> List String -> Html Msg
viewProductVariants cartItems productVariants productVariantDescriptions =
    Html.tbody []
        (List.map
            (\variant ->
                Html.tr []
                    [ Html.td []
                        [ Html.span [ Html.Attributes.class "ticket-title" ] [ Html.text variant.title ]
                        , Html.br [] []
                        , Html.span [ Html.Attributes.class "ticket-description" ] [ viewProductVariantDescription variant.id productVariantDescriptions ]
                        ]
                    , Html.td [] [ Html.text (viewPrice variant.price) ]
                    , Html.td [] [ viewQuantity cartItems variant.id ]
                    ]
            )
            productVariants
        )


viewProductVariantDescription : Int -> List String -> Html Msg
viewProductVariantDescription variantId productVariantDescriptions =
    let
        variantIds =
            List.map
                (\pair ->
                    String.split ":" pair
                )
                productVariantDescriptions
                |> List.map
                    (\aPair ->
                        List.take 2 aPair |> List.head |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0
                    )

        variantDescriptions =
            List.map
                (\pair ->
                    String.split ":" pair
                )
                productVariantDescriptions
                |> List.map
                    (\aPair ->
                        List.take 2 aPair |> List.reverse |> List.head |> Maybe.withDefault ""
                    )

        filteredVariantDescriptions =
            List.map2 (\id description -> { id = id, description = description }) variantIds variantDescriptions
                |> List.filter
                    (\{ id, description } ->
                        (String.length (String.replace " " "" description) > 0) && id == variantId
                    )

        filteredVariantDescription =
            Maybe.withDefault { id = 0, description = "" } (List.head filteredVariantDescriptions)
    in
    if filteredVariantDescription.id == variantId then
        Html.text filteredVariantDescription.description

    else
        Html.text ""


viewPrice : Int -> String
viewPrice priceInt =
    if priceInt == 0 then
        "£0.00"

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
    List.filter (\item -> item.variantId == variantId) cartItems
        |> List.map (\itemWithVariantId -> itemWithVariantId.quantity)
        |> List.sum


hasGiftAidTicket : List ProductVariant -> List CartItem -> Bool
hasGiftAidTicket allVariants cartItems =
    let
        giftAidIds =
            List.filter (\variant -> String.contains "gift aid" (String.toLower variant.title)) allVariants
                |> List.map (\variant -> variant.id)

        cartItemVariantIds =
            List.filter (\item -> item.quantity > 0) cartItems
                |> List.map (\item -> item.variantId)
    in
    (List.filter (\id -> List.member id giftAidIds) cartItemVariantIds
        |> List.length
    )
        > 0


viewGiftAidDeclaration : GiftAidCopy -> Bool -> Html Msg
viewGiftAidDeclaration giftAidCopy giftAidDeclaration =
    Html.div [ Html.Attributes.class "gift-aid-container" ]
        [ Html.h2 [] [ Html.text giftAidCopy.heading ]
        , Html.p [] [ Html.text giftAidCopy.info ]
        , Html.label []
            [ Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Events.onClick ToggleGiftAidDeclaration
                , Html.Attributes.checked giftAidDeclaration
                ]
                []
            , Html.text giftAidCopy.declaration
            ]
        ]


ticketDetailString : Model -> String
ticketDetailString model =
    case model.date of
        Nothing ->
            ""

        Just aDate ->
            String.join " "
                [ maybeExhibitionTitle model
                , Date.format fmDateFormatWithWeekday aDate
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
