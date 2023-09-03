module ExhibitionDate exposing (..)

import Browser
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
    }


type alias Exhibition =
    { title : String
    , startDate : String
    , endDate : String
    }


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
    in
    ( { exhibitionList = decodedExhibitionList
      , selectedExhibition = "My Exhibition"
      }
    , Cmd.none
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
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
        ]
