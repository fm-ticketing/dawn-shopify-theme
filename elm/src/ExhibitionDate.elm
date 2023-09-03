module ExhibitionDate exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes



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
    {}


type alias Model =
    { selectedExhibition : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { selectedExhibition = "My Exhibition"
      }
    , Cmd.none
    )



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
    Html.div [] [ Html.text "Hello elm" ]
