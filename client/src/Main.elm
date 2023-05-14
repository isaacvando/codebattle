module Main exposing (..)

-- import Html exposing (..)

import Browser
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


type Msg
    = NoOp
    | GotString (Result Http.Error String)
    | TypedRoomCode String


type alias Model =
    { text : String
    , key : Browser.Navigation.Key
    , roomCode : String
    }



-- INIT


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { text = ""
      , key = key
      , roomCode = ""
      }
    , Http.get
        { url = "http://localhost:8080/user"
        , expect = Http.expectString GotString
        }
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotString (Ok s) ->
            ( { model | text = s }, Cmd.none )

        TypedRoomCode s ->
            if String.length s < 4 then
                ( { model | roomCode = String.toUpper s }, Cmd.none )

            else if String.length s == 4 then
                ( { model | roomCode = String.toUpper s }, Cmd.none )

            else
                update NoOp model

        x ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Test App"
    , body = [ layout [] (viewBody model) ]
    }


viewBody model =
    column [ centerX, spacing 50 ]
        [ paragraph [ Font.size 50 ] [ text "⚔️code battle⚔️" ]
        , Input.text []
            { onChange = TypedRoomCode
            , placeholder = Nothing
            , text = model.roomCode
            , label = Input.labelAbove [] (text "Room Code")
            }
        ]



-- HELPER


blankUrl : Url.Url
blankUrl =
    { protocol = Url.Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }
