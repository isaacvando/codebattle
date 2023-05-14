module Main exposing (..)

import Browser
import Browser.Navigation
import Html exposing (..)
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


type alias Model =
    { text : String
    }



-- INIT


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { text = "" }
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

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Test App"
    , body =
        [ h1 [] [ text "we are live!" ]
        , text model.text
        ]
    }



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
