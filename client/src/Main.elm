module Main exposing (..)

import Browser
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events as Events
import Http
import Json.Decode as Decode
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
    | TypedUsername String
    | TypedRoomCode String
    | JoinRoom
    | JoinRoomResponse (Result Http.Error ())


type alias Model =
    { roomCode : String
    , username : String
    , formError : String
    }


apiUrl : String
apiUrl =
    "http://localhost:8080"



-- INIT


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { roomCode = ""
      , username = ""
      , formError = ""
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypedRoomCode s ->
            if String.length s < 4 then
                ( { model | roomCode = String.toUpper s }, Cmd.none )

            else if String.length s == 4 then
                ( { model | roomCode = String.toUpper s }, Cmd.none )

            else
                update NoOp model

        TypedUsername s ->
            ( { model | username = String.toUpper s |> String.filter Char.isAlpha }, Cmd.none )

        JoinRoom ->
            let
                m =
                    { model | formError = "" }
            in
            if formIsValid model then
                ( m
                , Http.post
                    { url = apiUrl ++ "/" ++ model.username ++ "/" ++ model.roomCode
                    , body = Http.emptyBody
                    , expect = Http.expectWhatever JoinRoomResponse
                    }
                )

            else
                ( m, Cmd.none )

        JoinRoomResponse r ->
            case r of
                Err (Http.BadStatus 404) ->
                    ( { model | formError = "I couldn't find a room with the code " ++ model.roomCode }, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


formIsValid : Model -> Bool
formIsValid model =
    String.length model.roomCode == 4 && String.length model.username > 0



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Test App"
    , body = [ layout [ padding 25 ] (viewBody model) ]
    }


viewBody model =
    column
        [ centerX
        , spacing 40
        , onEnter JoinRoom
        ]
        ([ paragraph [ Font.size 50 ] [ text "⚔️Code Battle⚔️" ]
         , Input.text []
            { onChange = TypedUsername
            , placeholder = getPlaceholder "NAME"
            , text = model.username
            , label = Input.labelAbove [] (text "Username")
            }
         , Input.text []
            { onChange = TypedRoomCode
            , placeholder = getPlaceholder "CODE"
            , text = model.roomCode
            , label = Input.labelAbove [] (text "Room Code")
            }
         , Input.button
            [ Background.color (rgb255 66 135 235)
            , Border.rounded 15
            , padding 20
            , centerX
            ]
            { onPress = Just JoinRoom
            , label = paragraph [] [ text "Join Room" ]
            }
         ]
            ++ viewFormError model
        )


viewFormError : Model -> List (Element msg)
viewFormError model =
    case model.formError of
        "" ->
            []

        err ->
            [ paragraph [ Font.color (rgb255 255 160 122) ] [ text err ] ]


getPlaceholder : String -> Maybe (Input.Placeholder msg)
getPlaceholder s =
    Just (Input.placeholder [] (text s))


onEnter : msg -> Attribute msg
onEnter msg =
    htmlAttribute
        (Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )



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
