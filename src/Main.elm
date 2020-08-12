module Main exposing
    ( Model
    , Msg(..)
    , User(..)
    , emptyModel
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, style, value)
import Html.Events exposing (..)
import Json.Decode as Json



---- MODEL ----


type alias UserInfo =
    { name : String
    , age : Maybe Int
    }


type User
    = Anonymous
    | Authenticated UserInfo


type AgeCount
    = Increment
    | Decrement


type alias Model =
    { user : User

    -- This is the name that will be in the form.
    , name : String
    , age : Maybe Int
    }


emptyModel : Model
emptyModel =
    { user = Anonymous
    , name = ""
    , age = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = SetName String
    | SetAge String
    | SetUser


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            ( { model | name = name }
            , Cmd.none
            )

        SetUser ->
            ( { model | user = Authenticated { name = model.name, age = model.age } }
            , Cmd.none
            )
        SetAge age ->
            ( { model | age = String.toInt age }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "User Checks" ]
        , viewName model
        , viewAge model
        ]


viewName : Model -> Html Msg
viewName model =
    case model.user of
        Anonymous ->
            Html.form [ onSubmit SetUser ]
                [ text "You are not Logged in, Please Enter your name"
                , input
                    [ style "margin-left" "12px"
                    , class "form-default"
                    , placeholder "name"
                    , onInput <| SetName
                    ]
                    []
                , button [ class "btn btn-success" ] [ text "Submit" ]
                ]

        Authenticated authName ->
            div [] [ text authName.name ]


viewAge : Model -> Html Msg
viewAge model =
    case model.user of
        Anonymous ->
            text ""

        Authenticated user ->
            case user.age of
                Just age ->
                    div []
                        [ button [] [ text "-" ]
                        , text (String.fromInt age)
                        , button [] [ text "+" ]
                        ]

                Nothing ->
                    Html.form [ onSubmit SetUser ]
                        [ text "Please Enter yout age"
                        , input
                            [ style "margin-left" "12px"
                            , class "form-default"
                            , placeholder "name"
                            , onInput SetAge
                            ]
                            []
                        , button [ class "btn btn-success" ] [ text "Submit" ]
                        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
