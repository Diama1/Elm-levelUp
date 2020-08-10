module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href, src, class, style, placeholder, value)
import Html.Events exposing (..)
import Json.Decode as Json


---- MODEL ----

type alias UserInfo = 
    { name : String
    , age: Int
    }

type User = 
    Anonymous
    | Authenticated UserInfo

type AgeCount = 
    Increment | Decrement

type alias Model =
    User

emptyModel : Model
emptyModel = 
   Anonymous

init : ( Model, Cmd Msg )
init =
    ( emptyModel , Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "User Checks" ]
        , viewName model
        , viewAge model
        ]

viewName: Model -> Html Msg
viewName model= 
    case model of 
        Anonymous -> 
            div [] [ text "You are not Logged in, Please Enter your name", br [] [], br [] []
            , input [ style "margin-left" "12px", class "form-default", placeholder "name" ] [], button [ class "btn btn-success", onInput  ] [ text "Submit"]  ]
        Authenticated authName ->
            div [] [ text authName.name]

onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)

viewAge: Model -> Html Msg
viewAge model=
    case model of 
        Anonymous ->
            text " "

            
        Authenticated authAge -> 
            div []
                [ button [ ] [ text "-" ]
                , div [] [ text (String.fromInt authAge.age)]
                , button [ ] [ text "+" ]
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
