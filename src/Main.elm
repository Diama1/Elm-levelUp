module Main exposing
    ( Model
    , Msg(..)
    , User(..)
    , emptyModel
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, src, style, value, disabled)
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
    , userForm : UserForm

    
    }

type alias UserForm =
    { name : String
    , age : String
    }

emptyForm : UserForm
emptyForm = 
    { name = " "
    , age = " "
    }

emptyModel : Model
emptyModel =
    { user = Anonymous
    , userForm = emptyForm
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type Msg
    = SetName String
    | SetAge String
    | SetUser
    | SetAgeCount AgeCount
    | Logout 


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noChange =
            (model, Cmd.none)
    in
    case msg of

        SetAge age ->
            let 
                userForm = 
                    model.userForm

                ageInputUpdated =
                    { userForm | age = age}
            in
            ({ model | userForm = ageInputUpdated }, Cmd.none)

        SetName name ->
            let 
                userForm =
                    model.userForm
                
                userFormUpdate = 
                    { userForm | name = name }
                
            in
            ( { model | userForm = userFormUpdate }
            , Cmd.none
            )

        SetUser ->
            let
                userForm =
                    model.userForm
            in
            ( { model | user = Authenticated { name = userForm.name, age = String.toInt userForm.age } }
            , Cmd.none
            )
        SetAgeCount ageCount ->
            case model.user of
                Anonymous -> 
                    noChange
                
                Authenticated user -> 
                    case user.age of
                        Nothing ->
                            noChange

                        Just age ->
                            let
                                newAge =
                                    case ageCount of
                                        Increment ->
                                            age + 1
                                        Decrement ->
                                            if age <= 0 then
                                                0
                                            else
                                                age - 1
                                
                                userAgeUpdated =
                                    { user | age = Just newAge }

                            in
                            ({ model | user = Authenticated userAgeUpdated }, Cmd.none)
        
        Logout ->
            ({ model | user = Anonymous }, Cmd.none)


        



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "User Checks" ]
        , div [] [ text (Debug.toString model.user )]
        , viewName model
        , viewAge model
        , viewLogout model
        
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

viewLogout : Model -> Html Msg
viewLogout model =
    case model.user of
        Anonymous ->
            button [  disabled True ] [ text "Logout"]

        Authenticated user -> 
            button [ onClick Logout ] [ text " Logout "]

viewAge : Model -> Html Msg
viewAge model =
    case model.user of
        Anonymous ->
            text ""

        Authenticated user ->
            case user.age of
                Just age ->
                    let 
                        btnDisable =
                            age <= 0
                    in
                    div []
                        [ button [ onClick (SetAgeCount Decrement)
                        , disabled btnDisable ]
                        [ text "-" ]
                        , text (String.fromInt age)
                        , button [ onClick (SetAgeCount Increment) ] [ text "+" ]
                        ]

                Nothing ->
                    Html.form [ onSubmit SetUser ]
                        [ text "Please Enter yout age"
                        , input
                            [ style "margin-left" "12px"
                            , class "form-default"
                            , placeholder "Add Age"
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
