module Main exposing
    ( Model
    , Msg(..)
    , User(..)
    , emptyModel
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, href, placeholder, src, style, type_, value)
import Html.Events exposing (..)



---- MODEL ----


type alias UserInfo =
    { name : String
    , age : Maybe Int
    }


type User
    = Anonymous
    | Authenticated UserInfo


type alias Model =
    { user : User
    , userForm : UserForm
    }


type alias UserForm =
    { name : String
    , age : String
    }


emptyUserForm : UserForm
emptyUserForm =
    { name = ""
    , age = ""
    }


emptyModel : Model
emptyModel =
    { user = Anonymous
    , userForm = emptyUserForm
    }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



---- UPDATE ----


type AgeAction
    = Decrement
    | Increment


type Msg
    = SetAge String
    | SetAgeOnUser AgeAction
    | SetName String
    | SetUser


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noChange =
            ( model, Cmd.none )
    in
    case msg of
        SetAge age ->
            let
                userForm =
                    model.userForm

                userFormUpdated =
                    { userForm | age = age }
            in
            ( { model | userForm = userFormUpdated }
            , Cmd.none
            )

        SetAgeOnUser ageAction ->
            case model.user of
                Anonymous ->
                    noChange

                Authenticated user ->
                    case user.age of
                        Nothing ->
                            noChange

                        Just age ->
                            let
                                ageUpdated =
                                    case ageAction of
                                        Decrement ->
                                            if age <= 1 then
                                                1

                                            else
                                                age - 1

                                        Increment ->
                                            age + 1

                                userUpdated =
                                    { user | age = Just ageUpdated }
                            in
                            ( { model | user = Authenticated userUpdated }
                            , Cmd.none
                            )

        SetName name ->
            let
                userForm =
                    model.userForm

                nameUpdate =
                    if name == "Amitai" then
                        "Diane"

                    else
                        name

                userFormUpdated =
                    { userForm | name = nameUpdate }
            in
            ( { model | userForm = userFormUpdated }
            , Cmd.none
            )

        SetUser ->
            let
                userForm =
                    model.userForm
            in
            ( { model
                | user =
                    Authenticated
                        { name = userForm.name
                        , age = String.toInt userForm.age
                        }

                -- , userForm = emptyUserForm
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "User Checks" ]
        , viewName model
        , viewAge model
        , pre []
            [ h2 [] [ text "User" ]
            , div [] [ text <| Debug.toString model.user ]
            , h2 [] [ text "Form" ]
            , div [] [ text <| Debug.toString model.userForm ]
            ]
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
                    , value model.userForm.name
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
                    let
                        isDisabled =
                            age <= 1
                    in
                    div []
                        [ button
                            [ onClick (SetAgeOnUser Decrement)
                            , disabled isDisabled
                            ]
                            [ text "-" ]
                        , text (String.fromInt age)
                        , button [ onClick (SetAgeOnUser Increment) ] [ text "+" ]
                        ]

                Nothing ->
                    Html.form [ onSubmit SetUser ]
                        [ text "Please Enter your age"
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
