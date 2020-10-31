module Page.NewUser exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page.EditUser exposing (Msg)
import Route
import User exposing (User, UserId, emptyUser, newUserEncoder, userDecoder)


type alias Model =
    { navKey : Nav.Key
    , user : User
    , createError : Maybe String
    }


type Msg
    = StoreFirstName String
    | StoreLastName String
    | CreateUser
    | UserCreated (Result Http.Error User)


init : Nav.Key -> ( Model, Cmd Msg )
init navKey =
    ( initialModel navKey, Cmd.none )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , user = emptyUser
    , createError = Nothing
    }


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Create New User" ]
        , newUserForm
        , viewError model.createError
        ]


newUserForm : Html Msg
newUserForm =
    Html.form []
        [ div []
            [ label []
                [ text "First name"
                , br [] []
                , input [ type_ "text", onInput StoreFirstName ] []
                ]
            ]
        , div []
            [ label []
                [ text "Last name"
                , br [] []
                , input [ type_ "text", onInput StoreLastName ] []
                ]
            ]
        , div []
            [ button [ type_ "button", onClick CreateUser ]
                [ text "Save" ]
            ]
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't create a user at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreFirstName firstName ->
            let
                oldUser =
                    model.user

                updateFirstName =
                    { oldUser | firstName = firstName }
            in
            ( { model | user = updateFirstName }, Cmd.none )

        StoreLastName lastName ->
            let
                oldUser =
                    model.user

                updateLastName =
                    { oldUser | lastName = lastName }
            in
            ( { model | user = updateLastName }, Cmd.none )

        CreateUser ->
            ( model, createUser model.user )

        UserCreated (Ok user) ->
            ( { model | user = user, createError = Nothing }
            , Route.pushUrl Route.Users model.navKey
            )

        UserCreated (Err error) ->
            ( { model | createError = Just (buildErrorMessage error) }
            , Cmd.none
            )


createUser : User -> Cmd Msg
createUser user =
    Http.post
        { url = "http://localhost:5019/users"
        , body = Http.jsonBody (newUserEncoder user)
        , expect = Http.expectJson UserCreated userDecoder
        }
