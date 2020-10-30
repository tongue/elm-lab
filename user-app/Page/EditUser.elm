module Page.EditUser exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import RemoteData exposing (WebData)
import Route
import User exposing (User, UserId, userDecoder, userEncoder)


type alias Model =
    { navKey : Nav.Key
    , user : WebData User
    , saveError : Maybe String
    }


type Msg
    = UserRecieved (WebData User)
    | UpdateFirstName String
    | UpdateLastName String
    | SaveUser
    | UserSaved (Result Http.Error User)


init : UserId -> Nav.Key -> ( Model, Cmd Msg )
init userId navKey =
    ( initialModel navKey, fetchUser userId )


initialModel : Nav.Key -> Model
initialModel navKey =
    { navKey = navKey
    , user = RemoteData.Loading
    , saveError = Nothing
    }


fetchUser : UserId -> Cmd Msg
fetchUser userId =
    Http.get
        { url = "http://localhost:5019/users" ++ User.idToString userId
        , expect =
            userDecoder
                |> Http.expectJson (RemoteData.fromResult >> UserRecieved)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserRecieved user ->
            ( { model | user = user }, Cmd.none )

        UpdateFirstName newFirstName ->
            let
                updateFirstName =
                    RemoteData.map
                        (\userData ->
                            { userData | firstName = newFirstName }
                        )
                        model.user
            in
            ( { model | user = updateFirstName }, Cmd.none )

        UpdateLastName newLastName ->
            let
                updateLastName =
                    RemoteData.map
                        (\userData ->
                            { userData | lastName = newLastName }
                        )
                        model.user
            in
            ( { model | user = updateLastName }, Cmd.none )

        SaveUser ->
            ( model, saveUser model.user )

        UserSaved (Ok userData) ->
            let
                user =
                    RemoteData.succeed userData
            in
            ( { model | user = user, saveError = Nothing }
            , Route.pushUrl Route.Users model.navKey
            )

        UserSaved (Err error) ->
            ( { model | saveError = Just (buildErrorMessage error) }, Cmd.none )


saveUser : WebData User -> Cmd Msg
saveUser user =
    case user of
        RemoteData.Success userData ->
            let
                userUrl =
                    "http://localhost:5019/posts/"
                        ++ User.idToString userData.id
            in
            Http.request
                { method = "PATCH"
                , headers = []
                , url = userUrl
                , body = Http.jsonBody (userEncoder userData)
                , expect = Http.expectJson UserSaved userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit user" ]
        , viewUser model.user
        , viewSaveError model.saveError
        ]


viewUser : WebData User -> Html Msg
viewUser user =
    case user of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success userData ->
            editForm userData

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


editForm : User -> Html Msg
editForm user =
    Html.form []
        [ div []
            [ label []
                [ text "Firstname: " ]
            , input
                [ type_ "text"
                , value user.firstName
                , onInput UpdateFirstName
                ]
                []
            ]
        , div []
            [ label []
                [ text "Lastname: " ]
            , input
                [ type_ "text"
                , value user.lastName
                , onInput UpdateLastName
                ]
                []
            ]
        , div []
            [ button [ type_ "button", onClick SaveUser ]
                [ text "Submit" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch user at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewSaveError : Maybe String -> Html Msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't save User at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
