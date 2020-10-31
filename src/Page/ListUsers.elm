module Page.ListUsers exposing (Model, Msg, init, update, view)

import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import User exposing (User, UserId, usersDecoder)


type alias Model =
    { users : WebData (List User)
    , deleteError : Maybe String
    }


type Msg
    = FetchUsers
    | UsersRecieved (WebData (List User))
    | DeleteUser UserId
    | UserDeleted (Result Http.Error String)


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchUsers )


initialModel : Model
initialModel =
    { users = RemoteData.Loading
    , deleteError = Nothing
    }


fetchUsers : Cmd Msg
fetchUsers =
    Http.get
        { url = "http://localhost:5019/users/"
        , expect =
            usersDecoder
                |> Http.expectJson (RemoteData.fromResult >> UsersRecieved)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchUsers ->
            ( { model | users = RemoteData.Loading }, fetchUsers )

        UsersRecieved response ->
            ( { model | users = response }, Cmd.none )

        DeleteUser userId ->
            ( model, deleteUser userId )

        UserDeleted (Ok _) ->
            ( model, fetchUsers )

        UserDeleted (Err error) ->
            ( { model | deleteError = Just (buildErrorMessage error) }
            , Cmd.none
            )


deleteUser : UserId -> Cmd Msg
deleteUser userId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:5019/users/" ++ User.idToString userId
        , body = Http.emptyBody
        , expect = Http.expectString UserDeleted
        , timeout = Nothing
        , tracker = Nothing
        }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchUsers ]
            [ text "Refresh users" ]
        , p []
            [ a [ href "/users/new" ]
                [ text "Create new user" ]
            ]
        , viewUsers model.users
        , viewDeleteError model.deleteError
        ]


viewUsers : WebData (List User) -> Html Msg
viewUsers users =
    case users of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success actualUsers ->
            div []
                [ h3 [] [ text "Users" ]
                , table []
                    ([ viewTableHeader ] ++ List.map viewUser actualUsers)
                ]

        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th []
            [ text "ID" ]
        , th []
            [ text "First name" ]
        , th []
            [ text "Last name" ]
        ]


viewUser : User -> Html Msg
viewUser user =
    let
        userPath =
            "/users/" ++ User.idToString user.id
    in
    tr []
        [ td []
            [ text (User.idToString user.id) ]
        , td []
            [ text user.firstName ]
        , td []
            [ text user.lastName ]
        , td []
            [ a [ href userPath ] [ text "Edit" ] ]
        , td []
            [ button [ type_ "button", onClick (DeleteUser user.id) ]
                [ text "Delete" ]
            ]
        ]


viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch users at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewDeleteError : Maybe String -> Html msg
viewDeleteError maybeError =
    case maybeError of
        Just error ->
            div []
                [ h3 [] [ text "Couldn't delete post at this time." ]
                , text ("Error: " ++ error)
                ]

        Nothing ->
            text ""
