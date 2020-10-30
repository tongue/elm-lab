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
    }


type Msg
    = FetchUsers
    | UsersRecieved (WebData (List User))


init : ( Model, Cmd Msg )
init =
    ( { users = RemoteData.Loading }, fetchUsers )


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


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchUsers ]
            [ text "Refresh users" ]
        , viewUsers model.users
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
