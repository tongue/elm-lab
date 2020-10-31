module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Page.EditUser as EditUser
import Page.ListUsers as ListUsers
import Page.NewUser as NewUser
import Route exposing (Route)
import Url exposing (Url)


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | ListPage ListUsers.Model
    | EditPage EditUser.Model
    | NewPage NewUser.Model


type Msg
    = ListPageMsg ListUsers.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | EditPageMsg EditUser.Msg
    | NewPageMsg NewUser.Msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Users ->
                    let
                        ( pageModel, pageCmds ) =
                            ListUsers.init
                    in
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )

                Route.User userId ->
                    let
                        ( pageModel, pageCmd ) =
                            EditUser.init userId model.navKey
                    in
                    ( EditPage pageModel, Cmd.map EditPageMsg pageCmd )

                Route.NewUser ->
                    let
                        ( pageModel, pageCmd ) =
                            NewUser.init model.navKey
                    in
                    ( NewPage pageModel, Cmd.map NewPageMsg pageCmd )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "User app"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListUsers.view pageModel
                |> Html.map ListPageMsg

        EditPage pageModel ->
            EditUser.view pageModel
                |> Html.map EditPageMsg

        NewPage pageModel ->
            NewUser.view pageModel
                |> Html.map NewPageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListUsers.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( EditPageMsg subMsg, EditPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    EditUser.update subMsg pageModel
            in
            ( { model | page = EditPage updatedPageModel }
            , Cmd.map EditPageMsg updatedCmd
            )

        ( NewPageMsg subMsg, NewPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    NewUser.update subMsg pageModel
            in
            ( { model | page = NewPage updatedPageModel }
            , Cmd.map NewPageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )
