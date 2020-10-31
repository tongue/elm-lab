module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing (..)
import User exposing (UserId)


type Route
    = NotFound
    | Users
    | User UserId
    | NewUser


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Users top
        , map Users (s "users")
        , map User (s "users" </> User.idParser)
        , map NewUser (s "users" </> s "new")
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Users ->
            "/users"

        User userId ->
            "/users/" ++ User.idToString userId

        NewUser ->
            "/users/new"
