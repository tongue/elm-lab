module User exposing (User, UserId, idParser, idToString, userDecoder, userEncoder, usersDecoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)


type UserId
    = UserId Int


type alias User =
    { id : UserId
    , age : Int
    , firstName : String
    , lastName : String
    , email : String
    , picture : String
    }


idToString : UserId -> String
idToString (UserId id) =
    String.fromInt id


idDecoder : Decoder UserId
idDecoder =
    Decode.map UserId int


usersDecoder : Decoder (List User)
usersDecoder =
    list userDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" idDecoder
        |> required "age" int
        |> required "firstName" string
        |> required "lastName" string
        |> required "email" string
        |> required "picture" string


idParser : Parser (UserId -> a) a
idParser =
    custom "USERID" <|
        \userId ->
            Maybe.map UserId (String.toInt userId)


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "id", encodeId user.id )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        ]


encodeId : UserId -> Encode.Value
encodeId (UserId id) =
    Encode.int id
