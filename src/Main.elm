module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text, a, li, ol)
import Html.Attributes exposing (src, href)
import Json.Decode exposing (Decoder, decodeValue, field, list, map3, map4, maybe, string)



decodeCard : Decoder Card
decodeCard =
    map3
        Card
        (maybe (field "title" string))
        (field "text" string)
        (maybe (field "additionalText" string))


decodeDeck : Decoder Deck
decodeDeck =
    map4
        Deck
        (field "slug" string)
        (field "name" string)
        (maybe (field "source" string))
        (field "cards" (list decodeCard))



---- MODEL ----


type alias Card =
    { title : Maybe String
    , text : String
    , additionalText : Maybe String
    }


type alias Deck =
    { slug : String
    , name : String
    , source : Maybe String
    , cards : List Card
    }


type alias Model =
    { decks : Result Json.Decode.Error (List Deck)
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decks =
            decodeValue (list decodeDeck) flags
    in
    ( { decks = decks }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        oneDeck : Deck -> Html Msg
        oneDeck d =
            li [] [ a [ href d.slug ] [ text d.slug ] ]

        x =
            case model.decks of
                Ok decks ->
                    ol [] (List.map oneDeck decks)
                        

                Err e ->
                    text <| "ERROR!: " ++ Json.Decode.errorToString e
    in
    { title = "Online Informational Cards"
    , body =
        [ div []
            [ x
            ]
        ]
    }



---- PROGRAM ----


main : Program Json.Decode.Value Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
