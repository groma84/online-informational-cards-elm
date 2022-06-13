module Main exposing (..)

import Browser exposing (UrlRequest)
import Html exposing (Html, a, div, h1, img, li, ol, text)
import Html.Attributes exposing (href, src)
import Json.Decode exposing (Decoder, decodeValue, field, list, map3, map4, maybe, string)
import Url
import Browser.Navigation as Nav
import Url.Parser as UP exposing ((</>))


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
type Route 
    = NotFound
    | Homepage
    | DeckDetails String

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
    , key : Nav.Key
    , route : Route
    }


parser : UP.Parser (Route -> a) a
parser =
    UP.oneOf
    [
        UP.map DeckDetails (UP.s deckPrefix </> UP.string)
    ]

init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        decks =
            decodeValue (list decodeDeck) flags
    in
    ( { decks = decks, route = Homepage, key = key }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | OnUrlRequest UrlRequest
    | OnUrlChange Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        OnUrlChange url ->
            let
                parsedRoute : Route
                parsedRoute = Maybe.withDefault NotFound (UP.parse parser url)
            in
            ( { model | route = parsedRoute }
            , Cmd.none
            )



---- VIEW ----

deckPrefix : String
deckPrefix = "deck"

view : Model -> Browser.Document Msg
view model =
    let
        deckLink : String -> String
        deckLink slug = deckPrefix ++ "/" ++ slug

        oneDeck : Deck -> Html Msg
        oneDeck d =
            li [] [ a [ href (deckLink d.slug) ] [ text d.slug ] ]

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
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
