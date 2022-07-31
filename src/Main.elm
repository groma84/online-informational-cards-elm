module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, div, h1, header, img, li, main_, ol, span, text)
import Html.Attributes exposing (class, href, src)
import Json.Decode exposing (Decoder, decodeValue, field, list, map3, map4, maybe, string)
import List.Extra
import Url
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
    | Home
    | DeckDetails String
    | CardDetails String Int


type Page
    = NotFoundPage
    | Homepage
    | DeckDetailsPage Deck
    | CardDetailsPage Card


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
    , page : Page
    }


parser : UP.Parser (Route -> a) a
parser =
    UP.oneOf
        [ UP.map DeckDetails (UP.s deckPrefix </> UP.string)
        , UP.map CardDetails (UP.s deckPrefix </> UP.string </> UP.s cardPrefix </> UP.int)
        , UP.map Home UP.top
        ]


init : Json.Decode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        decks =
            decodeValue (list decodeDeck) flags
    in
    ( { decks = decks, page = urlToPage decks url, key = key }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | OnUrlRequest UrlRequest
    | OnUrlChange Url.Url


urlToPage : Result Json.Decode.Error (List Deck) -> Url.Url -> Page
urlToPage decks url =
    let
        findDeck : String -> Maybe Deck
        findDeck slug =
            case decks of
                Err _ ->
                    Nothing

                Ok deckList ->
                    List.Extra.find (\d -> d.slug == slug) deckList

        parsedRoute : Route
        parsedRoute =
            Maybe.withDefault NotFound (UP.parse parser url)

        page =
            case parsedRoute of
                NotFound ->
                    NotFoundPage

                Home ->
                    Homepage

                DeckDetails slug ->
                    Result.withDefault NotFoundPage
                        (Result.map
                            (\ds ->
                                findDeck slug
                                    |> Maybe.map DeckDetailsPage
                                    |> Maybe.withDefault NotFoundPage
                            )
                            decks
                        )

                CardDetails deckId cardNumber ->
                    let
                        findCard : Deck -> Maybe Card
                        findCard deck =
                            List.Extra.getAt cardNumber deck.cards

                        foundDeck =
                            findDeck deckId

                        foundCard =
                            Maybe.andThen findCard foundDeck
                    in
                    Maybe.map CardDetailsPage foundCard
                        |> Maybe.withDefault NotFoundPage
    in
    page


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
            ( { model | page = urlToPage model.decks url }
            , Cmd.none
            )



---- VIEW ----


pageTitle : Page -> String
pageTitle page =
    let
        title =
            case page of
                NotFoundPage ->
                    "404 - Not Found"

                Homepage ->
                    "Homepage"

                DeckDetailsPage deck ->
                    deck.name

                CardDetailsPage card ->
                    Maybe.withDefault "" card.title
    in
    title ++ " @ OInC"


deckPrefix : String
deckPrefix =
    "deck"


cardPrefix : String
cardPrefix =
    "card"


deckLink : String -> String
deckLink slug =
    deckPrefix ++ "/" ++ slug


cardLink deckSlug index =
    deckSlug ++ "/" ++ cardPrefix ++ "/" ++ String.fromInt index

pageChrome : Html Msg -> Html Msg
pageChrome content =
    div
        [ class "container"
        , class "container-md"
        , class "paper"
        ]
        [ header []
            [ span [] [ text "🐷" ]
            , span [] [ text "Online Informational Cards" ]
            ]
        , main_ []
            [ content
            ]
        ]


homepage : Model -> Html Msg
homepage model =
    let
        oneDeck : Deck -> Html Msg
        oneDeck d =
            li [] [ a [ href (deckLink d.slug) ] [ text d.slug ] ]

        deckList =
            case model.decks of
                Ok decks ->
                    ol [] (List.map oneDeck decks)

                Err e ->
                    text <| "ERROR!: " ++ Json.Decode.errorToString e
    in
        pageChrome deckList
    


deckDetailsPage : Deck -> Html Msg
deckDetailsPage deck =
    let
        oneCard : Int -> Card -> Html Msg
        -- TODO: shorten card.text for displaying if card.title is not available
        oneCard index card =
            li [] [ a [ href (cardLink deck.slug index) ] [ text (Maybe.withDefault card.text card.title) ] ]
    in
    div [] [ ol [] (List.indexedMap oneCard deck.cards) ]


cardDetailsPage : Card -> Html Msg
cardDetailsPage card =
    -- TODO: everything
    div [] [ text card.text ]


view : Model -> Browser.Document Msg
view model =
    { title = pageTitle model.page
    , body =
        [ case model.page of
            NotFoundPage ->
                text "page not found"

            Homepage ->
                homepage model

            DeckDetailsPage deck ->
                deckDetailsPage deck

            CardDetailsPage card ->
                cardDetailsPage card
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
