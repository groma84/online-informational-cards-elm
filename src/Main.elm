module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, h2, h3, h4, h5, h6, header, img, li, main_, ol, p, span, text)
import Html.Attributes exposing (class, href, id, src, type_)
import Html.Events exposing (onClick)
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


type Page
    = NotFoundPage
    | Homepage
    | DeckDetailsPage Deck


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
    | ScrollToRandomCard


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
            [ span [ class "header-icon" ] [ text "🐷" ]
            , span [ class "header-letter" ] [ text "O" ]
            , span [ class "header-text" ] [ text "nline" ]
            , span [ class "header-letter" ] [ text "In" ]
            , span [ class "header-text" ] [ text "formational" ]
            , span [ class "header-letter" ] [ text "C" ]
            , span [ class "header-text" ] [ text "ards" ]
            ]
        , main_ [ class "with-margins" ]
            [ content
            ]
        ]


oneDeckInDeckList : Deck -> Html Msg
oneDeckInDeckList d =
    let
        subtitle =
            case d.source of
                Nothing ->
                    text ""

                Just s ->
                    h5 [ class "card-subtitle", class "deck-list-card-subtitle" ] [ text s ]

        cardCountText =
            List.length d.cards |> String.fromInt
    in
    li [ class "deck-in-decklist" ]
        [ div [ class "card" ]
            [ h4 [ class "card-title", class "deck-list-card-title" ] [ text d.name ]
            , subtitle
            , div [ class "deck-list-card-content" ]
                [ p [ class "card-text" ] [ text (cardCountText ++ " cards") ]
                , a [ class "card-link", href (deckLink d.slug) ] [ text "Go to cards of deck" ]
                ]
            ]
        ]


homepage : Model -> Html Msg
homepage model =
    let
        deckList =
            case model.decks of
                Ok decks ->
                    ol [ class "deck-list" ] (List.map oneDeckInDeckList decks)

                Err e ->
                    text <| "ERROR!: " ++ Json.Decode.errorToString e
    in
    pageChrome deckList


deckDetailsPage : Deck -> Html Msg
deckDetailsPage deck =
    let
        -- TODO: go to random card of deck
        oneCard : Int -> Card -> Html Msg
        oneCard index card =
            let
                cardId =
                    deck.slug ++ "--" ++ String.fromInt index
            in
            li [ class "card-in-cardlist", id cardId ]
                [ div [ class "card" ]
                    [ h4 [ class "card-title", class "deck-list-card-title" ] [ text (Maybe.withDefault "" card.title) ]
                    , div [ class "deck-list-card-content" ]
                        [ p [ class "card-text" ] [ text card.text ]
                        , a [ class "card-link", href (cardLink deck.slug index) ] [ text "Go to card" ]
                        ]
                    ]
                ]
    in
    pageChrome
        (div []
            [ div [ class "flex", class "cards-page--title-container" ]
                [ h3 [ class "deck-title-on-cards-page" ] [ text deck.name ]
                , button [ type_ "button", onClick ScrollToRandomCard ] [ text "Scroll to random card" ]
                ]
            , ol [ class "deck-list" ] (List.indexedMap oneCard deck.cards)
            ]
        )


view : Model -> Browser.Document Msg
view model =
    { title = pageTitle model.page
    , body =
        [ case model.page of
            NotFoundPage ->
                -- TODO: Add Link to homepage and some styling/image
                text "page not found"

            Homepage ->
                homepage model

            DeckDetailsPage deck ->
                deckDetailsPage deck

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
