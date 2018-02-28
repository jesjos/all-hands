module AllHands exposing (main)

import Html exposing (..)
import Html.Attributes exposing (for, class, classList, type_, id, selected, value)
import Html.Events exposing (onInput, onClick, onSubmit)
import Time exposing (Time)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (frenchLocale)


type alias Seconds =
    Int


type Currency
    = Euro
    | SwedishKrona
    | UsDollar


type alias Model =
    { attendees : Int
    , hourlyRate : Float
    , duration : Seconds
    , description : String
    , started : Bool
    , viewState : ViewState
    , currency : Currency
    }


type Msg
    = DescriptionChanged String
    | AttendeesChanged String
    | HourlyRateChanged String
    | NewTime Time
    | StartMeeting
    | EndMeeting
    | CurrencyChanged String
    | Noop


type ViewState
    = NewMeeting
    | ShowMeeting


main : Program Never Model Msg
main =
    Html.program
        { update = update
        , view = view
        , init = init
        , subscriptions = subscriptions
        }


hours : Int -> Int
hours numberOfHours =
    3600 * numberOfHours


asHours : Seconds -> Float
asHours seconds =
    (toFloat seconds) / 3600


init : ( Model, Cmd Msg )
init =
    { attendees = 10
    , hourlyRate = 100
    , duration = 0
    , description = ""
    , started = False
    , viewState = NewMeeting
    , currency = Euro
    }
        ! []


allCurrencies =
    [ Euro, UsDollar, SwedishKrona ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second NewTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DescriptionChanged newDescription ->
            { model | description = newDescription } ! []

        HourlyRateChanged newRate ->
            { model | hourlyRate = parseFloat newRate } ! []

        AttendeesChanged newAttendees ->
            { model | attendees = parseInt newAttendees } ! []

        NewTime _ ->
            if model.started then
                { model | duration = model.duration + 1 } ! []
            else
                model ! []

        StartMeeting ->
            { model | started = True, viewState = ShowMeeting } ! []

        EndMeeting ->
            { model | started = False } ! []

        CurrencyChanged name ->
            { model | currency = Maybe.withDefault model.currency (parseCurrency name) } ! []

        Noop ->
            model ! []


parseCurrency : String -> Maybe Currency
parseCurrency string =
    case (string |> String.trim |> String.toLower) of
        "euro" ->
            Just Euro

        "usdollar" ->
            Just UsDollar

        "swedishkrona" ->
            Just SwedishKrona

        _ ->
            Nothing


parseFloat : String -> Float
parseFloat string =
    String.toFloat string |> Result.withDefault 0


parseInt : String -> Int
parseInt string =
    String.toInt string |> Result.withDefault 0


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "text-white" ] [ text "All Hands" ]
        , h3 [ class "text-white" ] [ text "Enter some facts about your meeting and click start" ]
        , section [] ((viewForViewState model) ++ [ startStopButton model.started ])
        ]


viewForViewState : Model -> List (Html Msg)
viewForViewState model =
    case model.viewState of
        NewMeeting ->
            newMeeting model

        ShowMeeting ->
            showMeeting model


newMeeting : Model -> List (Html Msg)
newMeeting model =
    [ renderForm model ]


showMeeting : Model -> List (Html Msg)
showMeeting model =
    [ renderMeeting model ]


renderMeeting : Model -> Html msg
renderMeeting model =
    div []
        [ div [ class "row" ]
            [ makeSmallCard <| ("Attendees: " ++ (toString model.attendees) |> text)
            , makeSmallCard <| ("Hourly rate: " ++ (amountInCurrency model.currency model.hourlyRate) |> text)
            , makeSmallCard <| ("Description: " ++ model.description |> text)
            ]
        , div [ class "row second-row" ]
            [ makeBigCard <| ("Duration: " ++ (model.duration |> secondsToStopwatch) |> text)
            , makeBigCard <| ("Cost: " ++ (cost model |> amountInCurrency model.currency) |> text)
            ]
        ]

makeSmallCard = makeCard "col-sm-4"
makeBigCard = makeCard "col-sm-6"

makeCard klass text =
    div [class klass] [ div [ class "card" ] [ div [ class "card-body" ] [ text ] ] ]


defaultCurrencyFormat currency number =
    identity


cost : Model -> Float
cost model =
    (toFloat model.attendees) * model.hourlyRate * (asHours model.duration)


optionForCurrency : Currency -> Currency -> Html Msg
optionForCurrency selectedCurrency currency =
    option [ selected (selectedCurrency == currency), value (toString currency) ] [ currency |> currencyLongName |> text ]


amountInCurrency : Currency -> Float -> String
amountInCurrency currency amount =
    case currency of
        Euro ->
            defaultFormat amount ++ "€"

        UsDollar ->
            "$" ++ defaultFormat amount

        SwedishKrona ->
            defaultFormat amount ++ " kr"


currencyLongName : Currency -> String
currencyLongName currency =
    case currency of
        Euro ->
            "Euro (€)"

        UsDollar ->
            "US Dollar ($)"

        SwedishKrona ->
            "Swedish Krona (kr)"


renderForm : Model -> Html Msg
renderForm model =
    form [ onSubmit StartMeeting ]
        [ div [ class "row" ]
            [ div [ class "col-sm-3 cell" ]
                [ label [ for "currencies" ] [ text "Currency" ]
                , select [ id "currencies", class "form-control", onInput CurrencyChanged ] (List.map (optionForCurrency model.currency) allCurrencies)
                ]
            , div [ class "col-sm-3 cell" ]
                [ label [ for "attendees" ] [ text "Attendees" ]
                , input [ id "attendees", class "form-control", type_ "number", onInput AttendeesChanged ] []
                ]
            , div [ class "col-sm-3 cell" ]
                [ label [ for "hourly-rate" ] [ text "Hourly rate" ]
                , input [ id "hourly-rate", class "form-control", type_ "number", onInput HourlyRateChanged ] []
                ]
            , div [ class "col-sm-3 cell" ]
                [ label [ for "description" ] [ text "Description" ]
                , input [ id "description", class "form-control", onInput DescriptionChanged ] []
                ]
            ]
        ]


startStopButton : Bool -> Html Msg
startStopButton started =
    div [ class "row meeting-controls" ]
        [ div [ class "col-sm text-center" ]
            [ button
                [ onClick <| startStopTagger started
                , class "btn btn-xl rounded-pill mt-5"
                , classList [ ( "btn-primary", not started ), ( "btn-danger", started ) ]
                ]
                [ text <| buttonLabel started ]
            ]
        ]


startStopTagger : Bool -> Msg
startStopTagger started =
    case started of
        True ->
            EndMeeting

        False ->
            StartMeeting


buttonLabel : Bool -> String
buttonLabel started =
    if started then
        "End meeting"
    else
        "Start meeting"


secondsToStopwatch : Seconds -> String
secondsToStopwatch seconds =
    String.join ":" [ (paddedHours seconds), (paddedMinutes seconds), (paddedSeconds seconds) ]


paddedHours : Seconds -> String
paddedHours seconds =
    padWithZero <| toString <| (seconds // 3600) % 60


paddedMinutes : Seconds -> String
paddedMinutes seconds =
    padWithZero <| toString <| (seconds // 60) % 60


paddedSeconds : Seconds -> String
paddedSeconds seconds =
    padWithZero <| toString <| seconds % 60


padWithZero : String -> String
padWithZero =
    String.padLeft 2 '0'


defaultFormat =
    format { frenchLocale | decimals = 2 }
