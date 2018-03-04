module AllHands exposing (main)

import Currencies exposing (Currency)
import Html exposing (..)
import Html.Attributes exposing (for, class, classList, type_, id, selected, value, style, placeholder)
import Html.Events exposing (onInput, onClick, onSubmit)
import Time exposing (Time)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (frenchLocale)
import Maybe.Extra


type alias Seconds =
    Int


type MeetingStatus
    = New
    | Started
    | Paused


type alias Model =
    { attendees : Int
    , hourlyRate : Float
    , duration : Seconds
    , description : String
    , status : MeetingStatus
    , viewState : ViewState
    , currency : Currency
    }


type Msg
    = DescriptionChanged String
    | AttendeesChanged String
    | HourlyRateChanged String
    | NewTime Time
    | StartMeeting
    | PauseMeeting
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
    newMeeting
        ! []


newMeeting =
    { attendees = 10
    , hourlyRate = 100
    , duration = 0
    , description = ""
    , status = New
    , viewState = NewMeeting
    , currency = Euro
    }


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
            if model.status == Started then
                { model | duration = model.duration + 1 } ! []
            else
                model ! []

        StartMeeting ->
            { model | status = Started, viewState = ShowMeeting } ! []

        PauseMeeting ->
            { model | status = Paused } ! []

        EndMeeting ->
            newMeeting ! []

        CurrencyChanged name ->
            { model | currency = Maybe.withDefault model.currency (parseCurrency name) } ! []

        Noop ->
            model ! []


parseFloat : String -> Float
parseFloat =
    parseWithZeroDefault String.toFloat


parseInt : String -> Int
parseInt =
    parseWithZeroDefault String.toInt


parseWithZeroDefault : (String -> Result err number) -> (String -> number)
parseWithZeroDefault parseFunction =
    \string -> Result.withDefault 0 (parseFunction string)


view : Model -> Html Msg
view model =
    div [ class "container" ] (model |> viewForViewState)


viewForViewState : Model -> List (Html Msg)
viewForViewState model =
    case model.viewState of
        NewMeeting ->
            renderNewMeeting model

        ShowMeeting ->
            showMeeting model


mainHeader : Html Msg
mainHeader =
    h1 [ class "text-white" ] [ text "All Hands" ]


subHeader : Html Msg
subHeader =
    h3 [ class "text-white" ] [ text "Enter some facts about your meeting and click start" ]


renderNewMeeting : Model -> List (Html Msg)
renderNewMeeting model =
    [ mainHeader, subHeader, section [] [ renderForm model, meetingControls model.status ] ]


showMeeting : Model -> List (Html Msg)
showMeeting model =
    [ mainHeader, section [] [ renderMeeting model, meetingControls model.status ] ]


renderMeeting : Model -> Html Msg
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


makeSmallCard : Html Msg -> Html Msg
makeSmallCard =
    makeCard "col-sm-4"


makeBigCard : Html Msg -> Html Msg
makeBigCard =
    makeCard "col-sm-6"


makeCard : String -> Html Msg -> Html Msg
makeCard klass content =
    div [ class klass ]
        [ div [ class "card" ]
            [ div [ class "card-body" ] [ content ]
            ]
        ]


cost : Model -> Float
cost model =
    (toFloat model.attendees) * model.hourlyRate * (asHours model.duration)


optionForCurrency : Currency -> Currency -> Html Msg
optionForCurrency selectedCurrency currency =
    option
        [ selected (selectedCurrency == currency)
        , value (toString currency)
        ]
        [ currency |> currencyLongName |> text ]


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
        [ div [ class "row text-white" ]
            [ div [ class "col-sm-3 cell" ]
                [ label [ for "currencies" ] [ text "Currency" ]
                , select
                    [ id "currencies", class "form-control", onInput CurrencyChanged ]
                    (List.map (optionForCurrency model.currency) allCurrencies)
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
                , input
                    [ id "description"
                    , class "form-control"
                    , onInput DescriptionChanged
                    , placeholder "Yet another daily..."
                    ]
                    []
                ]
            ]
        , button [ style [ ( "visibility", "hidden" ) ] ] []
        ]


meetingControls : MeetingStatus -> Html Msg
meetingControls status =
    div [ class "row meeting-controls" ]
        [ div [ class "col-sm text-center" ]
            ([ startPauseButton status ] ++ (Maybe.Extra.toList (newMeetingButton status)))
        ]


newMeetingButton : MeetingStatus -> Maybe (Html Msg)
newMeetingButton status =
    case status of
        Paused ->
            Just
                (button
                    [ onClick EndMeeting
                    , class "btn btn-xl rounded-pill mt-5"
                    ]
                    [ text "New meeting" ]
                )

        _ ->
            Nothing


startPauseButton : MeetingStatus -> Html Msg
startPauseButton status =
    let
        started =
            status == Started
    in
        button
            [ onClick <| startStopTagger started
            , class "btn btn-xl rounded-pill mt-5"
            , classList [ ( "btn-primary", not started ), ( "btn-danger", started ) ]
            ]
            [ text <| buttonLabel status ]


startStopTagger : Bool -> Msg
startStopTagger started =
    case started of
        True ->
            PauseMeeting

        False ->
            StartMeeting


buttonLabel : MeetingStatus -> String
buttonLabel status =
    case status of
        Started ->
            "Pause meeting"

        Paused ->
            "Resume meeting"

        _ ->
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


defaultFormat : Float -> String
defaultFormat =
    format { frenchLocale | decimals = 2 }
