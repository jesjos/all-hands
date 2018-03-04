module Currencies exposing (Currency(..), allCurrencies, parseCurrency, currencyLongName)


type Currency
    = Euro
    | SwedishKrona
    | UsDollar


allCurrencies : List Currency
allCurrencies =
    [ Euro, UsDollar, SwedishKrona ]


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


currencyLongName : Currency -> String
currencyLongName currency =
    case currency of
        Euro ->
            "Euro (€)"

        UsDollar ->
            "US Dollar ($)"

        SwedishKrona ->
            "Swedish Krona (kr)"
