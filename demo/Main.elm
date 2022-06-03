module Main exposing (main)

import Browser
import DateRangePicker as Picker
import DateRangePicker.Helpers as Helpers
import DateRangePicker.Range as Range exposing (Range)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Time
import Time.Extra


type alias Model =
    { config : Picker.Config
    , picker : Picker.State
    }


type Msg
    = PickerChanged Picker.State
    | UpdateConfig Picker.Config


init : () -> ( Model, Cmd Msg )
init _ =
    initFromConfig
        (Picker.configure
            (\default ->
                { default
                    | allowFuture = True
                    , allowedToPickDay = allowedToPickDay
                    , noRangeCaption = "Click me!"
                    , dayFormatter = dayFormatter
                    , rangeFormatter = rangeFormatter
                    , sticky = True
                    , predefinedRanges = predefinedRanges
                }
            )
        )
        Nothing


allowedToPickDay : Time.Zone -> { dayToPick : Time.Posix, begin : Maybe Time.Posix } -> Bool
allowedToPickDay zone { begin, dayToPick } =
    let
        onWeekend =
            case dayToPick |> Time.toWeekday zone of
                Time.Sat ->
                    True

                Time.Sun ->
                    True

                _ ->
                    False
    in
    case begin of
        Nothing ->
            not onWeekend

        Just beginAt ->
            not onWeekend
                && (Range.create zone beginAt (Time.Extra.setYear zone (Time.toYear zone beginAt + 1) beginAt)
                        |> Range.between dayToPick
                   )


predefinedRanges : Time.Zone -> Time.Posix -> List ( String, Range )
predefinedRanges zone today =
    let
        daysBefore n posix =
            posix |> Time.Extra.addDays -n |> Time.Extra.startOfDay zone
    in
    [ ( "Today"
      , Range.create zone (Time.Extra.startOfDay zone today) (Time.Extra.endOfDay zone today)
      )
    , ( "Yesterday"
      , Range.create zone (today |> daysBefore 1 |> Time.Extra.startOfDay zone) (today |> daysBefore 1 |> Time.Extra.endOfDay zone)
      )
    , ( "Last 7 days"
      , Range.create zone (today |> daysBefore 7) (today |> Time.Extra.startOfDay zone |> Time.Extra.addMillis -1)
      )
    , ( "Last 30 days"
      , Range.create zone (today |> daysBefore 30) (today |> Time.Extra.startOfDay zone |> Time.Extra.addMillis -1)
      )
    , ( "This month"
      , Range.create zone (today |> Time.Extra.startOfMonth zone) today
      )
    , ( "Last month"
      , Range.create zone (today |> Helpers.startOfPreviousMonth zone) (today |> Time.Extra.startOfMonth zone |> Time.Extra.addMillis -1)
      )
    ]


dayFormatter :
    Time.Zone
    ->
        { day : Time.Posix
        , today : Time.Posix
        }
    -> Html Never
dayFormatter zone { day, today } =
    let
        greaterThanToday =
            Time.Extra.compare day today

        daysBetween =
            Range.days (Range.create zone day today)

        beforeOrAfterToday =
            case ( daysBetween, greaterThanToday ) of
                ( 0, _ ) ->
                    ""

                ( _, EQ ) ->
                    ""

                ( _, GT ) ->
                    "+"

                ( _, LT ) ->
                    "-"
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "row-gap" "0.1em"
        ]
        [ div
            [ style "font-weight" "bold"
            ]
            [ day |> Time.toDay zone |> String.fromInt |> text
            ]
        , div
            [ style "color" "#888"
            , style "font-size" "0.7em"
            , style "flex-direction" "column"
            ]
            [ beforeOrAfterToday
                ++ (case daysBetween of
                        0 ->
                            daysBetween |> String.fromInt

                        _ ->
                            (daysBetween |> String.fromInt)
                                ++ "d"
                   )
                |> text
            ]
        ]


rangeFormatter : Time.Zone -> Time.Posix -> Range -> String
rangeFormatter zone today range =
    let
        begin =
            Range.beginsAt range

        end =
            Range.endsAt range
    in
    if
        Helpers.sameDay zone
            begin
            end
            && Time.Extra.toIso8601Date
                zone
                today
            == Time.Extra.toIso8601Date zone end
    then
        "today"

    else if Helpers.sameDay zone begin end then
        relativeTime zone today begin

    else
        "from "
            ++ relativeTime zone today begin
            ++ " to "
            ++ relativeTime zone today end


relativeTime : Time.Zone -> Time.Posix -> Time.Posix -> String
relativeTime zone today end =
    let
        endGreaterThanToday =
            Time.Extra.compare end today

        endDaysBetween =
            Range.days (Range.create zone end today)
    in
    case ( endDaysBetween, endGreaterThanToday ) of
        ( 0, _ ) ->
            "today"

        ( _, EQ ) ->
            "today"

        ( 1, GT ) ->
            String.fromInt endDaysBetween ++ " day from now"

        ( 1, LT ) ->
            String.fromInt endDaysBetween ++ " day ago"

        ( _, GT ) ->
            String.fromInt endDaysBetween ++ " days from now"

        ( _, LT ) ->
            String.fromInt endDaysBetween ++ " days ago"


initFromConfig : Picker.Config -> Maybe Range -> ( Model, Cmd Msg )
initFromConfig config range =
    let
        picker =
            Picker.init config range
    in
    ( { config = config, picker = picker }
    , Picker.now PickerChanged picker
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickerChanged state ->
            ( { model | picker = state }, Cmd.none )

        UpdateConfig config ->
            model.picker
                |> Picker.getRange
                |> initFromConfig config


view : Model -> Html Msg
view { config, picker } =
    let
        field children =
            p [] [ label [] children ]
    in
    div []
        [ h1 [] [ text "elm-daterange-picker" ]
        , Picker.view PickerChanged picker
        , h2 [] [ text "Live configuration" ]
        , div []
            [ case Picker.getRange picker of
                Just range ->
                    dl []
                        [ dt [] [ text "Selected: " ]
                        , dd [] [ text (Range.format Time.utc range) ]
                        , dt [] [ text "toString: " ]
                        , dd [] [ code [] [ text (Range.toString range) ] ]
                        , dt [] [ text "JSON: " ]
                        , dd [] [ pre [] [ Range.encode range |> Encode.encode 2 |> text ] ]
                        ]

                Nothing ->
                    p [] [ text "Nothing selected yet" ]
            ]
        , div []
            [ field
                [ input
                    [ type_ "checkbox"
                    , onCheck (\allow -> UpdateConfig { config | allowFuture = allow })
                    , checked config.allowFuture
                    ]
                    []
                , text " Allow future"
                ]
            , field
                [ input
                    [ type_ "checkbox"
                    , onCheck (\allow -> UpdateConfig { config | applyRangeImmediately = allow })
                    , checked config.applyRangeImmediately
                    ]
                    []
                , text " Apply predefined range immediately"
                ]
            , field
                [ text "No range caption "
                , input
                    [ type_ "text"
                    , onInput (\caption -> UpdateConfig { config | noRangeCaption = caption })
                    , value config.noRangeCaption
                    ]
                    []
                ]
            , field
                [ input
                    [ type_ "checkbox"
                    , onCheck (\sticky -> UpdateConfig { config | sticky = sticky })
                    , checked config.sticky
                    ]
                    []
                , text " Sticky (always opened)"
                ]
            , field
                [ text "Weeks start on "
                , [ Time.Sat, Time.Sun, Time.Mon ]
                    |> List.map
                        (\day ->
                            option
                                [ value (dayToString day)
                                , selected (day == config.weeksStartOn)
                                ]
                                [ text (dayToString day) ]
                        )
                    |> select [ onInput (\str -> UpdateConfig { config | weeksStartOn = dayFromString str }) ]
                ]
            ]
        ]


dayToString : Time.Weekday -> String
dayToString day =
    case day of
        Time.Sun ->
            "Sunday"

        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"


dayFromString : String -> Time.Weekday
dayFromString string =
    case string of
        "Sunday" ->
            Time.Sun

        "Monday" ->
            Time.Mon

        "Tuesday" ->
            Time.Tue

        "Wednesday" ->
            Time.Wed

        "Thursday" ->
            Time.Thu

        "Friday" ->
            Time.Fri

        _ ->
            Time.Sat


subscriptions : Model -> Sub Msg
subscriptions { picker } =
    Picker.subscriptions PickerChanged picker


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
