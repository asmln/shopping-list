port module Shlist exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
  { xchangeName : String
  , xchangeUrl : String
  , xchangeRate : Float
  }

init : (Model, Cmd Msg)
init =
  (Model "USDRUB" "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20csv%20where%20url%3D%22http%3A%2F%2Ffinance.yahoo.com%2Fd%2Fquotes.csv%3Fe%3D.csv%26f%3Dc4l1%26s%3D${xName}%3DX%22%3B&format=json&diagnostics=true&callback=" 0, Cmd.none)

-- UPDATE

type Msg
  = ShowXchRate
  | XchRate (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowXchRate ->
      (model, getXchangeRate model.xchangeName model.xchangeUrl)

    XchRate (Ok xRateStr) -> 
        let n = Result.withDefault 0 (String.toFloat xRateStr) 
        in
            (Model model.xchangeName model.xchangeUrl n, Cmd.none)

    XchRate (Err _) ->
      (model, Cmd.none)

getXchangeRate : String -> String -> Cmd Msg
getXchangeRate xName xUrl =
  let
    url =
      replace "${xName}" xName xUrl

    request =
      Http.get url decodeX
  in
    Http.send XchRate request

replace : String -> String -> String -> String
replace from to str =
    String.split from str
        |> String.join to

decodeX : Decode.Decoder String
decodeX = Decode.at ["query", "results", "row", "col1"] Decode.string

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.xchangeName ]
        , h3 [] [ text (toString model.xchangeRate) ]
        , button [ onClick ShowXchRate ] [ text "Test!" ]
        ]
