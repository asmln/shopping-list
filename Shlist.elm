port module Shlist exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode

-- MODEL

type alias Model =
  { xchangeName : String
  , xchangeUrl : String
  , xchangeRate : String
  }

init : (Model, Cmd Msg)
init =
  (Model "USDRUB" "https://query.yahooapis.com/v1/public/yql?q=select * from csv where url='http://finance.yahoo.com/d/quotes.csv?e=.csv&f=nl1d1t1&s=${xName}=X';&format=json&callback=" "1", Cmd.none)

-- UPDATE

type Msg
  = ShowXchRate
  | XchRate (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowXchRate ->
      (model, getXchangeRate model.xchangeName model.xchangeUrl)

    XchRate (Ok xRate) ->
      (Model model.xchangeName model.xchangeUrl xRate, Cmd.none)

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

decodeX : Decode.Decoder String
decodeX =
  Decode.at ["query", "results", "row", "col1"] Decode.string

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.xchangeName]
    , button [ onClick ShowXchRate ] [ text "Test!" ]
    ]
