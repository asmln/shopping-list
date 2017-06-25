port module Shlist exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Decode
import Dom
import Html exposing (..)
import String
import Task

main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }

port setStorage : Model -> Cmd msg

-- This function adds the setStorage command for every step of the update function.
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )

-- MODEL

type alias Model =
    { uid : Int
    , items : List Item
    , itemName : String
    , xchangeName : String
    , xchangeUrl : String
    , xchangeRate : Float
    }
    
type alias Item =
    { id : Int
    , name : String
    , price : Float
    , editing : Bool
    }

originalModel : Model
originalModel =
    { uid = 0
    , items = []
    , itemName = ""
    , xchangeName = "RUBUSD"
    , xchangeUrl = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20csv%20where%20url%3D%22http%3A%2F%2Ffinance.yahoo.com%2Fd%2Fquotes.csv%3Fe%3D.csv%26f%3Dc4l1%26s%3D${xName}%3DX%22%3B&format=json&diagnostics=true&callback="
    , xchangeRate = 0
    }
    
newItem : Int -> String -> Float -> Item
newItem id name price =
    { id = id
    , name = name
    , price = price
    , editing = False
    }

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    (Maybe.withDefault originalModel savedModel, getXchangeRate originalModel.xchangeName originalModel.xchangeUrl)


-- UPDATE

type Msg
    = Nothing
    | UpdateItemName String
    | EditingItem Int Bool
    | UpdateItem Int String
    | Add
    | Delete Int
    | DeleteAll
    | Buy Int String
    | XchRate (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nothing ->
        model ! []
      
    Add ->
        { model | uid = model.uid + 1
                , itemName = ""
                , items = if String.isEmpty model.itemName then
                              model.items
                          else
                              model.items ++ [ newItem model.uid model.itemName 0 ]
        } ! []
        
    UpdateItemName str ->
        { model | itemName = str } ! []

    EditingItem id isEditing ->
        let
            updateI i = if i.id == id then
                            { i | editing = isEditing }
                        else
                            i

            focus = Dom.focus ("item-" ++ toString id)
        in
            { model | items = List.map updateI model.items }
                ! [ Task.attempt (\_ -> Nothing) focus ]

    UpdateItem id item ->
        let
            updateI i = if i.id == id then
                            { i | name = item }
                        else
                            i
        in
            { model | items = List.map updateI model.items }
                ! []

    Delete id ->
        { model | items = List.filter (\i -> i.id /= id) model.items } ! []

    DeleteAll ->
        { model | items = [] } ! []
        
    Buy id priceStr ->
        let
            updateItem i =
                if i.id == id then
                    { i | price = Result.withDefault 0 (String.toFloat priceStr) }
                else
                    i
        in
                { model | items = List.map updateItem model.items } ! []
  
    XchRate (Ok xRateStr) -> 
        let n = Result.withDefault 0 (String.toFloat xRateStr) 
        in
            { model | xchangeRate = n } ! []

    XchRate (Err _) ->
      model ! []
      
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


-- VIEW

view : Model -> Html Msg
view model =
    div
        [ class "shlist-wrapper" ]
        [ section
            [ class "shlist" ]
            [ lazy viewInput model.itemName
            , lazy viewItems model.items
            , lazy viewControls model.items
            , lazy2 viewResult model.items model.xchangeRate
            ]
        , infoFooter
        ]

viewInput : String -> Html Msg
viewInput item =
    header
        [ class "header" ]
        [ h1 [] [ text "shopping-list" ]
        , input
            [ class "new-item"
            , placeholder "What needs to be buy?"
            , autofocus True
            , value item
            , name "newItem"
            , onInput UpdateItemName
            , onEnter Add
            ]
            []
        ]
        
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (Decode.andThen isEnter keyCode)
        
viewItems : List Item -> Html Msg
viewItems items =
    section
        [ class "main" ]
        [ Keyed.ul [ class "item-list" ] <|
            List.map viewKeyedItem items
        ]
            
viewKeyedItem : Item -> ( String, Html Msg )
viewKeyedItem item =
    ( toString item.id, lazy viewItem item )
    
viewItem : Item -> Html Msg
viewItem item =
    let
        originPriceArr = [ class "price", onInput (Buy item.id)]

        priceArr = if item.price == 0 then 
                       originPriceArr ++ [ placeholder "RUB" ]
                   else
                       originPriceArr ++ [ value (toString item.price) ]
    in
        li
            [ classList [ ( "completed", item.price > 0 ), ( "editing", item.editing ) ] ]
            [ div
                [ class "view" ]
                [ input
                    priceArr
                    []
                , label
                    [ onDoubleClick (EditingItem item.id True) ]
                    [ text item.name ]
                , button
                    [ class "destroy"
                    , onClick (Delete item.id)
                    ]
                    []
                ]
        , input
            [ class "edit"
            , value item.name
            , name "title"
            , id ("item-" ++ toString item.id)
            , onInput (UpdateItem item.id)
            , onBlur (EditingItem item.id False)
            , onEnter (EditingItem item.id False)
            ]
            []
        ]
        
viewControls : List Item -> Html Msg
viewControls items =
    footer
        [ class "footer"
        , hidden (List.isEmpty items)
        ]
        [ lazy viewControlsCount (List.length items)
        , lazy viewControlsClear (List.length items)
        ]
        
viewControlsCount : Int -> Html Msg
viewControlsCount cnt =
    let
        item_ =
            if cnt == 1 then
                " item"
            else
                " items"
    in
        span
            [ class "item-count" ]
            [ strong [] [ text (toString cnt) ]
            , text (item_)
            ]
            
viewControlsClear : Int -> Html Msg
viewControlsClear cnt =
    button
        [ class "clear-all"
        , hidden (cnt == 0)
        , onClick DeleteAll
        ]
        [ text ("Clear all (" ++ toString cnt ++ ")") ]

viewResult : List Item -> Float -> Html Msg
viewResult items rate =
    let 
        ta = calcTotalAmount items rate
    in 
        footer
            [ class "total-ammount"
            , hidden (ta == 0)
            ]
            [ text ("USD total ammount: " ++ toString (toFloat (round (ta * 10000)) / 10000)) ]

calcTotalAmount : List Item -> Float -> Float
calcTotalAmount items rate = (List.sum (List.map (\i -> i.price) items)) * rate

infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a item" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/asmln" ] [ text "Anatoly Samoylenko" ]
            ]
        , p [] [ text "My first Elm project" ]
        ]
