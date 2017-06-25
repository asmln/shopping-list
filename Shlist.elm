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
        , subscriptions = \_ -> Sub.none
        }

-- MODEL

type alias Model =
    { uid : Int
    , items : List Item
    , itemName : String
    , totalAmmount : Float
    
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
    , totalAmmount = 0
    , xchangeName = "USDRUB"
    , xchangeUrl = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20csv%20where%20url%3D%22http%3A%2F%2Ffinance.yahoo.com%2Fd%2Fquotes.csv%3Fe%3D.csv%26f%3Dc4l1%26s%3D${xName}%3DX%22%3B&format=json&diagnostics=true&callback="
    , xchangeRate = 0
    }
    
newItem : String -> Int -> Entry
newItem id name =
    { id = id
    , name = name
    , price = 0
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
                              model.items ++ [ newItem model.uid model.itemName ]
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
        { model | items = List.filter (\i -> i.id /= id) model.items 
                , totalAmount = calcTotalAmount model } ! []

    DeleteAll ->
        { model | items = [] 
                , totalAmount = 0} ! [] -- send recalculate event
        
    Buy id price ->
        let
            updateItem i =
                if i.id == id then
                    { i | price = price }
                else
                    i
        in
                { model | items = List.map updateItem model.items 
                        , totalAmount = calcTotalAmount model} ! []
  
    XchRate (Ok xRateStr) -> 
        let n = Result.withDefault 0 (String.toFloat xRateStr) 
        in
            { model | xchangeRate = n } ! []

    XchRate (Err _) ->
      model ! []
      
calcTotalAmount : Model -> Float
calcTotalAmount model = (List.sum (List.map (\i -> i.price) model.items)) * model.xchangeRate
    
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
        [ class "shlist-wrapper"
        , style [ ( "visibility", "hidden" ) ]
        ]
        [ section
            [ class "shlist" ]
            [ lazy viewInput model.itemName
            , lazy2 viewItems model.items
            , lazy2 viewControls model.items
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
            , onInput UpdateItem
            , onEnter Add
            ]
            []
        ]
        
onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)
        
viewItems : List Entry -> Html Msg
viewItems items =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ Keyed.ul [ class "item-list" ] <|
                List.map viewKeyedEntry items
            ]
            
viewKeyedEntry : Item -> ( String, Html Msg )
viewKeyedEntry item =
    ( toString item.id, lazy viewItem item )
    
viewItem : Item -> Html Msg
viewItem item =
    li
        [ classList [ ( "completed", item.price > 0 ), ( "editing", item.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "price"
                , onInput (Buy item.id)
                ]
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
            , value todo.name
            , name "name"
            , id ("item-" ++ toString item.id)
            , onInput (UpdateItem item.id)
            , onBlur (EditingItem item.id False)
            , onEnter (EditingItem item.id False)
            ]
            []
        ]
        
viewControls : String -> List Item -> Html Msg
viewControls visibility entries =
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
        [ text ("Clear all (" ++ toString cnt ++ ")")
        ]

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


--------------
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text model.xchangeName ]
        , h3 [] [ text (toString model.xchangeRate) ]
        , button [ onClick ShowXchRate ] [ text "Test!" ]
        ]
