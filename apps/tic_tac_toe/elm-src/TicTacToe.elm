import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Json.Decode exposing (..)
import WebSocket

wsServer : String
wsServer =
    "ws://localhost:8080/connect"

main: Program Never Model Msg
main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }

-- MODEL

type Box = O | X | Empty
type alias Row = List Box
type alias Board = List Row
type alias Model = {
    sessionId: String,
    status: String,
    board: Board
}

init : (Model, Cmd Msg)
init =
    (Model "" "" emptyGrid, newSession)

emptyGrid : Board
emptyGrid = [
        [ Empty, Empty, Empty ],
        [ Empty, Empty, Empty ],
        [ Empty, Empty, Empty ]
    ]

-- UPDATE

type Msg =
    NewGame |
    NewMessage String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewGame ->
            (model, Cmd.none)
        NewMessage str ->
            ((handleMessage str model), Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
      WebSocket.listen wsServer NewMessage

-- VIEW

view : Model -> Html Msg
view model =
    div [] [
        h1 [] [ text "Tic Tac Toe" ],
        div [ style [ ("padding", "25px") ] ] [
            button [ onClick NewGame, disabled True ] [ text "New Game!" ]
        ],
        div [ style [ ("padding", "25px") ] ] [ text model.status ],
        div [ class "board" ] [
            table [ style [ ("border-collapse", "collapse"), ("border", "1px solid black") ] ] [
                tbody [] (List.map renderRow model.board)
            ]
        ]
    ]

renderRow : Row -> Html Msg
renderRow row =
    tr [] (List.map renderCell row)

renderCell : Box -> Html Msg
renderCell box =
    td [ style [ ("border", "1px solid black"), ("height", "50px"), ("width", "50px") ] ] [text (renderBox box)]

renderBox : Box -> String
renderBox box =
    case box of
        O -> "O"
        X -> "X"
        Empty -> ""

newSession : Cmd msg
newSession =
    WebSocket.send wsServer "new_session"

handleMessage : String -> Model -> Model
handleMessage str model =
    case decodeString (field "type" string) str of
        Ok t -> handleMessageType t str model
        Err _ -> model

handleMessageType : String -> String -> Model -> Model
handleMessageType t str model =
    case t of
        "new_session" -> handleNewSessionMsg str model
        _ -> model

handleNewSessionMsg : String -> Model -> Model
handleNewSessionMsg str model =
    case decodeString (field "id" string) str of
        Ok sid ->
            { model | sessionId = sid }
        Err _ -> model
