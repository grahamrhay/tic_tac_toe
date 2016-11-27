import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List

main: Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type Box = O | X | Empty
type alias Row = List Box
type alias Board = List Row
type alias Model = {
    status: String,
    board: Board
}

model : Model
model =
    Model "" [
        [ Empty, Empty, Empty ],
        [ Empty, Empty, Empty ],
        [ Empty, Empty, Empty ]
    ]

-- UPDATE

type Msg = NewGame

update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            model

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
