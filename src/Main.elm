module Main exposing (..)

import Browser
import Array exposing (Array)
import Html exposing (Html, h1, div, text, br, input, span, button)
import Html.Attributes as A exposing (id, class, style, type_, value)
import Html.Events exposing (preventDefaultOn,custom,onClick, onInput)
import Random exposing (Generator)
import Task exposing (perform, succeed)
import Json.Decode as Json
import Debug exposing (log)
import Html exposing (img)
import Html.Attributes exposing (src)
import Html exposing (p)
import Html exposing (Attribute)
import Html.Attributes exposing (contextmenu)
import Html exposing (strong)
import Set
import Mine
import String
import Html.Attributes exposing (disabled)



type alias BoardArray a = Array (Array a)

type alias Model =
  { board : BoardArray CaseState
  , states : BoardArray CaseFlagState
  , minesCount : Int
  , boardWidth : Int 
  , boardHeight : Int
  , gameStatus : GameStatus
  , input : InputMode
  , startX : Int
  , startY : Int
  , flag : Int
  , bomb : Int
  }

type CaseState --Etats des cases autour d'une mine
  = Mine
  | Space Int 

type CaseFlagState --Etats des cases autour de la case courrante
  = Opened Int 
  | Flagged
  | Hidden

type GameStatus -- Etats possibles de la partie
  = Start
  | First
  | Playing
  | GameOver
  | Win

type InputMode --Ouvrir les cases ou mettre des drapeaux là où l'on pense qu'il y a des bombes
  = Open




initModel =
  { board = Array.empty
  , states = Array.empty
  , minesCount = 99 -- 3 
  , boardWidth = 30 -- 3
  , boardHeight = 16 -- 3 
  , gameStatus = Start
  , input = Open
  , startX = 0
  , startY = 0
  , flag = 0
  , bomb = 99 -- 3
  }

type Msg
  = LeftClick( Int, Int )
  | Place Int ( Int, Int )
  | MakeBoard ()
  | Button
  | TextBox Type String
  | NOop
  | RightClick ( Int, Int )

type Type
  = Width
  | Height
  | MinesCount

increment : Int -> Int -> Int -- permettra l'affichage du nombre de drapeaux et le nombre de bombes cliqués avec leurs évolutions au fil du jeu
increment value model =
  model + value 

decrement : Int -> Int -> Int -- permettra l'affichage du nombre de drapeaux et le nombre de bombes cliqués avec leurs évolutions au fil du jeu
decrement value model =
  model - value 

onRightClick : msg -> Html.Attribute msg -- permettra de faire le click droit pour afficher les drapeaux
onRightClick message =
    custom "contextmenu"
        (Json.succeed { message = message, preventDefault = True, stopPropagation = False })

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    noChange = ( model, Cmd.none )
    debug = model |> log "model"
  in
    case msg of
      NOop -> noChange
      Button ->
        case model.gameStatus of
          Start ->
            ( { model |
                 gameStatus = First
              ,  input = Open
              , board =
                (\(x,y) ->
                  Array.repeat y (Array.repeat x (Space 0))
                )
                (model.boardWidth,model.boardHeight)
              , states =
                (\(x,y) ->
                  Array.repeat y (Array.repeat x (Hidden))
                )
                (model.boardWidth,model.boardHeight)
              }
            , Cmd.none
            )
          GameOver ->
            ( { model | gameStatus = Start , bomb = model.minesCount , flag = 0 }
            , Cmd.none
            )
          Win ->
            ( { model | gameStatus = Start , bomb = model.minesCount , flag = 0}
            , Cmd.none
            )
          _ -> noChange
      LeftClick(x,y) -> 
        case model.input of
          Open ->
            if model.gameStatus == First
            then
              ( { model |
                   startX = x
                , startY = y
                , states =
                  boardSet x y
                    (Hidden)
                    model.states
                , gameStatus = Playing
                }
              , Mine.generateRandomMines
                { width = model.boardWidth , height = model.boardHeight } (Place 1)
              )
            else
              if boardGet x y model.states == Just Flagged
              then
                noChange
              else
                case boardGet x y model.board of
                  Just Mine ->
                    ( { model |
                        gameStatus = GameOver
                      , states =
                        boardSet x y (Opened -1) model.states
                      }
                    , Cmd.none
                    )
                  _ ->
                    let
                      num =
                        case boardGet x y model.board of
                          Just (Space n) -> n
                          _ -> -1
                      setStates =
                        boardSet x y (Opened num)
                          model.states
                      newStates =
                        if num == 0
                        then
                          letOpen (x,y)
                            model.board
                            setStates
                        else
                          setStates
                    in
                      ( { model |
                          states = newStates
                        }
                        |> allChecked
                      , Cmd.none )
      Place n (x,y) ->
        if
          ( (||)
            (boardGet x y model.board == Just Mine)
            ((x,y)==(model.startX,model.startY))
          )
        then
          ( model
          , Mine.generateRandomMines
                { width = model.boardWidth , height = model.boardHeight } (Place n)
          )
        else
          let
            newModel =
              { model |
                board =
                  boardSet
                  x y
                  Mine
                  model.board
              }
          in
            if n == model.minesCount
            then
              ( newModel, perform MakeBoard (Task.succeed ()))
            else
            ( newModel
            , Mine.generateRandomMines
                { width = model.boardWidth , height = model.boardHeight } (Place (n + 1))
            )
      MakeBoard _ ->
        let
          (x,y) = (model.startX,model.startY)
          setModel = makeBoard model
          startPositionNum =
            case boardGet x y setModel.board of
              Just (Space n) -> n
              _ -> -1
          newModel =
            if startPositionNum == 0
            then
              { setModel |
                states =
                  letOpen (model.startX,model.startY)
                    setModel.board
                    setModel.states
              }
            else
              setModel
        in
          ( newModel, Cmd.none )
      TextBox t s ->
        let
          n = Maybe.withDefault 2 (String.toInt s)
        in
          case t of
            Width ->
              ( { model |
                  boardWidth = 
                  model.boardWidth
                }
              , Cmd.none
              )
            Height ->
              ( { model |
                  boardHeight = model.boardHeight
                }
              , Cmd.none
              )
            MinesCount ->
              ( { model |
                  minesCount = n
                }
              , Cmd.none
              )
      RightClick (x,y) ->
        case model.input of
          Open ->
            case boardGet x y model.states of
              Just Flagged ->
                ( { model |
                    states =
                      boardSet x y Hidden model.states
                      , flag = decrement 1 model.flag
                      , bomb = increment 1 model.bomb
                    
                  }
                , Cmd.none
                )
              Just Hidden ->
                ( { model |
                    states =
                      boardSet x y Flagged model.states
                      , flag = increment 1 model.flag
                      , bomb = decrement 1 model.bomb
                  }
                , Cmd.none
                )
              _ -> noChange
    
          
  
allChecked : Model -> Model -- Permet de voir si toute la grille est remplie
allChecked model =
  if
    (==)
      ( boardCount 
        (\s -> case s of
          Opened n -> False
          _ -> True
        )
        model.states
      )
      model.minesCount
  then
    { model | gameStatus = Win }
  else
    model


checkAround = -- regarde les 8 cases autour 
  [(-1,-1),(0,-1),(1,-1)
  ,(-1, 0)       ,(1, 0)
  ,(-1, 1),(0, 1),(1, 1)
  ]

letOpen : (Int,Int) -> BoardArray CaseState -> BoardArray CaseFlagState  -> BoardArray CaseFlagState -- auto ouverture
letOpen (x,y) f vf =
  let
    check (x_,y_) f_ vf_ =
      case (boardGet x_ y_ f_, boardGet x_ y_ vf_ ) of
        (Just (Space 0), Just Hidden) -> True
        _ -> False
    letOpenHelper (x_,y_) f_ vf_ =
      checkAround
        |> List.foldl
          (\(ax,ay) nvf ->
            if check ( ax+x_, ay+y_) f_ nvf
            then
              letOpenHelper ( ax+x_, ay+y_) f_
                ( boardSet (ax+x_) (ay+y_)
                  (Opened 0) nvf
                )
            else
              let
                getNum =
                  case boardGet (ax+x_) (ay+y_) f_ of
                    Just (Space n) -> n
                    _ -> -1
              in
                boardSet (ax+x_) (ay+y_)
                  (Opened getNum) nvf
          )
          vf_
  in
    letOpenHelper (x,y) f vf



makeBoard : Model -> Model -- créateur de  grille 
makeBoard model =
  let
    debug = log "called" "makeBoard"
    boardList = getboardList (model.boardWidth,model.boardHeight)
    newBoard =
      boardList
        |> List.concat
        |> List.foldl
          (\(x,y) board ->
            boardSet x y
              ( case boardGet x y board of
                Just Mine -> Mine
                _ ->
                  ( checkAround
                    |> List.foldl
                      (\(x_,y_) n ->
                        case boardGet (x+x_) (y+y_) board of
                          Just Mine -> n + 1
                          _ -> n
                      )
                      0
                  )
                    |> Space
              )
            board
          )
          model.board
    newStates =
      let
        (x,y) = (model.startX,model.startY)
      in
        boardSet x y
          (case boardGet x y newBoard of
            Just (Space n) -> Opened n
            _ -> Hidden
          )
          model.states
  in
    { model |
      board = newBoard
    , states = newStates
    }

boardGet : Int -> Int -> BoardArray a -> Maybe a -- obtenir le statut du board
boardGet x y arr =
  case Array.get y arr of
    Just a -> Array.get x a
    _ -> Nothing

boardSet : Int -> Int -> a -> BoardArray a -> BoardArray a -- modifier le statut du board
boardSet x y after arr =
  Array.set
    y
    ((Maybe.withDefault
        Array.empty
        (Array.get y arr)
      )
      |> Array.set x after
    )
    arr

boardCount : (a -> Bool) -> BoardArray a -> Int -- à utiliser dans allChecked
boardCount  f arr =
  arr
    |> Array.toList
    |> List.concatMap Array.toList
    |> List.filter f
    |> List.length


getboardList : (Int,Int) -> List (List (Int,Int) ) -- permettra d'afficher la grille
getboardList  (x,y) =
  List.range 0 (y - 1)
    |> List.map
      (\y_ ->
        List.range 0 (x - 1)
          |> List.map
            (\x_ -> ( x_, y_ ))
      )


view : Model -> Html Msg
view model =
  let
    ( width, height ) = (model.boardWidth,model.boardHeight)
    boardList = getboardList (model.boardWidth,model.boardHeight)
    buttonSize = "30px"
    bombCheck x y =
      (&&)
        (boardGet x y model.board == Just Mine)
        (model.gameStatus == Win || model.gameStatus == GameOver)
    viewBoard =
      boardList
        |> List.map
          (\li ->
            li
            |> List.map
            (\(x,y) ->
              case boardGet x y model.states of
                Just (Opened n) ->
                  button
                    [ onRightClick <| RightClick (x,y)
                    , onClick <| LeftClick(x,y)
                    , style "width" buttonSize
                    , style "height" buttonSize
                    , disabled (model.gameStatus == GameOver || model.gameStatus == Win) 
                    ]
                    [ if bombCheck x y 
                      then
                        text "💣"
                      else
                        text (" "++(String.fromInt n)++" ") 
                    ]
                Just Flagged ->
                  button
                    [ onRightClick <| RightClick (x,y)
                    , onClick <| LeftClick(x,y)
                    , style "width" buttonSize
                    , style "height" buttonSize
                    , style "background-color" "gray"
                    , disabled (model.gameStatus == GameOver || model.gameStatus == Win)
                    ]
                    [ if bombCheck x y  then text "💣" else text "🚩" ]
                  
                Just Hidden ->
                  button
                    [ onRightClick <| RightClick (x,y)
                    , onClick <| LeftClick(x,y)
                    , style "width" buttonSize
                    , style "height" buttonSize
                    , style "background-color" "gray"
                    , disabled (model.gameStatus == GameOver || model.gameStatus == Win) 
                    ]
                    [ if bombCheck x y then text "💣" else text "　" ]
                _ -> text ""
            )
          )
        |> List.intersperse [br[][]]
        |> List.concat  

  in
    case model.gameStatus of
      Start ->
        div[style "margin" "150px"]
          [ img [src "https://www.seekpng.com/png/detail/502-5024623_chibi-saying-hello-cartoon.png"] [] 
            ,h1 [] [text "Welcome to our Minesweeper 😆 !"]
          , br[][]
          , br[][]
          , button[style "border-radius" "10px",style "background-color" "LightSalmon",style "font-size" "20px",style "padding" "10px",onClick Button][text "Go "]
          ]
          
      GameOver ->
        div[style "margin" "150px"]
          <| List.append
            viewBoard
            [ br[][]
            ,br[][]
            , text "Game Over 😞 "
            , br[][]
            , br[][]
            , button[style "border-radius" "10px",style "background-color" "LightSalmon",style "font-size" "20px",style "padding" "10px",onClick Button][text "Continue"]
            ]
      Win ->
        div[style "margin" "150px"]
          <| List.append
            viewBoard
            [ br[][]
            , br[][]
            , text "Good Job 😄"
            , br[][]
            , br[][]
            , button[style "border-radius" "10px",style "background-color" "LightSalmon",style "font-size" "20px",style "padding" "10px",onClick Button][text "Another one ? "]
            ]
      _ ->
        div[style "margin" "150px"]
          <| List.append
            viewBoard
            [ br[][]
            , br[][]
            , text (String.fromInt model.bomb ++ " 💣    "++String.fromInt model.flag ++ " 🚩")
            , br[][]
            , br[][]
            , text "Good luck 🤞 "
            , br[][]
            , br[][]
            ]




main : Program () Model Msg
main =
  Browser.element
    { init = \_ -> ( initModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
