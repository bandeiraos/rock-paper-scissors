module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Process
import Task

-- MAIN

main : Program () Model Action
main =
  Browser.element { 
    init = init
  , update = update
  , subscriptions = subscriptions
  , view = view 
  }

-- MODEL

-- Player Choices:   1 = Rock | 2 = Paper | 3 = Scissors | 0 = Game Not Started
-- Results: 1 = Win | 2 = Draw | 3 = Lose

id_rock : Int
id_rock = 1
id_paper : Int
id_paper = 2
id_scissors : Int
id_scissors = 3
folder_image : String
folder_image = "./images/"



type alias Model =
  { playerChoice : Int
  , houseChoice: Int
  , roundResult: Int
  , score: Int
  , showRules: Bool
  , showResultLabel: Bool
  , showHouseChoice: Bool
  }

init : () -> (Model, Cmd Action)
init _ = 
  (
    { playerChoice = 0 
    , houseChoice = 0
    , roundResult = 0
    , score = 0
    , showRules = False
    , showResultLabel = False
    , showHouseChoice = False
    }, Cmd.none
  )

-- UPDATE

type Action
  = OnPlayerChoses Int
  | SetGameResults Int
  | ShowRules
  | PlayAgain 
  | ShowResultLabel
  | ShowHouseChoice


update : Action -> Model -> (Model, Cmd Action)
update msg model =
  case msg of
    OnPlayerChoses option ->
      ({ model | playerChoice = option }
      , Cmd.batch [
          Random.generate SetGameResults (Random.int 1 3)
        , delayViews 0.5 ShowHouseChoice
        , delayViews 1.5 ShowResultLabel
        ])
      
    SetGameResults houseRandomNum ->
      (getGameResults model houseRandomNum, Cmd.none)

    ShowRules ->
      ({ model | showRules = not model.showRules}, Cmd.none)

    PlayAgain ->
      ({ model | playerChoice = 0, houseChoice = 0, showResultLabel = False, showHouseChoice = False }
      , Cmd.none)

    ShowResultLabel ->
      ({model 
      | showResultLabel = True
      , score = setScore(checkPlayerVsHouse model.playerChoice model.houseChoice) model.score
      }
      , Cmd.none)
    
    ShowHouseChoice ->
        ({ model | showHouseChoice = True }, Cmd.none)
    

delayViews : Float -> Action -> Cmd Action
delayViews t action =
    Process.sleep (t * 1000)
        |> Task.perform (\_ -> action)

getGameResults : Model -> Int -> Model
getGameResults model houseRandomNum =
  { model | 
    houseChoice = houseRandomNum
  , roundResult = checkPlayerVsHouse model.playerChoice houseRandomNum
  }

checkPlayerVsHouse : Int -> Int -> Int
checkPlayerVsHouse p h =
  if p == h then
    2
  else if p == id_rock then -- ROCK
    case h of
      2 -> 3
      3 -> 1
      _ -> 0
  else if p == id_paper then -- PAPER
    case h of
      1 -> 1
      3 -> 3
      _ -> 0
  else if p == id_scissors then -- SCISSOR
    case h of
      1 -> 3
      2 -> 1
      _ -> 0
  else 0

setScore : Int -> Int -> Int
setScore won scr =
  if won == 1 then scr + 1
  else if won == 3 then scr - 1
  else scr

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Action
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Html Action
view model =
  div [class "page"] [
      div [class "page-content"] [
        contentView model
      ]
    , footerView
    , rulesModalView model.showRules
  ]

headerView : Int -> Html msg
headerView score =
  header [] [
    div [class "logo"] [
      img [src (folder_image ++ "logo.svg")] []
    ]
  , div [class "score-wrapper"] [
      div [class "score-content"] [
        span [class "title"] [text "Score"]
      , span [class "score"] [text (String.fromInt score)]
      ]
    ]
  ]

rulesModalView : Bool -> Html Action
rulesModalView show =
  div [classList [("rules-wrapper", True), ("hidden", show == False) ]] [
    div [class "rules-overlay", onClick ShowRules] []
  , div [class "rules-modal"] [
      div [class "rules-modal-header"] [
        span [] [text "Rules"]
      , button [type_ "button", onClick ShowRules] [img [src (folder_image ++ "icon-close.svg")] []]
      ]
    , div [class "rules-modal-body"] [
       img [src (folder_image ++ "image-rules.svg")] []
      ]
    ]
  ]

contentView : Model -> Html Action
contentView model =
  div [class "content-wrapper"]
    [ headerView model.score
    , div [class "content-center"] [
      choiceButtonsView model.playerChoice
    , resultView 
        model.roundResult 
        model.playerChoice 
        model.houseChoice 
        model.showResultLabel 
        model.showHouseChoice
    ]
    ]

footerView : Html Action
footerView = 
  footer [class "footer-wrapper"] [
    button [type_ "button", onClick ShowRules] [text "Rules"]
  ]

getChoiceImg : Int -> String
getChoiceImg c =
  case c of
    1 -> folder_image ++ "icon-rock.svg"
    2 -> folder_image ++ "icon-paper.svg"
    3 -> folder_image ++ "icon-scissors.svg"
    _ -> ""

choiceButtonsView : Int -> Html Action
choiceButtonsView c = 
  div [classList [("choices-wrapper", True), ("hidden", c/=0)]] [
    createButton (getChoiceImg id_paper) OnPlayerChoses 2
  , createButton (getChoiceImg id_rock) OnPlayerChoses 1
  , createButton (getChoiceImg id_scissors) OnPlayerChoses 3
  ]
    
resultView : Int -> Int -> Int -> Bool -> Bool -> Html Action
resultView r p h showResult showHouseChoice =
    div [classList [("result-wrapper", True), ("hidden", p==0)]] [
      getResultChoiceView p True False ((r == 1) && (showResult == True))
    , div [classList [("result-middle", True), ("hidden", showResult == False)]] [
        getResultLabelView r
      , playAgainButtonView
      ]
    , getResultChoiceView h False showHouseChoice ((r == 3) && (showResult == True))
    ]

playAgainButtonView : Html Action
playAgainButtonView =
  button [type_ "button", class "btn-again", onClick PlayAgain ] [text "Play again"]

getResultChoiceView : Int -> Bool -> Bool -> Bool -> Html msg
getResultChoiceView c isPlayer showHouseChoice hasWon =
    div [class "result-choice-wrapper"] [
      span [class "rc-label" ] [text (if isPlayer then "You Picked" else "House Picked")]
    , div [class "rc-inner"] [
        div [classList [("rc-placeholder", True) 
        , ("hidden", isPlaceholderHidden isPlayer showHouseChoice)]] []
      , div [classList [("rc-item circle-choice", True)
        , ("house-choice", if (isPlayer == False) && ((isResultHidden isPlayer showHouseChoice) == False) then True else False )
        , (getChoiceClassname c, True)
        , ("hidden", isResultHidden isPlayer showHouseChoice)
        , ("win-fx", hasWon) 
        ]] [
            div [class "rc-item-inner"] [
                img [src (getChoiceImg c), width 100, height 100] []
            ]
      ]
    ]
    ]

isPlaceholderHidden : Bool -> Bool -> Bool
isPlaceholderHidden ip shc =
    if ((shc==True) || ip == True) then True else False

isResultHidden : Bool -> Bool -> Bool
isResultHidden ip shc =
    if (shc == False) && (ip == False) then True else False

getChoiceClassname: Int -> String
getChoiceClassname c =
  if c == id_rock then "circle-rock" else
  if c == id_paper then "circle-paper" else
  if c == id_scissors then "circle-scissors" else ""

getResultLabelView : Int -> Html msg
getResultLabelView r =
  div [class "result-label"] [
    case r of
        1 -> span [] [text "You Win"]
        2 -> span [] [text "Draw"]
        3 -> span [] [text "You Lose"]
        _ -> span [class "hidden"] []
  ]

createButton : String -> (Int -> msg) -> Int -> Html msg
createButton imgSrc onclick option = 
  button [type_ "button", classList [("choice-option", True), (getChoiceClassname option, True)], 
    onClick (onclick option)] [
    div [class "choice-option-inner"] [
        img [src imgSrc] []
    ]
  ]