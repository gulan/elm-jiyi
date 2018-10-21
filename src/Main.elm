import Browser
import Html exposing (Html, button, div, text, br)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
      
type Msg = GotIt | Ready | Show | TryAgain
type Mode = Announce | Answer | Question | GameOver
type Status = MoreDraw | MoreRetry | NoMore

type alias Card = { chinese : String, pinyin : String , english  : String }
type alias Deck = List Card
type alias Seat = { draw : Deck, retry : Deck }
type alias Model = { mode : Mode, seat : Seat }

dummyCard : Card
dummyCard = {chinese = "Chinese", pinyin = "Pinyin", english = "English"}
            
dummyDeck : Deck
dummyDeck = [ dummyCard ]
            
dummySeat : Seat
dummySeat = {draw = dummyDeck, retry = dummyDeck}

topCard : Seat -> Maybe Card
topCard seat =
    case seat.draw of
        [] -> Nothing
        top :: _ -> Just top

status : Seat -> Status
status seat =
    case (seat.draw, seat.retry) of
        ([], []) -> NoMore
        (_::_, _) -> MoreDraw
        ([], _::_) -> MoreRetry

tossCard : Seat -> Seat
tossCard seat =
    case seat.draw of
        [] -> seat
        _ :: newdraw -> { seat | draw = newdraw }
                        
saveCard : Seat -> Seat
saveCard seat =
    case seat.draw of
        [] -> seat
        card :: newdraw -> { seat | draw = newdraw
                           , retry = seat.retry ++ [ card ] }

restack : Seat -> Seat
restack seat =
    { seat | draw = seat.retry ++ seat.draw, retry = [] }
          
      
init : Model
init = { mode = Announce
       , seat = {draw = [], retry = source}
       }

-- UPDATE
       
update : Msg -> Model -> Model
update msg model =
  case msg of
    Ready
        -> {model | mode = Question, seat = restack model.seat}
        
    Show
        -> {model | mode = Answer}
           
    GotIt
        -> let newseat = tossCard model.seat
           in case status newseat of
                  MoreDraw -> { model | mode = Question, seat = newseat }
                  MoreRetry -> { model | mode = Question, seat = restack newseat }
                  NoMore -> { model | mode = GameOver }
           
    TryAgain
        -> let newseat = saveCard model.seat
           in case status newseat of
                  MoreDraw -> { model | mode = Question, seat = newseat }
                  MoreRetry -> { model | mode = Question, seat = restack newseat }
                  NoMore -> { model | mode = GameOver }


-- VIEW

view : Model -> Html Msg
view model =
  case model.mode of
      Announce
          -> announceView model
             
      Question
          -> questionView model
                  
      Answer
          -> answerView model
                  
      GameOver
          -> gameOverView model
             
             
announceView : Model -> Html Msg
announceView model =
    div []
        [ button [ onClick Ready ] [ text "Start" ]
        ]
                
questionView : Model -> Html Msg
questionView model =
    case topCard model.seat of
        Nothing ->
            div [] [ text "Can't Happen Error" ]
        
        Just card ->
            div []
                [ button [ onClick Show ] [ text "Show" ]
                , div [] [ text (card.chinese) ]
                , div [] [ text (card.pinyin) ]
                ]
        
answerView : Model -> Html Msg
answerView model =
    case topCard model.seat of
        Nothing ->
            div [] [ text "Can't Happen Error" ]
        
        Just card ->
            div []
                [ button [ onClick TryAgain ] [ text "Try Again" ]
                , button [ onClick GotIt ] [ text "Got It" ]
                , div [] [ text (card.chinese) ]
                , div [] [ text (card.pinyin) ]
                , div [] [ text (card.english) ]
                ]
        
gameOverView : Model -> Html Msg
gameOverView model =
    div [] [ text "Game Over" ] 


-- The old HSK1 words.

source : Deck
source = [
    { chinese = "白", pinyin = "bái", english = "white; blank" }
    , { chinese = "百", pinyin = "bǎi", english = "hundred" }
    , { chinese = "摆", pinyin = "bǎi", english = "put; put on; sway" }
    , { chinese = "班", pinyin = "bān", english = "class; team" }
    , { chinese = "搬", pinyin = "bān", english = "move; carry" }
    , { chinese = "半", pinyin = "bàn", english = "half; halfway" }
    , { chinese = "半天", pinyin = "bàntiān", english = "half a day; a long time" }
    , { chinese = "办", pinyin = "bàn", english = "do; deal with" }
    , { chinese = "办法", pinyin = "bànfǎ", english = "way; means; measure" }
    , { chinese = "办公室", pinyin = "bàngōngshì", english = "office" }
    , { chinese = "帮助", pinyin = "bāngzhù", english = "help; assist; aid" }]
