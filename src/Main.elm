import Browser
import Html exposing (Html, button, div, text, br)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
      
type Msg = Restack | Show | Discard | Retry
    
type Mode = Question | Answer
    
type alias Model = { mode : Mode
                   , chinese : String
                   , pinyin : String
                   , english  : String
                   }

init : Model
init = { mode = Question
       , chinese = "C"
       , pinyin = "P"
       , english = "E"
       }

-- UPDATE
       
update : Msg -> Model -> Model
update msg model =
  case msg of
    Restack
        -> {model | mode = Question}
           
    Show
        -> {model | mode = Answer}
           
    Discard
        -> {model | mode = Question}
           
    Retry
        -> {model | mode = Question}

-- VIEW

view : Model -> Html Msg
view model =
  case model.mode of
      Question
          -> questionView model
                  
      Answer
          -> answerView model
             
questionView : Model -> Html Msg
questionView model =
    div []
        [ button [ onClick Restack ] [ text "Restack" ]
        , button [ onClick Show ] [ text "Show" ]
        , div [] [ text (model.chinese) ]
        , br [] []
        , div [] [ text (model.pinyin) ]
        ]
        
answerView : Model -> Html Msg
answerView model =
    div []
        [ button [ onClick Discard ] [ text "Discard" ]
        , button [ onClick Retry ] [ text "Retry" ]
        , div [] [ text (model.chinese) ]
        , br [] []
        , div [] [ text (model.pinyin) ]
        , br [] []
        , div [] [ text (model.english) ]
        ]
