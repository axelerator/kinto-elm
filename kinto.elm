module Kinto where
import Html exposing (Html, Attribute, text, toElement, div, input, button)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Signal exposing (Address, Mailbox)
import StartApp.Simple as StartApp
import String
import List exposing (map, length, maximum)
import Task
import Effects
--port addUser : Signal (String, UserRecord)

mehlbox = Signal.mailbox NoOp

port updateStorage : Signal Action
port updateStorage = .signal <| mehlbox

type alias QuestionId = Int

type alias Question = {
    id: QuestionId,
    text: String
  }

type alias Exam = {
    questions: List Question
  }

type Action = NoOp
            | AddQuestion
            | UpdateQuestion QuestionId String

emptyExam = { questions = [] } 
newQuestion exam = { text = "How much is the fish?"
                   , id = (nextQuestionId exam)
                   }

nextQuestionId exam =
  let 
    maxId = maximum ( (map .id) exam.questions)
  in
    case maxId of
      Nothing -> 1
      Just lastId -> lastId + 1
model : Signal Exam
model =
  Signal.foldp update emptyExam mehlbox.signal

main =
  Signal.map (view mehlbox.address) model
  --StartApp.start { model = emptyExam,  view = view, update= update}


updateQuestion exam questionId newText =
  let
      swapIfMatch q = if q.id == questionId then
                        { q | text = newText }
                      else
                        q
  in
    { exam | questions = map swapIfMatch exam.questions }

update : Action -> Exam -> Exam
update action oldExam =
  case action of
    NoOp -> (oldExam)
    AddQuestion -> { oldExam | questions = (newQuestion oldExam) :: oldExam.questions }
    UpdateQuestion qId newText -> updateQuestion oldExam qId newText


questionView address question =
  div [] [input 
           [ value question.text,
             on "input" targetValue (Signal.message address << UpdateQuestion question.id)
           ]
           []
         ]

view : Address Action -> Exam -> Html
view address exam =
  div []
    [ div [] (List.map (questionView address) exam.questions),
      div [ myStyle ] [ text (toString (List.length exam.questions)) ]
    , button [ onClick address AddQuestion ] [ text "Add IT!!" ]
    ]


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
