{--
elm init
elm install mdgriffith/elm-ui
elm install terezka/elm-charts
elm install elm/svg
elm install elm/random

elm make src/Main.elm --output=app.js --debug

--}


port module Main exposing (..)

import Browser
import Browser.Events
import Element
import Html


main : Program WindowDimensions Model Msg
main =
    Browser.element
        { init = initializeModel
        , view = viewModel
        , update = updateModel
        , subscriptions = mySubscriptions
        }



{-
   The ports get data in from Torus to initialize the exercise and then send
   data back to Torus to let it know that we're done.

   I have to use a port instead of flags because Torus doesn't get all the initialization
   data to my code quickly enough.

   I use subscriptions to receive data from the ports and to window events.
-}


port getFromTorus : (MasterySettings -> msg) -> Sub msg


port sendToTorus : Bool -> Cmd msg


mySubscriptions : Model -> Sub Msg
mySubscriptions _ =
    Sub.batch
        [ -- Received mastery settings (threshold and window) from Torus
          getFromTorus MsgGetFromTorus

        -- Update the model when the browser window gets resized
        , Browser.Events.onResize MsgWindowSizeChanged
        ]



{--
Mastery Settings get passed in through a JS port from Torus. The settings determine how many questions the user
must get right (the threshold) out of how many of the last questions asked (the window)
--}


type alias MasterySettings =
    { threshold : Int -- User needs to get <threshold> questions right...
    , window : Int -- out of the last <window> questions
    }


defaultMasterySettings : MasterySettings
defaultMasterySettings =
    { threshold = 4
    , window = 6
    }



{--
Window dimensions are used to make the app responsive. I need to keep track of the width and height.
The initial values get passed in to the app as flags. Then I use a subscription to keep track of 
changes during use.
--}


type alias WindowDimensions =
    { winWidth : Int
    , winHeight : Int
    }


defaultWindowDimensions : WindowDimensions
defaultWindowDimensions =
    { winWidth = 800
    , winHeight = 600
    }



{--
The progress bar has one item for each question in the "mastery window".
If the user needs to get 4 (threshold) of the last 6 (window) right, then 
the progress bar has 6 items.

Each item in the progress bar can have three possible states:
* The user got the question right (Just RightAnswer)
* The user got the question wrong (Just WrongAnswer)
* The question hasn't been asked yet (Nothing)
--}


type RightOrWrong
    = RightAnswer -- user chose the correct answer
    | WrongAnswer -- user chose the wrong answer


type alias ProgressBar =
    List (Maybe RightOrWrong)


emptyProgressBar : Int -> ProgressBar
emptyProgressBar masteryWindow =
    List.repeat masteryWindow Nothing



{--
A multiple choice question has three parts: 
* the stem 
* the list of possible responses
* an image (optional)

A response has three parts:
* the text that is shown to the user
* the feedback to give the user if they choose this response
* a flag for whether this response is correct or not
--}


type alias QuestionResponse =
    { textPart : String -- What gets displayed on the button for the user to choose
    , feedback : String -- The feedback associated with this answer
    , correctAnswer : Bool -- True when this is the right answer
    }


correctResponse : QuestionResponse
correctResponse =
    { textPart = "This is the right answer"
    , feedback = "You chose the right answer"
    , correctAnswer = True
    }


oneDistractor : QuestionResponse
oneDistractor =
    { textPart = "This is the first distractor"
    , feedback = "You chose the first distractor"
    , correctAnswer = False
    }


anotherDistractor : QuestionResponse
anotherDistractor =
    { textPart = "This is the second distractor"
    , feedback = "You chose the second distractor"
    , correctAnswer = False
    }


type QuestionImage
    = ImgDummy


type alias Question =
    { stem : String
    , possibleResponses : List QuestionResponse
    , image : Maybe QuestionImage
    }


emptyQuestion : Question
emptyQuestion =
    { stem = "This is an empty question"
    , possibleResponses = [ correctResponse, oneDistractor, anotherDistractor ]
    , image = Nothing
    }



{-
   This is the model for the state. I keep the question in here along with keeping track of
   the user's response to the question being asked, how many questions the user has gotten
   right, and the number of questions the user is supposed to be asked.
-}


type alias Model =
    { progressBar : ProgressBar
    , masterySettings : MasterySettings
    , windowDimensions : WindowDimensions
    , currentQuestion : Question
    , debug : Bool -- Do we show debug information?
    }


createNewModel : WindowDimensions -> MasterySettings -> Model
createNewModel newWindowDimensions newMasterySettings =
    { progressBar = emptyProgressBar newMasterySettings.window
    , masterySettings = newMasterySettings
    , windowDimensions = newWindowDimensions
    , currentQuestion = emptyQuestion
    , debug = True
    }


initializeModel : WindowDimensions -> ( Model, Cmd Msg )
initializeModel windowDimensions =
    ( createNewModel windowDimensions defaultMasterySettings
    , Cmd.none
    )



{-
   The flow for this program:
   * Initialize the exercise
       * Flags from JS -> window dimensions
       * getFromTorus port -> mastery settings (window, threshold)
   * Make a question
       * GetRandomQuestionParameters
           - If the numbers combine to make unique answers -> GotRandomQuestion
           - Otherwise -> GetNextQuestion
   * Present the question and wait for a response
   * Evaluate whether they got it right
       * GotResponse
           - If they are done, then send control back to Torus -> MsgSendToTorus and sendToTorus port
           - Otherwise, get the next question -> GetNextQuestion
-}


type Msg
    = MsgSendToTorus -- The user reached the threshold, go back to Torus (send to JavaScript)
    | MsgGetFromTorus MasterySettings -- Settings for mastery questions coming in from Torus (get from JavaScript)
    | MsgWindowSizeChanged Int Int -- Window changed size - maybe the device was rotated, maybe a change in the window


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        -- The user has demonstrated mastery, kick control back to Torus
        MsgSendToTorus ->
            ( model, sendToTorus True )

        -- Data to initialize the exercise has come in from Torus.
        MsgGetFromTorus settings ->
            let
                newMasterySettings : MasterySettings
                newMasterySettings =
                    { threshold = settings.threshold
                    , window = settings.window
                    }

                currentWindowDimensions : WindowDimensions
                currentWindowDimensions =
                    model.windowDimensions
            in
            ( createNewModel currentWindowDimensions newMasterySettings
            , Cmd.none
            )

        -- Something happened to change the window size, update the model to store the new size
        MsgWindowSizeChanged newWidth newHeight ->
            let
                newWindowDimensions : WindowDimensions
                newWindowDimensions =
                    { winWidth = newWidth
                    , winHeight = newHeight
                    }
            in
            ( { model | windowDimensions = newWindowDimensions }
            , Cmd.none
            )



{-
   The view consists of five stacked panels. I only display/update the panels that I need to at any given time.

    Instructions
    ----------------
    Question
    Image
    Answer Buttons
    ----------------
    Feedback
    ----------------
    Progress Bar
    ----------------
    Debug Info

   I use the status member of the model to determine which panels get displayed.
-}


viewModel : Model -> Html.Html Msg
viewModel model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.column
            [ Element.width Element.fill ]
            [ viewInstructionsPanel model.masterySettings
            , viewDebugPanel model
            ]
        )


viewInstructionsPanel : MasterySettings -> Element.Element Msg
viewInstructionsPanel masterySettings =
    let
        instructions =
            "You need to answer "
                ++ String.fromInt masterySettings.threshold
                ++ " out of your last "
                ++ String.fromInt masterySettings.window
                ++ " questions in order to advance."
    in
    Element.column
        [ Element.width Element.fill
        , Element.height (Element.fillPortion 1)
        , Element.padding 20
        ]
        [ Element.paragraph [] [ Element.text instructions ]
        ]


viewDebugPanel : Model -> Element.Element Msg
viewDebugPanel model =
    if model.debug then
        Element.column
            [ Element.width Element.fill
            , Element.height (Element.fillPortion 1)
            , Element.padding 20
            ]
            [ Element.paragraph [] [ Element.text ("threshold: " ++ String.fromInt model.masterySettings.threshold) ]
            , Element.paragraph [] [ Element.text ("window: " ++ String.fromInt model.masterySettings.window) ]
            , Element.paragraph [] [ Element.text ("window height: " ++ String.fromInt model.windowDimensions.winHeight) ]
            , Element.paragraph [] [ Element.text ("window width: " ++ String.fromInt model.windowDimensions.winWidth) ]
            ]

    else
        Element.none
