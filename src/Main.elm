{--
elm init
elm install mdgriffith/elm-ui
elm install terezka/elm-charts
elm install elm/svg
elm install elm/random

elm make src/Main.elm --output=app.js --debug

To Do:
* shuffle possible answers https://package.elm-lang.org/packages/elm-community/random-extra/latest/Random-List
* check whether mastery has been established

--}


port module Main exposing (..)

import Array
import Browser
import Browser.Events
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Random


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


addToProgressBar : MasterySettings -> ProgressBar -> RightOrWrong -> ProgressBar
addToProgressBar masterySettings progressBar progress =
    -- Add the latest progress (right or wrong) to the front of...
    Just progress
        -- the current progress bar with the last item removed
        :: List.take (masterySettings.window - 1) progressBar


viewProgressBar : ProgressBar -> Element.Element Msg
viewProgressBar progressBar =
    let
        -- Creates an empty element with a border (a box) for each item in progress list
        drawProgressBox p =
            let
                fillColor =
                    case p of
                        Just RightAnswer ->
                            Element.rgb255 0 255 0

                        Just WrongAnswer ->
                            Element.rgb255 255 0 0

                        Nothing ->
                            Element.rgb 255 255 255
            in
            Element.el
                [ Element.Background.color fillColor
                , Element.padding 10
                , Element.Border.rounded 6
                , Element.Border.width 3
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.height Element.fill
                , Element.width (Element.fillPortion 1)
                ]
                Element.none
    in
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.fillPortion 1)
        , Element.padding 20
        ]
        (List.map drawProgressBox progressBar)



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



-- I use the QuestionType to store the random values I need for each
-- different kind of question I want to ask


type
    QuestionType
    -- Which of these options is a valid response variable for a
    -- linear regression equation?
    = LinearRegressionResponseVariable


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


viewQuestion : Question -> Element.Element Msg
viewQuestion question =
    let
        -- drawButton is used to add one button to the panel for each possible answer
        -- presented to the user
        drawButton btn =
            Element.Input.button
                [ Element.padding 10
                , Element.Border.width 3
                , Element.Border.rounded 6
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.Font.variant Element.Font.smallCaps
                , Element.width (Element.fillPortion 1)
                ]
                { onPress = Just (MsgUserResponded btn)
                , label = Element.el [ Element.centerX ] (Element.text btn.textPart)
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.height (Element.fillPortion 3)
        , Element.padding 20
        , Element.explain Debug.todo
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.column
                [ Element.width (Element.fillPortion 1)
                , Element.padding 10
                ]
                [ Element.text question.stem ]
            , viewQuestionImage question.image
            ]
        , Element.row
            [ Element.width Element.fill ]
            (List.map drawButton question.possibleResponses)
        ]


viewQuestionImage : Maybe QuestionImage -> Element.Element Msg
viewQuestionImage questionImage =
    Element.column
        [ Element.width (Element.fillPortion 1)
        , Element.padding 10
        ]
        [ Element.el
            [ Element.centerX
            , Element.width (Element.px 300)
            , Element.height (Element.px 300)
            ]
            Element.none
        ]



{--
viewScatterPlot : ScatterPlot -> Element.Element Msg
viewScatterPlot sPlot =
    Element.html
        (Chart.chart
            [ Chart.Attributes.height 300
            , Chart.Attributes.width 300
            , Chart.Attributes.padding { top = 30, bottom = 5, left = 40, right = 40 }
            ]
            [ Chart.xLabels [ Chart.Attributes.withGrid ]
            , Chart.yLabels [ Chart.Attributes.withGrid ]
            , Chart.series .x
                [ Chart.scatter .y [ Chart.Attributes.opacity 0.3, Chart.Attributes.borderWidth 1 ]
                    |> Chart.variation (\_ data -> [ Chart.Attributes.size data.size ])
                , Chart.scatter .z [ Chart.Attributes.opacity 0.3, Chart.Attributes.borderWidth 1 ]
                    |> Chart.variation (\_ data -> [ Chart.Attributes.size data.size ])
                ]
                [ { x = 1, y = 2, z = 3, size = 450 }
                , { x = 2, y = 3, z = 5, size = 350 }
                , { x = 3, y = 4, z = 2, size = 150 }
                , { x = 4, y = 1, z = 3, size = 550 }
                , { x = 5, y = 4, z = 1, size = 450 }
                ]
            ]
        )
--}
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
    , Random.generate MsgGotNewQuestion (newLinearRegressionResponseVariableQuestion rightAnswers distractors)
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
    | MsgGetNewQuestion QuestionType -- Create a new random question of type <QuestionType>
    | MsgGotNewQuestion Question -- I just created a new random question, display it
    | MsgUserResponded QuestionResponse -- User pressed a button to choose an answer to the question


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

        -- We need a new random "which of these makes a good response variable?" question
        MsgGetNewQuestion LinearRegressionResponseVariable ->
            ( model
            , Random.generate MsgGotNewQuestion (newLinearRegressionResponseVariableQuestion rightAnswers distractors)
            )

        -- We just got a new random question, so display it and wait for the user to respond
        MsgGotNewQuestion newQuestion ->
            ( { model | currentQuestion = newQuestion }
            , Cmd.none
            )

        -- The user responded, provide feedback, update the progress bar, check whether we're done or if
        -- we should generate another question
        MsgUserResponded userResponse ->
            let
                rightOrWrong =
                    if userResponse.correctAnswer then
                        RightAnswer

                    else
                        WrongAnswer
            in
            ( { model | progressBar = addToProgressBar model.masterySettings model.progressBar rightOrWrong }
            , Random.generate MsgGotNewQuestion (newLinearRegressionResponseVariableQuestion rightAnswers distractors)
            )



-- We need to generate a new random question
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
            , viewQuestion model.currentQuestion
            , viewProgressBar model.progressBar
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


nominalMeasures : List String
nominalMeasures =
    [ "gender"
    , "marital status"
    , "ethnicity"
    , "job title"
    , "employer"
    , "brand of wheelchair"
    , "type of prosthetic ('passive', 'body-powered' or 'myoelectric')"
    ]


ordinalMeasures : List String
ordinalMeasures =
    [ "education level (e.g., 'elementary', 'high school', 'college'...)"
    , "military rank (e.g., 'private', 'corporal', 'sergeant'...)"
    , "product quality (on a scale of 'poor', 'average', 'good' or 'excellent')"
    , "clothing size (e.g., 'small', 'medium', 'large'...)"
    , "frequency of occurrence (on a scale of 'never', 'rarely', 'sometimes', 'always')"
    ]


intervalMeasures : List String
intervalMeasures =
    [ "temperature (in degrees Celsius)"
    , "Functional Independence Measure (FIM) score (ranges from 18 to 126)"
    , "Berg Balance Scale (BBS) score (ranges from 0 to 56)"
    , "Modified Ashworth Scale score (ranges from 0 to 5)"
    , "Mini-Mental State Exam (MMSE) score (ranges from 0 to 30)"
    , "Beck Depression Inventory (BDI) score (ranges from 0 to 63)"
    ]


ratioMeasures : List String
ratioMeasures =
    [ "blood pressure"
    , "weight (in pounds)"
    , "height (in inches)"
    , "heart rate (in beats per minute)"
    , "grip strength (in pounds)"
    , "age (in years)"
    , "the number of ADLs a client can complete independently"
    , "the number of verbal outbursts a child makes during a single class period"
    , "the number of falls an individual has in a month"
    , "score on Timed Up and Go (TUG) test (in seconds)"
    ]


rightAnswers : Array.Array String
rightAnswers =
    intervalMeasures
        ++ ratioMeasures
        |> Array.fromList


distractors : Array.Array String
distractors =
    nominalMeasures
        ++ ordinalMeasures
        |> Array.fromList


fillInLinearRegressionResponseVariableQuestion : Array.Array String -> Array.Array String -> Int -> Int -> Int -> Question
fillInLinearRegressionResponseVariableQuestion rightAnswersArray distractorsArray correctAnswerIndex distractorIndex1 distractorIndex2 =
    let
        correctAnswerLocal : QuestionResponse
        correctAnswerLocal =
            { textPart = Maybe.withDefault "This was supposed to be the right answer" (Array.get correctAnswerIndex rightAnswersArray)
            , feedback = "You chose the right answer"
            , correctAnswer = True
            }

        distractor1Local : QuestionResponse
        distractor1Local =
            { textPart = Maybe.withDefault "This was supposed to be an incorrect answer" (Array.get distractorIndex1 distractorsArray)
            , feedback = "A response variable for linear regression must be an interval or ratio measure"
            , correctAnswer = False
            }

        distractor2Local : QuestionResponse
        distractor2Local =
            { textPart = Maybe.withDefault "This was supposed to be an incorrect answer" (Array.get distractorIndex2 distractorsArray)
            , feedback = "A response variable for linear regression must be an interval or ratio measure"
            , correctAnswer = False
            }
    in
    { stem = "Which of these options is a valid response variable for linear regression equation?"
    , image = Nothing
    , possibleResponses = [ correctAnswerLocal, distractor1Local, distractor2Local ]
    }


newLinearRegressionResponseVariableQuestion : Array.Array String -> Array.Array String -> Random.Generator Question
newLinearRegressionResponseVariableQuestion rightAnswersArray distractorsArray =
    -- I'm going to generate three random values and send them to a function
    Random.map3
        -- This function takes three random values and returns a value of type Question
        (fillInLinearRegressionResponseVariableQuestion rightAnswersArray distractorsArray)
        -- These are the things that generate the three random values
        (Random.int 0 (Array.length rightAnswersArray - 1))
        (Random.int 0 (Array.length distractorsArray - 1))
        (Random.int 0 (Array.length distractorsArray - 1))
