{--
elm init
elm install mdgriffith/elm-ui
elm install terezka/elm-charts
elm install elm/svg
elm install elm/random
elm install elm-community/random-extra

elm make src/Main.elm --output=app.js --debug

To Do:
* shuffle possible answers https://package.elm-lang.org/packages/elm-community/random-extra/latest/Random-List
* check whether mastery has been established

--}


port module Main exposing (..)

import Browser
import Browser.Events
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Random
import Random.List


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
          getFromTorus MsgUpdateMasterySettings

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


type alias ProgressBar =
    List (Maybe Bool)


emptyProgressBar : Int -> ProgressBar
emptyProgressBar masteryWindow =
    List.repeat masteryWindow Nothing


addToProgressBar : MasterySettings -> ProgressBar -> Bool -> ProgressBar
addToProgressBar masterySettings progressBar progress =
    -- Add the latest progress (right or wrong) to the front of...
    Just progress
        -- the current progress bar with the last item removed
        :: List.take (masterySettings.window - 1) progressBar


updateProgressBar : Model -> Model
updateProgressBar model =
    let
        newProgressBar : ProgressBar
        newProgressBar =
            case model.userResponse of
                -- We don't have any response from the user, yet, so don't change
                -- the progress bar at all
                Nothing ->
                    model.progressBar

                -- We did get a response from the user, it's either right or wrong
                Just actualResponse ->
                    -- Add the latest progress (right or wrong) to the front of...
                    Just actualResponse.correctAnswer
                        -- the current progress bar with the last item removed
                        :: List.take (model.masterySettings.window - 1) model.progressBar
    in
    { model | progressBar = newProgressBar }


viewProgressBar : ProgressBar -> Element.Element Msg
viewProgressBar progressBar =
    let
        -- Creates an empty element with a border (a box) for each item in progress list
        drawProgressBox p =
            let
                fillColor =
                    case p of
                        Just True ->
                            Element.rgb255 0 255 0

                        Just False ->
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


masteryThresholdReached : Model -> Bool
masteryThresholdReached model =
    let
        ( rightAnswers, _ ) =
            -- split the progress bar into two lists: the right answers and everything else (wrong and not yet)
            List.partition (\rOrW -> rOrW == Just True) model.progressBar
    in
    -- has the number of right answers reached the threshold?
    List.length rightAnswers >= model.masterySettings.threshold



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
    { correctAnswer : Bool -- True when this is the right answer
    , feedback : String -- The feedback associated with this answer
    , textPart : String -- What gets displayed on the button for the user to choose
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
    , image : Maybe QuestionImage
    , possibleResponses : List QuestionResponse
    }


emptyQuestion : Question
emptyQuestion =
    { stem = "This is an empty question"
    , possibleResponses = [ correctResponse, oneDistractor, anotherDistractor ]
    , image = Nothing
    }


viewQuestion : Model -> Element.Element Msg
viewQuestion model =
    let
        -- drawButton is used to add one button to the panel for each possible answer
        -- presented to the user
        -- The buttons are only active if the user has not selected a response
        -- Once the user chooses an answer, the buttons deactivate
        drawButton btn =
            let
                btnResponse =
                    if model.userResponse == Nothing then
                        -- If the user has not chosen a button, yet, then the buttons
                        -- are active
                        Just (MsgUserResponded btn)

                    else
                        -- otherwise, buttons are all deactivated
                        Nothing

                btnBackgroundColor =
                    -- I want to set the button color based on whether or not it's the right answer
                    if model.userResponse == Nothing then
                        -- If the user hasn't chosen a button, then the button is white
                        Element.rgb255 255 255 255

                    else if btn.correctAnswer then
                        -- If this was the correct answer, color it green
                        Element.rgb 0 200 0

                    else
                        -- If this wasn't the correct answer, then color it red
                        Element.rgb255 200 0 0
            in
            Element.Input.button
                [ Element.padding 10
                , Element.Border.width 3
                , Element.Border.rounded 6
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.Font.variant Element.Font.smallCaps
                , Element.width (Element.fillPortion 1)
                , Element.Background.color btnBackgroundColor
                ]
                { onPress = btnResponse
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
                [ Element.text model.currentQuestion.stem ]
            , viewQuestionImage model.currentQuestion.image
            ]
        , Element.row
            [ Element.width Element.fill ]
            (List.map drawButton model.currentQuestion.possibleResponses)
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
    , userResponse : Maybe QuestionResponse
    , debug : Bool -- Do we show debug information?
    }


createNewModel : WindowDimensions -> MasterySettings -> Model
createNewModel newWindowDimensions newMasterySettings =
    { progressBar = emptyProgressBar newMasterySettings.window
    , masterySettings = newMasterySettings
    , windowDimensions = newWindowDimensions
    , currentQuestion = emptyQuestion
    , userResponse = Nothing
    , debug = True
    }


initializeModel : WindowDimensions -> ( Model, Cmd Msg )
initializeModel windowDimensions =
    -- Start the app with the window dimensions from the browser
    -- and the default mastery settings
    -- mastery settings will get updated through a message from
    -- Torus almost immediately
    ( createNewModel windowDimensions defaultMasterySettings
      -- Create a new random question to display to the user
      -- and wait for a response
    , Random.generate MsgDisplayNewQuestion newQuestion
    )



{-
   The flow for this program:
    * Initialize the exercise (ports and init function)
       * Flags from JS -> window dimensions
       * getFromTorus port -> mastery settings (window, threshold)
    * Show a question and wait for user to respond
        * Make a new random question [Random.generate MsgDisplayNewQuestion newQuestion]
        * Display the new question
    * User submits an answer so we process the response
        * Give feedback associated with response
        * Update progress bar
    * Wait for user to read response and press a button to continue
        * If the user is done, tell them to press a button to go back to Torus
        * If the user is not done, tell them to press a button
-}


type Msg
    = MsgReturnToTorus -- The user reached the threshold, go back to Torus (send to JavaScript)
    | MsgUpdateMasterySettings MasterySettings -- Settings for mastery questions coming in from Torus (get from JavaScript)
    | MsgWindowSizeChanged Int Int -- Window changed size - maybe the device was rotated, maybe a change in the window
    | MsgGetNewQuestion -- The user has read the feedback and has asked for the next question
    | MsgDisplayNewQuestion Question -- I just created a new random question, display it
    | MsgUserResponded QuestionResponse -- User pressed a button to choose an answer to the question


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        -- The user has demonstrated mastery, kick control back to Torus
        MsgReturnToTorus ->
            ( model, sendToTorus True )

        -- Data to initialize the exercise has come in from Torus.
        MsgUpdateMasterySettings settings ->
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

        -- The user has read the feedback and asked for a new question
        MsgGetNewQuestion ->
            ( model, Random.generate MsgDisplayNewQuestion newQuestion )

        -- We just got a new random question, so display it and wait for the user to respond
        MsgDisplayNewQuestion newRandomQuestion ->
            ( { model
                | currentQuestion = newRandomQuestion
                , userResponse = Nothing
              }
            , Cmd.none
            )

        -- The user responded. We need to:
        -- 1. provide feedback,
        -- 2. update the progress bar
        MsgUserResponded newUserResponse ->
            ( { model
                | userResponse = Just newUserResponse
                , progressBar = addToProgressBar model.masterySettings model.progressBar newUserResponse.correctAnswer
              }
            , Cmd.none
              -- , Random.generate MsgDisplayNewQuestion newQuestion
            )



-- We need to generate a new random question
{-
   The view consists of stacked panels. I only display/update the panels that I need to at any given time.

    Instructions
    ----------------
    Progress Bar
    ----------------
    Question | Image
    Answer Buttons (Active)
    ----------------
    Debug Info


    Instructions
    ----------------
    Progress Bar
    ----------------
    Question | Image
    Answer Buttons (Not Active)
    ----------------
    Feedback
    ----------------
    Next Question Button / Go Back to Torus Button
    ----------------
    Debug Info

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
            , viewProgressBar model.progressBar
            , viewQuestion model
            , viewFeedback model
            , viewDebugPanel model
            ]
        )


viewFeedback : Model -> Element.Element Msg
viewFeedback model =
    case model.userResponse of
        Nothing ->
            -- If the user hasn't made a choice, then we don't give any feedback
            Element.none

        Just userResponse ->
            -- otherwise, the user has made a choice and we give them the right feedback
            let
                ( buttonMessage, buttonLabel ) =
                    if masteryThresholdReached model then
                        -- if mastery threshold is reached then we're done
                        -- question button is the "let's go back to Torus" button
                        ( MsgReturnToTorus
                        , "Return to Lesson"
                        )

                    else
                        -- if mastery threshold hasn't been reached then
                        -- question button is "give me the next question" button
                        ( MsgGetNewQuestion
                        , "Next Question"
                        )

                nextBtn =
                    Element.Input.button
                        [ Element.padding 10
                        , Element.Border.width 3
                        , Element.Border.rounded 6
                        , Element.Border.color (Element.rgb255 0 0 0)
                        , Element.Font.variant Element.Font.smallCaps
                        , Element.width (Element.fillPortion 1)
                        ]
                        { onPress = Just buttonMessage
                        , label = Element.el [ Element.centerX ] (Element.text buttonLabel)
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
                        [ Element.text userResponse.feedback ]
                    ]
                , Element.row
                    [ Element.width Element.fill ]
                    [ nextBtn ]
                ]


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


listOfRightAnswers : List QuestionResponse
listOfRightAnswers =
    let
        allRightAnswers : List String
        allRightAnswers =
            intervalMeasures ++ ratioMeasures

        correctAnswerFlag : Bool
        correctAnswerFlag =
            True

        responseFeedback : String
        responseFeedback =
            "That is correct!"
    in
    List.map (QuestionResponse correctAnswerFlag responseFeedback) allRightAnswers


listOfDistractors : List QuestionResponse
listOfDistractors =
    let
        allDistractors : List String
        allDistractors =
            nominalMeasures ++ ordinalMeasures

        correctAnswerFlag : Bool
        correctAnswerFlag =
            False

        responseFeedback : String
        responseFeedback =
            "This is not a good choice for the response variable. The response variable in a linear regression equation must be an interval or a ratio."
    in
    List.map (QuestionResponse correctAnswerFlag responseFeedback) allDistractors


randomListItem : List QuestionResponse -> Random.Generator ( Maybe QuestionResponse, List QuestionResponse )
randomListItem choiceList =
    -- Choose a random item from the list of question responses -> return a tuple with
    -- * the random choicewrapped in a Maybe
    -- * a list of everything else that wasn't chosen
    Random.List.choose choiceList


newListOfResponses : Int -> List QuestionResponse -> List QuestionResponse -> Random.Generator (List QuestionResponse)
newListOfResponses numberOfDistractors rightAnswersP wrongAnswersP =
    let
        makeAListOfThree :
            ( List QuestionResponse, List QuestionResponse ) -- list with one right answer and the right answers that weren't chosen
            -> ( List QuestionResponse, List QuestionResponse ) -- list with <x> wrong answers and the wrong answers that weren't chosen
            -> List QuestionResponse -- list with the chosen right and wrong answers
        makeAListOfThree ( r, rs ) ( d, ds ) =
            -- r is a list with one item: the right answer
            -- d is a list with more than one item: the distractors
            -- this function mushes them together into a single list
            r ++ d
    in
    -- call the "makeAListOfThree" function with two parameters
    Random.map2
        -- call this function (the next two lines provide the parameters for this function)
        makeAListOfThree
        -- pick one right answer (you also get a list of right answers that weren't chosen)
        (Random.List.choices 1 rightAnswersP)
        -- pick <x> distractors (you also get a list of distractors that weren't chosen)
        (Random.List.choices numberOfDistractors wrongAnswersP)


newQuestion : Random.Generator Question
newQuestion =
    let
        newStem =
            "Which of these options is a valid response variable for linear regression equation?"

        newImage =
            Nothing
    in
    Random.map
        (Question newStem newImage)
        (newListOfResponses 2 listOfRightAnswers listOfDistractors
            |> Random.andThen Random.List.shuffle
        )
