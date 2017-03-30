module Main exposing (..)

import Html exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)
import Matrix exposing (Matrix)
import Array exposing (Array)


type alias State =
    Bool


type alias Model =
    { matrix : Matrix State
    , nextMatrix : Matrix State
    , currentInstructions : List Instruction
    }


type Instruction
    = And
    | Or
    | Xor
    | Not
    | NotImplemented


rowToInstruction : Array State -> Instruction
rowToInstruction array =
    case Array.toList array of
        [ True, False, False, False, False, False, False, False ] ->
            And

        [ False, True, False, False, False, False, False, False ] ->
            Or

        [ True, True, False, False, False, False, False, False ] ->
            Xor

        [ False, False, False, False, False, False, False, True ] ->
            Not

        _ ->
            NotImplemented


allColumn : Int -> (State -> Bool) -> Matrix State -> Bool
allColumn colIndex eq matrix =
    case Matrix.getColumn colIndex matrix of
        Nothing ->
            False

        Just col ->
            Array.slice 2 (Array.length col) col
                |> Array.filter (eq >> not)
                |> Array.isEmpty


anyColumn : Int -> (State -> Bool) -> Matrix State -> Bool
anyColumn colIndex eq matrix =
    case Matrix.getColumn colIndex matrix of
        Nothing ->
            True

        Just col ->
            Array.slice 2 (Array.length col) col
                |> Array.filter (eq)
                |> Array.isEmpty
                |> not


singleColumn : Int -> (State -> Bool) -> Matrix State -> Bool
singleColumn colIndex eq matrix =
    case Matrix.getColumn colIndex matrix of
        Nothing ->
            False

        Just col ->
            Array.slice 2 (Array.length col) col
                |> Array.filter (eq)
                |> Array.length
                |> (==) 1


andMatrix : Matrix State -> Matrix State
andMatrix matrix =
    Matrix.indexedMap
        (\i j s ->
            if j < 2 then
                s
            else if allColumn i ((==) True) matrix then
                True
            else
                False
        )
        matrix


anyMatrix : Matrix State -> Matrix State
anyMatrix matrix =
    Matrix.indexedMap
        (\i j s ->
            if j < 2 then
                s
            else if anyColumn i ((==) True) matrix then
                True
            else
                False
        )
        matrix


onlyOneMatrix : Matrix State -> Matrix State
onlyOneMatrix matrix =
    Matrix.indexedMap
        (\i j s ->
            if j < 2 then
                s
            else if singleColumn i ((==) True) matrix then
                True
            else
                False
        )
        matrix


noneMatrix : Matrix State -> Matrix State
noneMatrix matrix =
    Matrix.indexedMap
        (\i j s ->
            if j < 2 then
                s
            else if allColumn i ((==) False) matrix then
                True
            else
                False
        )
        matrix


flipMatrix : Matrix State -> Matrix State
flipMatrix matrix =
    Matrix.indexedMap
        (\i j s ->
            if j < 2 then
                s
            else
                not s
        )
        matrix


nextState : Instruction -> Matrix State -> Matrix State
nextState instruction matrix =
    case instruction of
        NotImplemented ->
            matrix

        And ->
            andMatrix matrix

        Or ->
            anyMatrix matrix

        Xor ->
            onlyOneMatrix matrix

        Not ->
            flipMatrix matrix


type Msg
    = Flip Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Flip x y ->
            let
                nextMatrix =
                    Matrix.update y x not model.matrix

                firstRow =
                    Matrix.getRow 0 nextMatrix
                        |> Maybe.withDefault Array.empty

                secondRow =
                    Matrix.getRow 1 nextMatrix
                        |> Maybe.withDefault Array.empty

                firstInstruction =
                    rowToInstruction firstRow

                secondInstruction =
                    rowToInstruction secondRow
            in
                { model
                    | matrix =
                        nextMatrix
                    , nextMatrix =
                        nextState firstInstruction nextMatrix
                            |> nextState secondInstruction
                    , currentInstructions =
                        [ firstInstruction
                        , secondInstruction
                        ]
                }


view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.div
            [ style [ ( "padding-left", "170px" ), ( "padding-top", "50px" ) ] ]
            [ Html.text <| toString model.currentInstructions ]
        , viewBefore model
        , viewAfter model
        ]


viewAfter : Model -> Html.Html Msg
viewAfter model =
    Matrix.height model.nextMatrix
        |> List.range 0
        |> List.map (\i -> viewRow i model.nextMatrix)
        |> Html.div []


viewBefore : Model -> Html.Html Msg
viewBefore model =
    Matrix.height model.matrix
        |> List.range 0
        |> List.map (\i -> viewRow i model.matrix)
        |> Html.div []


viewRow : Int -> Matrix State -> Html.Html Msg
viewRow rowIndex matrix =
    Matrix.getRow rowIndex matrix
        |> Maybe.withDefault Array.empty
        |> Array.indexedMap (viewPunchhole rowIndex)
        |> Array.toList
        |> Html.ul [ style [ ( "width", "100%" ), ( "float", "left" ) ] ]


viewPunchhole : Int -> Int -> Bool -> Html.Html Msg
viewPunchhole rowIndex colIndex state =
    Html.li
        [ onClick (Flip rowIndex colIndex)
        , style
            [ ( "list-style-type", "none" )
            , ( "background-color"
              , if rowIndex > 1 then
                    if state then
                        "green"
                    else
                        "red"
                else
                    (if state then
                        "yellow"
                     else
                        "black"
                    )
              )
            , ( "width", "50px" )
            , ( "height", "50px" )
            , ( "float", "left" )
            ]
        ]
        []


initialModel : Model
initialModel =
    { matrix =
        Matrix.repeat 8 4 False
    , nextMatrix =
        Matrix.repeat 8 4 False
    , currentInstructions =
        []
    }


main : Program Never Model Msg
main =
    beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
