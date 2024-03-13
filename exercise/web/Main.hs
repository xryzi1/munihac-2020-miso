{-# language NamedFieldPuns    #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards   #-}
{-# language OverloadedLists   #-}

module Main where

import Data.Foldable (asum)
import Data.List (transpose, (!!))
import Data.Map ()
import Data.Maybe (isJust)

import Miso
import Miso.String (MisoString, toMisoString)
import GHC.RTS.Flags (DebugFlags(squeeze))

main :: IO ()
main = startApp App { .. }
  where
    initialAction = None
    model         = initModel
    update        = updateModel
    view          = viewModel
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing
    logLevel      = Off

data Player = Player {
    square :: Square
  , name   :: String
  }
  deriving (Show, Eq)

john, jacob :: Player
john = Player { square = O, name = "John"}
jacob = Player { square = X, name = "Jacob" }

initModel :: Model
initModel = Model {
      grid = aGrid
    , player1 = john
    , player2 = jacob
    , isPlayer1Playing = True
    , winner = Nothing
    }

data Square
  = X | O
  deriving (Show, Eq)

type Grid = [[Maybe Square]]

emptyGrid, aGrid :: Grid
emptyGrid = replicate 3 (replicate 3 Nothing)
aGrid = [ [ Just X,  Nothing, Nothing ]
        , [ Nothing, Just O,  Nothing ]
        , replicate 3 Nothing ]

hasWinner :: Grid -> Maybe Square
hasWinner g
  = asum (map isWinnerRow thingToCheck)
  where
    thingToCheck
      = g ++ transpose g
          ++ [ [g !! 0 !! 0, g !! 1 !! 1, g !! 2 !! 2]
             , [g !! 0 !! 2, g !! 1 !! 1, g !! 2 !! 0] ]
    isWinnerRow :: [Maybe Square] -> Maybe Square
    isWinnerRow row
      | all isJust row, all (== head row) row
      = head row
      | otherwise
      = Nothing

data Model
  = Model {
    grid :: Grid
  , player1 :: Player
  , player2 :: Player
  , isPlayer1Playing :: Bool
  , winner :: Maybe Square
  }
  deriving (Show, Eq)

data Action
  = None
  | ClickSquare Int Int
  | NewGame
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel None m
  = noEff m
updateModel (ClickSquare rowId colId) m@(Model grid player1 player2 isPlayer1Playing winner)
  = case winner of
         Just _ -> noEff m
         Nothing ->
          case grid !! rowId !! colId of
              (Just _) -> noEff m -- Occupied
              Nothing -> pure $
                Model newGrid player1 player2 (not isPlayer1Playing) (hasWinner newGrid)
                where
                  (beforeRow, row:afterRow) = splitAt rowId grid
                  (beforeCol, _:afterCol) = splitAt colId row
                  newGrid = beforeRow ++ [beforeCol ++ Just currentPlayerSquare : afterCol] ++ afterRow
                  currentPlayerSquare :: Square
                  currentPlayerSquare =
                    if isPlayer1Playing
                    then square player1
                    else square player2

updateModel (NewGame) m
  = pure $ Model emptyGrid (player1 m) (player2 m) True Nothing


bootstrapUrl :: MisoString
bootstrapUrl = "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"

viewModel :: Model -> View Action
viewModel m
  = div_ [ class_ "container"]
         [ headerView
         , newGameView m
         , contentView m
         -- , statsView m
         , link_ [ rel_ "stylesheet"
                 , href_ bootstrapUrl ] ]

headerView :: View Action
headerView
  = nav_ [ class_ "navbar navbar-dark bg-dark"]
         [ h2_ [ class_ "bd-title text-light" ]
               [ text "Tic-Tac-Toe "
               , span_ [ class_ "badge badge-warning"]
                       [ text "in miso!"] ] ]

newGameView :: Model -> View Action
newGameView m
  = nav_ [ class_ "navbar navbar-light bg-light"]
         [ form_ [ class_ "form-inline" ]
                 [ input_  [ class_       "form-control mr-sm-2"
                           , type_        "text"
                           , value_       (toMisoString $ name $ player1 m)
                           , onChange  undefined
                           , placeholder_ "Plyer 1" ]
                 , select_ [ class_       "custom-select"
                           , style_ [("margin-right", "15px")] ]
                           (flip map ["O", "X"] $ \option ->
                              option_ [ ] [ text option])
                 , input_  [ class_       "form-control mr-sm-2"
                           , type_        "text"
                           , value_       (toMisoString $ name $ player2 m)
                           , onChange  undefined
                           , placeholder_ "Plyer 2" ]
                 , select_ [ class_       "custom-select"
                           , style_ [("margin-right", "15px")] ]
                           (flip map ["O", "X"] $ \option ->
                              option_ [ ] [ text option])
                 , button_ [ class_       "btn btn-outline-warning"
                           , type_        "button"
                           , onClick      NewGame
                           , disabled_    (gameNotEnded m) ]
                           [ text "New game" ] ] ]

gameNotEnded :: Model -> Bool
gameNotEnded m
 | (== Nothing) (winner m) && freeMove (grid m) = True
 | otherwise = False
  where freeMove grid = Nothing `elem` concat grid

contentView :: Model -> View Action
contentView m@Model { .. }
  = div_ [ style_ [("margin", "20px")]]
         [ gridView m
         , case winner of
                Nothing -> text ""
                (Just s) ->  alertView (toMisoString ("Winner is " ++ show s))
         ]


gridView :: Model -> View Action
gridView (Model grid p1 p2 isP1Playing winner)
  = div_ [ style_ [("margin", "20px")]]
         [ div_ [ class_ "row justify-content-around align-items-center" ]
                [ h3_ [ class_ (if isP1Playing then "text-primary" else "") ] [ text (toMisoString (name p1))]
                , div_ [ style_ [("display", "inline-block")] ]
                       [ div_ [ style_ [ ("display", "grid")
                                       , ("grid-template-rows", "1fr 1fr 1fr")
                                       , ("grid-template-columns", "1fr 1fr 1fr")
                                       , ("grid-gap", "2px") ] ]
                               ( flip concatMap (zip [0 ..] grid) $ \(rowId, row) ->
                                   flip map (zip [0 ..] row) $ uncurry (cell rowId) )]
                , h3_ [ class_ (if isP1Playing then "" else "text-primary")] [ text (toMisoString (name p2))] ] ]
  where
    cell :: Int -> Int -> Maybe Square -> View Action
    cell rowId colId square
      = div_ [ style_ [("width", "100px"), ("height", "100px")] ]
             [ button_ [ type_  "button"
                       , style_  [ ("width", "100%"), ("height", "100%")
                                 , ("font-size", "xxx-large") ]
                       , class_  "btn btn-outline-secondary"
                       , onClick (ClickSquare rowId colId) ]
                       [ text (showSquare square)]]
    showSquare :: Maybe Square -> MisoString
    showSquare (Just square) = (toMisoString . show) square
    showSquare _  = "."

alertView :: MisoString -> View Action
alertView v
  = div_ [ class_ "alert alert-warning"
         , style_ [("text-align", "center")] ]
         [ h4_ [ class_ "alert-heading" ]
               [ text v ] ]

fakeStats :: [MisoString]
fakeStats
  = [ "A - B, won by A in 3 moves"
    , "Quijote - Sancho, won by Sancho in 5 moves" ]

statsView :: Model -> View Action
statsView _
  = div_ [ class_ "row justify-content-around align-items-center"
         , style_ [("margin-bottom", "20px")] ]
         [ ul_ [ class_ "list-group"]
               ( flip map fakeStats $ \elt ->
                   ul_ [ class_ "list-group-item" ] [ text elt ] ) ]