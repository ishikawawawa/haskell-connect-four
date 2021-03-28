module Main where

import Data.List
import Data.Function
import Data.Tuple
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Text.Read

type Pos = (Int, Int)
data Piece = Yellow | Red deriving (Show, Eq)
type Board = Map.Map Pos Piece

boundsCheck :: Pos -> Bool
boundsCheck (x, y) = x >= 0 && y >= 0 && x <= 6 && y <= 5

connectHorizontal :: Pos -> [Pos]
connectHorizontal (x, y) = filter boundsCheck [(x + offset, y) | offset <- [-3..3]]

connectVertical :: Pos -> [Pos]
connectVertical (x, y) = filter boundsCheck [(x, y + offset) | offset <- [-3..3]]

connectUpperRight :: Pos -> [Pos]
connectUpperRight (x, y) = filter boundsCheck [(x + offset, y + offset) | offset <- [0..3]]

connectLowerRight :: Pos -> [Pos]
connectLowerRight (x, y) = filter boundsCheck [(x + offset, y - offset) | offset <- [0..3]]

connectUpperLeft :: Pos -> [Pos]
connectUpperLeft (x, y) = filter boundsCheck [(x - offset, y - offset) | offset <- [0..3]]

connectLowerLeft :: Pos -> [Pos]
connectLowerLeft (x, y) = filter boundsCheck [(x - offset, y - offset) | offset <- [0..3]]

checkConnectFour :: Pos -> [Pos] -> Bool
checkConnectFour pos positions =
  4 `elem`
  map (\checkPos -> length $ intersect checkPos positions)
  [ connectHorizontal pos
  , connectVertical pos
  , connectUpperRight pos
  , connectLowerRight pos
  , connectUpperLeft pos
  , connectLowerLeft pos
  ]

canPutPiece :: [Pos] -> Pos -> Bool
canPutPiece board pos =
  boundsCheck pos &&
  not (pos `elem` board) &&
  if (snd pos /= 0)
     then (fst pos, snd pos - 1) `elem` board
     else True

canPutPositions :: [Pos] -> [Pos]
canPutPositions pos =
  filter (canPutPiece pos) [(x, y) | x <- [0..6], y <- [0..5]]

putPiece :: Board -> Piece -> Pos -> Board
putPiece board piece pos = Map.insert pos piece board

nextTurn :: Piece -> Piece
nextTurn t = case t of Yellow -> Red
                       Red -> Yellow

turn :: (Board, Piece) -> Pos -> ((Board, Piece), Bool)
turn (board, currentTurn) pos =
  ((updatedBoard, next), isWin)
  where
    updatedBoard = putPiece board currentTurn pos
    currentTurnPieces = Map.keys $ Map.filter (== currentTurn) updatedBoard
    next = nextTurn currentTurn
    isWin = pos `checkConnectFour` currentTurnPieces

resolvePos :: Board -> Int -> Maybe Pos
resolvePos board x = 
  case filter (\(canX, _) -> canX == x)$ canPutPositions $ Map.keys board of
    (x : xs) -> Just x
    _ -> Nothing

dispPiece :: Maybe Piece -> String
dispPiece piece =
  case piece of
    Just x | x == Yellow -> " 🐶 "
           | x == Red -> " 🐱 "
    Nothing -> " ❌ "

dispBoard :: Board -> [String]
dispBoard board =
  reverse $
  map (\line -> concat $
    map (\pos -> (++) " " . dispPiece $
      swap pos `Map.lookup` board)line) $
  groupBy ((==) `on` fst)
  [(x, y) | x <- [0..5], y <- [0..6]]

validateInput :: Board -> String -> Either String Pos
validateInput board input =
  case (readMaybe input :: Maybe Int) of
    Nothing -> Left "type number"
    Just x -> case resolvePos board (x-1) of
                Just pos -> Right pos
                Nothing -> Left "can't put"

getInput :: Board -> IO Pos
getInput board = do
  input <- getLine
  case validateInput board input of
    Right pos -> pure pos
    Left msg -> do
      putStrLn $ "<< ERROR : " ++ msg
      getInput board

gameLoop :: (Board, Piece) -> Bool -> IO ()
gameLoop (board, currentTurn) isWin = do
  dispBoard board `forM_` putStrLn
  putStrLn "   1    2    3    4    5    6    7"
  if (isWin) then
    putStrLn $ "<< WINNER : " ++ case (nextTurn currentTurn) of
                                Yellow -> "DOG"
                                Red -> "Cat"
  else do
    resolvedPos <- getInput board
    let (next, win) = turn (board, currentTurn) resolvedPos
    putStrLn ""
    gameLoop next win

main :: IO ()
main = do
  let initialBoard = Map.fromList []
  let first = Red
  gameLoop (initialBoard, first) False

