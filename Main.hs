{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as Set
import           System.Random

import           Miso
import           Miso.String (MisoString, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
import qualified Miso.Style as CSS

-- | miso-snake: heavily inspired by elm-snake
-- (https://github.com/theburningmonk/elm-snake)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

segmentDim = 15
cherryRadius = 7.5
(width, height) = (600, 600)

-- | Utility for periodic tick subscriptions
every :: Int -> (Double -> action) -> Sub action
every n f sink = liftIO . void . forkIO . forever $ do
  threadDelay n
  sink =<< f <$> now

main :: IO ()
main = run $ startComponent (component NotStarted startSnake viewModel)
  { subs = [ directionSub ([38,87],[40,83],[37,65],[39,68]) ArrowPress -- arrows + WASD
           , keyboardSub KeyboardPress
           , every 50000 Tick -- 50 ms
           ]
  }

-- | Model
data Direction
  = U
  | D
  | L
  | R
  deriving (Show, Eq)

type Position = (Double, Double)

pos :: Double -> Double -> Position
pos = (,)

data Snake = Snake
  { shead :: !Position
  , stail :: ![Position]
  , direction :: !Direction
  } deriving (Show, Eq)

type Cherry = Maybe Position

type Score = Int

data Model
  = NotStarted
  | Started
    { snake :: !Snake
    , cherry :: !Cherry
    , score :: !Score
    }
  deriving (Show, Eq)

-- | Msg that can trigger updates to Model
data Msg
  = Tick !Double
  | ArrowPress !Arrows
  | KeyboardPress !(Set.Set Int)
  | Spawn !Double !Position
  | NoOp

-- | Initial Snake
initSnake :: Snake
initSnake = Snake { shead = h, stail = t, direction = R }
  where
    h = (height/2, width/2)
    t = fmap (\n -> pos (-n*segmentDim) 0) [1..8]

-- | Render a model
rootBase :: [View a] -> View a
rootBase content = div_ [] [ svg_ [ height_ $ px height
                                  , width_ $ px width
                                  ] [ g_  [] (bg : content) ]
                           ]
  where
    bg = rect_ [ width_ (px width), height_ (px height) ] []


textStyle :: Attribute a
textStyle = CSS.style_ [ ("fill", "green")
                       , ("stroke", "green")
                       , ("text-anchor", "middle")
                       ]

px :: Show a => a -> MisoString
px e = ms $ show e ++ "px"

viewModel :: Model -> View Msg
viewModel NotStarted = rootBase [ text_ [ x_ $ px (width / 2)
                                        , y_ $ px (height / 2)
                                        , textStyle
                                        ] [ text "press SPACE to start" ]
                                ]
viewModel Started{..} =
  rootBase $ scoreLbl : maybe [] (\c -> [cherrySvg c]) cherry ++ snakeSvg snake
  where
    scoreLbl = text_ [ x_ $ px 10
                     , y_ $ px (height - 10)
                     , textStyle
                     ] [ text $ ms $ show score ]
    cherrySvg (x, y) = ellipse_ [ cx_ $ px x
                                , cy_ $ px y
                                , rx_ $ px cherryRadius
                                , ry_ $ px cherryRadius
                                , CSS.style_ [ ("fill", "red")
                                             , ("stroke", "black")
                                             , ("stroke-width", "2")
                                             ]
                                ] []
    snakeSvg Snake {..} = snakeLimb "white" shead : map (snakeLimb "yellow") stail
    snakeLimb color (x, y) = rect_ [ width_ $ px segmentDim
                                   , height_ $ px segmentDim
                                   , x_ $ px x
                                   , y_ $ px y
                                   , CSS.style_ [ ("fill", color)
                                                , ("stroke", "black")
                                                , ("stroke-width", "2")
                                                ]
                                   ] []

-- | Updates model, optionally introduces side effects
startSnake :: Msg -> Effect Model Msg
startSnake msg = do
  get >>= \case
    Started{} ->
      updateModel msg
    NotStarted ->
      case msg of
        KeyboardPress keys | Set.member 32 keys -> noEff (Started initSnake Nothing 0)
        _ -> noEff NotStarted
updateModel (ArrowPress arrs) = do
  model <- get
  let newDir = getNewDirection arrs $ direction (snake model)
      newSnake = (snake model) { direction = newDir }
  noEff $ model { snake = newSnake }
updateModel (Spawn chance (randX, randY)) = do
  model <- get
  case chance of
    _ | chance <= 0.1 ->
          put model { cherry = spawnCherry randX randY }
      | otherwise ->
          put model
updateModel (Tick _) = do
  model@Started{..} <- get
  let
    newHead = getNewSegment (shead snake) (direction snake)
    ateCherry = maybe False (isOverlap newHead) cherry
    newTail =
        if ateCherry then shead snake : stail snake
                     else shead snake : init (stail snake) -- partial!
    newSnake = snake { shead = newHead, stail = newTail }
    (newCherry, newScore) =
        if ateCherry then (Nothing, score + 1)
                     else (cherry, score)
    newModel = model { snake = newSnake, cherry = newCherry, score = newScore }
    gameOver = isGameOver newHead newTail

  if | gameOver          -> noEff NotStarted
     | cherry == Nothing -> newModel <# do
         [chance, xPos, yPos] <- liftIO $ replicateM 3 $ randomRIO (0, 1)
         return $ Spawn chance (xPos, yPos)
     | otherwise         -> noEff newModel
updateModel _ = pure ()

getNewDirection :: Arrows -> Direction -> Direction
getNewDirection (Arrows arrX arrY) dir
  | dir == U || dir == D =
    case arrX of
      -1 -> L
      1 -> R
      _ -> dir
  | otherwise =
    case arrY of
      -1 -> U
      1 -> D
      _ -> dir

getNewSegment :: Position -> Direction -> Position
getNewSegment (x, y) direction =
  case direction of
    U    -> pos x (y+segmentDim)
    D    -> pos x (y-segmentDim)
    L    -> pos (x-segmentDim) y
    R    -> pos (x+segmentDim) y

isGameOver :: Position -> [Position] -> Bool
isGameOver newHead@(x,y) newTail =
  elem newHead newTail   -- eat itself
  || x > width - segmentDim    -- hit right
  || y > height - segmentDim   -- hit bottom
  || x < 0                     -- hit top
  || y < 0                     -- hit left

spawnCherry :: Double -> Double -> Cherry
spawnCherry randW randH =
  let x = randW * (width - 2*cherryRadius) + cherryRadius
      y = randH * (height - 2*cherryRadius) + cherryRadius
  in Just $ pos x y

isOverlap :: Position -> Position -> Bool
isOverlap (snakeX, snakeY) (cherryX, cherryY) =
  let (xd, yd) = ( cherryX - snakeX - (segmentDim /2)
                 , cherryY - snakeY - (segmentDim / 2)
                 )
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (cherryRadius * 2)
