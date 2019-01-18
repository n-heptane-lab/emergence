module Main where

import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, newBroadcastTChanIO, dupTChan, readTChan, writeTChan, tryReadTChan)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad (forever)
import Data.Function (on)
import Data.List (minimumBy, sortBy)
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Vector ((!), Vector)
import qualified Data.Vector as Vector
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq, ViewL(..), (|>), viewl)
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import Graphics.Gloss hiding (Vector)
-- import Graphics.Gloss.Interface.Pure.Game hiding (Vector)
import Graphics.Gloss.Interface.IO.Animate (Controller(..), animateIO, animateFixedIO)
import Linear.Algebra (mult)
import Linear.Metric (dot)
import Linear.Vector (sumV, (^+^),(^*) )
import Linear.Matrix ((!*))
import System.Environment
import System.Exit (exitSuccess)
import System.Random (newStdGen, randomIO, randomRIO, randomRs)

windowRadius :: Int
windowRadius = 800

window :: Display
window = InWindow "emergence" (windowRadius,windowRadius) (10,10)

background :: Color
background = white

-- handleInput :: Event -> World -> World
-- handleInput ev world = world

data Announcement = Announcement
  { position     :: (Float, Float)
  , currentColor :: Color
  }
  deriving (Show)

type Announcements = IntMap Announcement

distance :: Announcement -> Announcement -> Float
distance a1 a2 =
  let (x1, y1) = position a1
      (x2, y2) = position a2
  in (((x1 - x2) ** 2) + ((y1 - y2) ** 2)) ** 0.5

nearestNeighbor :: Int -> Announcement -> Announcements -> Maybe (Int, Announcement)
nearestNeighbor yourId you announcements =
  let anns = IntMap.toList announcements
  in case anns of
    [] -> Nothing
    [x] -> Nothing
    _  -> Just $ minimumBy (compare `on` (distance you . snd)) (filter ((/=) yourId . fst) anns)

neighborsByDistance :: Int -> Announcement -> Announcements -> [(Int, Announcement)]
neighborsByDistance yourId you announcements =
  let anns = filter ((/=) yourId . fst)  $ IntMap.toList announcements
  in sortBy (compare `on` (distance you . snd)) anns
{-     
  in case anns of
    [] -> Nothing
    [x] -> Nothing
    _  -> Just $ minimumBy (compare `on` (distance you . snd)) (filter ((/=) yourId . fst) anns)
-}
updateAnnouncements :: TChan (Int, Announcement) -> Announcements -> STM Announcements
updateAnnouncements c announcements =
  do ma <- tryReadTChan c
     case ma of
       Nothing      -> pure announcements
       (Just (i,a)) -> updateAnnouncements c (IntMap.insert i a announcements)

differentColor :: Color -> Color -> Color
differentColor c1 c2
  | (c1 /= c2) && (c1 /= black)    = c1
  | c1 == red   = blue
  | c1 == blue  = green
  | c1 == green = red
  | otherwise = red

differentColor' :: () -> Int -> Announcement -> Announcements -> IO (Maybe Announcement, ())
differentColor' () myId me announcements =
  do let mnearest = nearestNeighbor myId me announcements
         color' =
           case mnearest of
             Nothing -> currentColor me -- atomically $ writeTChan c (ident, (Announcement (x,y) colr))
             (Just nearest) -> (differentColor (currentColor me) (currentColor $ snd nearest)) -- atomically $ writeTChan c (ident, (Announcement (x,y) (differentColor colr (currentColor $ snd nearest))))
     (x', y') <- randomWalk (position me)
--     (x', y') <- pure $ position me
     pure (Just $ Announcement (x', y') color', ())

differentColor2 :: Color -> Color -> Color -> Color
differentColor2 c1 c2 c3
  | (c1 /= c2)  && (c1 /= c3) && (c1 /= black)                 = c1
  | (c2 == red) && (c3 == red) = green
  | (c2 == red) && (c3 == blue) = green
  | (c2 == red) && (c3 == green) = blue

  | (c2 == blue) && (c3 == red) = green
  | (c2 == blue) && (c3 == green) = red
  | (c2 == blue) && (c3 == blue) = red

  | (c2 == green) && (c3 == red) = blue
  | (c2 == green) && (c3 == green) = blue
  | (c2 == green) && (c3 == blue) = red
  | otherwise = red
--   | otherwise = error $ show (c1, c2, c3)

differentColor2' :: () -> Int -> Announcement -> Announcements -> IO (Maybe Announcement, ())
differentColor2' () myId me announcements =
  do let nearest = take 2 $ neighborsByDistance myId me announcements
         color' =
           case nearest of
             [nearest1, nearest2] -> (differentColor2 (currentColor me) (currentColor $ snd nearest1) (currentColor $ snd nearest2)) -- atomically $ writeTChan c (ident, (Announcement (x,y) (differentColor colr (currentColor $ snd nearest))))
             _ -> currentColor me
{-
           case mnearest of
             Nothing -> currentColor me -- atomically $ writeTChan c (ident, (Announcement (x,y) colr))
             (Just nearest) -> (differentColor (currentColor me) (currentColor $ snd nearest)) -- atomically $ writeTChan c (ident, (Announcement (x,y) (differentColor colr (currentColor $ snd nearest))))
-}
     (x', y') <- randomWalk (position me)
--     (x', y') <- pure $ position me 
     pure (Just $ Announcement (x', y') color', ())

pulsar :: (Float, Float) -> Int -> Announcement -> Announcements -> IO (Maybe Announcement, (Float, Float))
pulsar (rate, amount) myid me _ =
  do let (rate', amount') = if (amount + rate <= 1) && (amount + rate >= 0)
                         then (rate, amount + rate)
                         else ((-rate), amount - rate)
     (x', y') <- randomWalk (position me)
     pure $ (Just $ Announcement (x', y') (makeColor amount' 0 0 1), (rate', amount'))

data Follower
  = FindSomeone
  | Watch
      { startTime  :: UTCTime
      , watching   :: Int
      , actions    :: Seq (NominalDiffTime, Color)
      }
  | Replay
      { startTime :: UTCTime
      , actions   :: Seq (NominalDiffTime, Color)
      }
{-
 { lastTime :: UTCTime
-- , delay :: 
 }
 -}
follower :: Follower -> Int -> Announcement -> Announcements -> IO (Maybe Announcement, Follower)
follower follower myId me announcements =
  case follower of
    FindSomeone ->
      do let neighbors = neighborsByDistance myId me announcements
         now <- getCurrentTime
         i <- randomRIO (0,2)
         if length neighbors <= 2
           then pure (Just me, FindSomeone)
           else do let (nearestId, (Announcement _ color)) = neighbors!!i
                   putStrLn $ "myId = " ++ (show myId) ++ " Found = " ++ show nearestId
                   pure (Just me, Watch now nearestId (Seq.singleton (0, color)))

    (Watch startTime watchingId actions) ->
      do now <- getCurrentTime
         let (Just (Announcement _ color)) = IntMap.lookup watchingId announcements
             delta = now `diffUTCTime` startTime
             actions' = actions |> (delta, color)
         pos <- randomWalk (position me)
         if delta > 2
           then do putStrLn $ "myId = " ++ (show myId) ++ " Replaying = " ++ show watchingId
                   pure (Just $ me { position = pos }, Replay now actions')
           else do -- putStrLn $ "delta = " ++ show delta
                   pure (Just $ me { position = pos }, Watch startTime watchingId actions')
    (Replay startTime actions) ->
      do now <- getCurrentTime
         pos <- randomWalk (position me)
         let delta = now `diffUTCTime` startTime
         case viewl actions of
               EmptyL -> pure (Nothing, FindSomeone)
               ((d, c) :< as) ->
                 if (d < delta)
                 then pure (Just (me { currentColor = c, position = pos }), Replay startTime as)
                 else pure (Just $ me { position = pos }, Replay startTime actions)

mkNode :: (st -> Int -> Announcement -> Announcements -> IO (Maybe Announcement, st)) -> TChan (Int, Announcement) -> st -> Int -> IO ThreadId
mkNode handler bc initSt ident =
  do c <- atomically $ dupTChan bc
     forkIO $ let loop st me@(Announcement (x,y) colr) announcements' =
                    do announcements <- atomically $ updateAnnouncements c announcements'
                       (mann, st') <- handler st ident me announcements
{-
                       let mnearest = nearestNeighbor me announcements
                       case mnearest of
                         Nothing -> atomically $ writeTChan c (ident, (Announcement (x,y) colr))
                         (Just nearest) -> atomically $ writeTChan c (ident, (Announcement (x,y) (differentColor colr (currentColor $ snd nearest))))
                       (x', y') <- randomWalk (x, y)
-}
                       ann' <- case mann of
                         (Just ann') -> do atomically $ writeTChan c (ident, ann')
                                           pure ann'
                         Nothing -> pure me
                       threadDelay 80000
                       loop st' ann' announcements
              in do -- ci <- randomRIO (0,2)
                    x <- randomRIO (-60,60)
                    y <- randomRIO (-60,60)
                    loop initSt (Announcement (x,y) black {- ([red, blue, green]!!ci) -}) IntMap.empty

randomWalk :: (Float, Float) -> IO (Float, Float)
randomWalk (x,y) =
  do xDelta <- randomRIO (-2, 2)
     yDelta <- randomRIO (-2, 2)
     let x' = if ((x + xDelta) >= (fromIntegral windowRadius)) || ((x + xDelta) <= (fromIntegral windowRadius))
             then (x - xDelta)
             else (x + xDelta)
         y' = if ((y + yDelta) >= (fromIntegral windowRadius)) || ((y + yDelta) <= (fromIntegral windowRadius))
             then (y - yDelta)
             else (y + yDelta)
     pure (x', y')

readTChans :: TChan a -> STM [a]
readTChans c =
  do ma <- tryReadTChan c
     case ma of
       Nothing -> pure []
       (Just a) ->
         do as <- readTChans c
            pure (a:as)

main :: IO ()
main = forever $ do
  do bchan <- newBroadcastTChanIO
     chan <- atomically $ dupTChan bchan
     state <- atomically $ newTVar IntMap.empty
     mkNode differentColor2' bchan () 0 ; mapM_ (mkNode follower bchan FindSomeone) [1..30]
--     mapM_ (mkNode differentColor2' bchan ()) [0..50]
--     mapM_ (mkNode differentColor' bchan ()) [0..50]
--     mapM_ (mkNode differentColor2' bchan ()) [0..10]
--     mapM_ (mkNode pulsar bchan (0.05, 0.2)) [0..0]

     animateFixedIO window background (updatePicture chan state) callback
  where
    renderNode :: Announcement -> Picture
    renderNode (Announcement (x,y) c) =
      translate x y $
       color c $
        circleSolid 10
    updatePicture :: TChan (Int, Announcement) -> TVar (IntMap Announcement) -> Float -> IO Picture
    updatePicture chan stateV now =
      do st <- atomically $ do vals <- readTChans chan
                               state <- readTVar stateV
                               let state' = IntMap.union (IntMap.fromList vals) state
                               writeTVar stateV state'
                               pure state'
         pure $ pictures (map renderNode (IntMap.elems st) )

    callback (Controller r _) =
      do r
         putStrLn "callback"


{-
-- linear classification
--

data Neuron = Neuron
  { weights            :: Vector Float
  , bias               :: Float
  , activationFunction :: Float -> Float
  }

type TrainingSet = [(Vector Float, Float)]

mkPerceptron :: Int -> IO Neuron
mkPerceptron numInputs =
  do gen <- newStdGen
     let weights' = take numInputs $ randomRs (-0.1, 0.1) gen
         bias = 0
     return $ Neuron (Vector.fromList weights') bias heavisideStep

heavisideStep :: Float -> Float
heavisideStep x
  | x < 0     = (-1)
  | otherwise = 1

evalNeuron :: Neuron -> Vector Float -> Float
evalNeuron (Neuron weights bias activationFunction) inputs =
  activationFunction ((dot inputs weights) + bias)

train :: (Vector Float, Float) -> Neuron -> Neuron
train (input, target) neuron =
  let lr = 1 -- 0.005
      guess = evalNeuron neuron input
      err   = target - guess
      weights' = (weights neuron) ^+^ (input ^* (err * lr))
      bias' = (bias neuron) + (err * lr)
  in neuron { weights = weights', bias = bias' }

p0 = Neuron (Vector.fromList [6, 2, 2]) (-3) heavisideStep
p1 = Neuron (Vector.fromList [6, 2, 2]) 5 heavisideStep

inputs0 :: Vector Float
inputs0 = Vector.fromList [ 0, 1, 1]

windowRadius = 800

window :: Display
window = InWindow "bad idea" (windowRadius,windowRadius) (10,10)

-- convert from (-1,1) to window coord
toCoord :: Float -> Float
toCoord n = n * ( ((fromIntegral windowRadius) - 100) / 2)

background :: Color
background = white

markerRadius = 5

drawX :: Picture
drawX =
--   color c $
--   translate x y $
    pictures [ line [(-markerRadius, -markerRadius), (markerRadius, markerRadius)]
             , line [(-markerRadius, markerRadius), (markerRadius, -markerRadius)]
             ]

drawO :: Picture
drawO =
--  color c $
--   translate x y $
    circle markerRadius

render :: World -> Picture
render (World neuron trainingData index _) =
  pictures (box : center : xLabel : yLabel : renderNeuron neuron : [ renderTd td i | (td, i) <- zip trainingData [0..] ])
  where
    center :: Picture
    center = color black $ pictures $ [ line [ (-5, 0), (5, 0) ], line [ (0, -5), (0, 5) ] ]
    xLabel = translate (toCoord (-0.1)) (toCoord (-1.1)) $ scale 0.1 0.1 $ Text "Hotness"
    yLabel = translate (toCoord (-1.1)) (toCoord (-0.1)) $ rotate (-90) $ scale 0.1 0.1 $ Text "$$$"
    box =  line [ (toCoord (-1), toCoord (-1))
                , (toCoord (1), toCoord (-1))
                , (toCoord (1), toCoord 1)
                , (toCoord (-1), toCoord 1)
                , (toCoord (-1), toCoord (-1))
                ]
    renderNeuron :: Neuron -> Picture
    renderNeuron n =
      let m = (weights n ! 0) / (weights n ! 1)
          x = 1
          y x' = (- ((bias n) / (weights n ! 1))) - m * x'
      in line [ (toCoord (-x), toCoord $ y (-x)), (toCoord x, toCoord $ y x) ]
--         line [ (toCoord (-1), toCoord (-1)), (toCoord 1, toCoord 1) ]
    renderTd :: (Vector Float, Float) -> Int -> Picture
    renderTd (inputs, expected) i =
      let guessed = evalNeuron neuron inputs
          c = if (i == index) then blue else (if guessed == expected then green else red)
          xc = toCoord (inputs ! 0)
          yc = toCoord (inputs ! 1)
      in color c $
          translate xc yc $
            if expected >= 0
               then drawO
               else drawX

fps :: Int
fps = 5

data World = World
 { neuron :: Neuron
 , trainingData :: [(Vector Float, Float)]
 , index :: Int
 , isTraining :: Bool
 }

initialState :: IO World
initialState =
  do n <- mkPerceptron 2
     g <- newStdGen
     g' <- newStdGen
     let xs = randomRs (-1, 1::Float) g
         ys = randomRs (-1, 1::Float) g'
         td = take 150 $ zipWith (\x y -> (Vector.fromList [x, y], if (y > -(0.5*x) + 0.3) then 1 else -1)) xs ys
     pure $ World n td 0 False

andState :: IO World
andState =
    do n <- mkPerceptron 2
       let trainingState = [ (Vector.fromList [1,1], 1)
                           , (Vector.fromList [1, (-1)], (-1))
                           , (Vector.fromList [(-1), (-1)], (-1))
                           , (Vector.fromList [(-1), (1)], (-1))
                           ]
       pure $ World n trainingState 0 False

orState :: IO World
orState =
    do n <- mkPerceptron 2
       let trainingState = [ (Vector.fromList [1   ,1    ],   1)
                           , (Vector.fromList [1   , (-1)],  (1))
                           , (Vector.fromList [(-1), (-1)], (-1))
                           , (Vector.fromList [(-1), (1) ], (1))
                           ]
       pure $ World n trainingState 0 False

xorState :: IO World
xorState =
    do n <- mkPerceptron 2
       let trainingState = [ (Vector.fromList [1   ,1    ],   (-1))
                           , (Vector.fromList [1   , (-1)],  (1))
                           , (Vector.fromList [(-1), (-1)], (-1))
                           , (Vector.fromList [(-1), (1) ], (1))
                           ]
       pure $ World n trainingState 0 False

handleInput :: Event -> World -> World
handleInput event world =
  case event of
    (EventKey _ Up _ _) ->
      world { isTraining = not (isTraining world) }
    _ -> world

isTrainingComplete :: Neuron -> TrainingSet -> Bool
isTrainingComplete n td =
  all (\(inputs, expected) -> evalNeuron n inputs == expected) td

update :: Float -> World -> World
update delta w =
  if isTraining w then
    let n' = train ((trainingData w)!!(index w)) (neuron w)
    in trace (show $ (bias n', weights n')) $ w { neuron = n'
         , index = ((index w) + 1) `mod` (length (trainingData w))
         , isTraining = not (isTrainingComplete n' (trainingData w))
         }
  else w


main :: IO ()
main =
  do print $ evalNeuron p0 inputs0
     print $ evalNeuron p1 inputs0
     world <- initialState
     play window background fps world render handleInput update

{-
  let n' = foldr train (neuron w) (trainingData w)
  in w { neuron = n' }
-}
{-
p1 = Perceptron (Vector.fromList [3, 1, 1])

-- Vector.fromList[ (Vector.fromList  [6,2,2]), (Vector.fromList  [3,1,1])] !* Vector.fromList [1, 1, 1]

-}
{-
data Layer = Layer
 { perceptrons :: Vector Neuron
 }

type Brain = Vector Layer
-}
-}
