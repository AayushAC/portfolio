module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Data.Maybe (maybeToList)

-- | Configuration
windowSize, cellSize :: Int
windowSize = 600  -- Increased window size
cellSize   = 20

gridW, gridH :: Int
gridW = windowSize `div` cellSize
gridH = windowSize `div` cellSize

type Pos = (Int, Int)
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq)
data Mode      = Single | Multi deriving (Eq)
data GamePhase = Menu | Playing | Over deriving (Eq)

-- | Each snake's state
data Snake = Snake
  { segments :: [Pos]
  , dir      :: Direction
  , alive    :: Bool
  }

-- | Full game state
data GameState = GameState
  { phase    :: GamePhase
  , mode     :: Mode
  , s1       :: Snake
  , s2       :: Maybe Snake
  , food     :: Pos
  , gen      :: StdGen
  , score1   :: Int
  , score2   :: Int
  , high1    :: Int
  , high2    :: Int
  }

-- | Build initial snake of length 3
initSnake :: Pos -> Direction -> Snake
initSnake start d = Snake
  { segments = take 3 $ iterate (step (opposite d)) start
  , dir      = d
  , alive    = True
  }
  where
    step DirUp    (x,y) = (x,   y+1)
    step DirDown  (x,y) = (x,   y-1)
    step DirLeft  (x,y) = (x-1, y)
    step DirRight (x,y) = (x+1, y)
    opposite DirUp    = DirDown
    opposite DirDown  = DirUp
    opposite DirLeft  = DirRight
    opposite DirRight = DirLeft

-- | Randomly place food
placeFood :: [Pos] -> StdGen -> (Pos, StdGen)
placeFood occ g =
  let (x,g1) = randomR (0, gridW - 1) g
      (y,g2) = randomR (0, gridH - 1) g1
  in if (x,y) `elem` occ
     then placeFood occ g2
     else ((x,y), g2)

-- | Initialize the menu state
initialMenu :: StdGen -> GameState
initialMenu g = GameState Menu Single dummySnake Nothing dummyPos g 0 0 0 0
  where dummySnake = initSnake (0,0) DirRight
        dummyPos = (0,0)

-- | Initialize the game state
initGame :: StdGen -> Mode -> GameState
initGame g m =
  let g'        = snd $ next g
      snake1    = initSnake (gridW `div` 4, gridH `div` 2) DirRight
      snake2    = if m == Multi
                  then Just (initSnake (3*gridW `div` 4, gridH `div` 2) DirLeft)
                  else Nothing
      occupied  = segments snake1 ++ maybe [] segments snake2
      (f, g'')  = placeFood occupied g'
  in GameState Playing m snake1 snake2 f g'' 0 0 0 0

-- | Convert grid to screen coordinates
toScreen :: Pos -> (Float, Float)
toScreen (x,y) =
  ( fromIntegral x * sz - fromIntegral windowSize/2 + sz/2
  , fromIntegral y * sz - fromIntegral windowSize/2 + sz/2
  )
  where sz = fromIntegral cellSize

-- | Render the game
draw :: GameState -> Picture
draw gs = case phase gs of
  Menu    -> drawMenu
  Playing -> drawPlaying gs
  Over    -> drawGameOver gs

-- | Draw the menu screen
drawMenu :: Picture
drawMenu = pictures
  [ color white $ translate (-220) 100 $ scale 0.3 0.3 $ text "SNAKE GAME"
  , color white $ translate (-220) 0 $ scale 0.2 0.2 $ text "Press '1' for Single Player"
  , color white $ translate (-220) (-50) $ scale 0.2 0.2 $ text "Press '2' for Multiplayer"
  , color white $ translate (-220) (-150) $ scale 0.15 0.15 $ text "Controls:"
  , color white $ translate (-220) (-180) $ scale 0.15 0.15 $ text "Player 1: Arrow Keys"
  , color white $ translate (-220) (-210) $ scale 0.15 0.15 $ text "Player 2: WASD"
  , color white $ translate (-220) (-240) $ scale 0.15 0.15 $ text "Restart: R"
  ]

-- | Draw the playing screen
drawPlaying :: GameState -> Picture
drawPlaying gs = pictures $
  drawSnake (s1 gs) green ++
  maybe [] (`drawSnake` blue) (s2 gs) ++
  [ foodPic
  , drawScores
  ]
  where
    sz = fromIntegral cellSize
    drawSnake sn col = [ color col $ uncurry translate (toScreen p) $ rectangleSolid sz sz
                        | p <- segments sn
                        ]
    foodPic = color red $ uncurry translate (toScreen $ food gs) $ rectangleSolid sz sz
    drawScores = color white $ translate (-fromIntegral windowSize/2 + 10)
                          (fromIntegral windowSize/2 - 20) $
                scale 0.15 0.15 $
                text $ base ++ extra
    base  = "P1: " ++ show (score1 gs)
    extra = if mode gs == Multi then "  P2: " ++ show (score2 gs) else ""

-- | Draw the game over screen
drawGameOver :: GameState -> Picture
drawGameOver gs = pictures
  [ color white $ translate (-220) 50 $ scale 0.3 0.3 $ text "GAME OVER"
  , color white $ translate (-220) (-10) $ scale 0.2 0.2 $ text result
  , color white $ translate (-220) (-60) $ scale 0.2 0.2 $ text scoreText
  , color white $ translate (-220) (-110) $ scale 0.2 0.2 $ text "Press R to restart"
  , color white $ translate (-220) (-160) $ scale 0.2 0.2 $ text "Press M for menu"
  ]
  where
    alive1 = alive (s1 gs)
    alive2 = maybe False alive (s2 gs)
    result = case mode gs of
      Single -> if score1 gs > high1 gs then "New High Score!" else "Game Over!"
      Multi  -> case (alive1, alive2) of
                  (True, False) -> "Player 1 Wins!"
                  (False, True) -> "Player 2 Wins!"
                  _             -> "Draw!"
    scoreText = case mode gs of
      Single -> "Score: " ++ show (score1 gs)
      Multi  -> "P1: " ++ show (score1 gs) ++ "  P2: " ++ show (score2 gs)

-- | Handle input
event :: Event -> GameState -> GameState
event e gs = case phase gs of
  Menu    -> handleMenuEvent e gs
  Playing -> handlePlayEvent e gs
  Over    -> handleOverEvent e gs

-- | Handle menu input
handleMenuEvent :: Event -> GameState -> GameState
handleMenuEvent (EventKey (Char c) Down _ _) gs = case c of
  '1' -> initGame (gen gs) Single
  '2' -> initGame (gen gs) Multi
  _   -> gs
handleMenuEvent _ gs = gs

-- | Handle playing input
handlePlayEvent :: Event -> GameState -> GameState
-- Arrow keys for P1
handlePlayEvent (EventKey (SpecialKey key) Down _ _) gs = case key of
  KeyUp    -> turn True  DirUp   gs
  KeyDown  -> turn True  DirDown gs
  KeyLeft  -> turn True  DirLeft gs
  KeyRight -> turn True  DirRight gs
  _        -> gs
-- WASD for P2
handlePlayEvent (EventKey (Char c) Down _ _) gs = case c of
  'w' -> turn False DirUp   gs
  's' -> turn False DirDown gs
  'a' -> turn False DirLeft gs
  'd' -> turn False DirRight gs
  _   -> gs
handlePlayEvent _ gs = gs

-- | Handle game over input
handleOverEvent :: Event -> GameState -> GameState
handleOverEvent (EventKey (Char c) Down _ _) gs = case c of
  'r' -> initGame (gen gs) (mode gs)
  'm' -> initialMenu (gen gs)
  _   -> gs
handleOverEvent _ gs = gs

-- | Change direction, preventing 180° reversal
turn :: Bool -> Direction -> GameState -> GameState
turn isP1 d gs
  | isP1      = gs { s1 = updateDir (s1 gs) }
  | otherwise = gs { s2 = fmap updateDir (s2 gs) }
  where
    updateDir sn = if opposite (dir sn) d then sn else sn { dir = d }
    opposite DirUp DirDown   = True
    opposite DirDown DirUp   = True
    opposite DirLeft DirRight = True
    opposite DirRight DirLeft = True
    opposite _ _             = False

-- | Per-tick update
update :: Float -> GameState -> GameState
update _ gs | phase gs /= Playing = gs
update _ gs =
  let -- Move each snake
      moved1 = moveSnake (s1 gs) (food gs)
      moved2 = fmap (`moveSnake` food gs) (s2 gs)
      
      -- Collision & eating
      (a1, e1) = checkSnake moved1 (maybe [] segments moved2) (food gs)
      (a2, e2) = maybe (False, False)
                  (\sn -> checkSnake sn (segments moved1) (food gs))
                  moved2
      
      -- Update scores
      sc1 = score1 gs + if a1 && e1 then 1 else 0
      sc2 = score2 gs + if a2 && e2 then 1 else 0
      
      -- New food?
      (nf, ng') = if e1 || e2
                  then placeFood (segments moved1 ++ maybe [] segments moved2) (gen gs)
                  else (food gs, gen gs)
      
      -- Update alive flags
      final1 = moved1 { alive = a1 }
      final2 = fmap (\sn -> sn { alive = a2 }) moved2
      
      -- Game over check
      isOver = case mode gs of
                 Single -> not a1
                 Multi  -> not (a1 && a2) -- One player dead means game over
      
      -- Update high scores
      hi1 = max (high1 gs) sc1
      hi2 = max (high2 gs) sc2
      
      -- New phase
      newPhase = if isOver then Over else Playing
      
  in gs { s1     = final1
        , s2     = final2
        , score1 = sc1
        , score2 = sc2
        , high1  = hi1
        , high2  = hi2
        , food   = nf
        , gen    = ng'
        , phase  = newPhase
        }

-- | Move a snake step, growing if it eats
moveSnake :: Snake -> Pos -> Snake
moveSnake sn fpos =
  let h      = head $ segments sn
      newH   = wrapPos $ case dir sn of
                DirUp    -> (fst h,     snd h + 1)
                DirDown  -> (fst h,     snd h - 1)
                DirLeft  -> (fst h - 1, snd h)
                DirRight -> (fst h + 1, snd h)
      ate    = newH == fpos
      newSeg = if ate then newH : segments sn else newH : init (segments sn)
  in sn { segments = newSeg }

-- | Wrap position around the grid edges
wrapPos :: Pos -> Pos
wrapPos (x, y) = ((x + gridW) `mod` gridW, (y + gridH) `mod` gridH)

-- | Check collision (self, other) and eating
checkSnake :: Snake -> [Pos] -> Pos -> (Bool, Bool)
checkSnake sn others fpos =
  let h      = head $ segments sn
      body   = tail $ segments sn
      alive' = not (h `elem` body) && not (h `elem` others)
      ate    = h == fpos
  in (alive', ate)

-- | Main entry point
main :: IO ()
main = do
  rng <- getStdGen
  play
    (InWindow "Snake" (windowSize, windowSize) (100, 100))
    black
    10
    (initialMenu rng)
    draw
    event
    update