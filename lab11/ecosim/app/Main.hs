{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Array
import Data.List     (sortOn)
import Text.Printf   (printf)
import Control.Monad.State
import Control.Monad (replicateM)

-- | Configuration
windowWidth, windowHeight :: Int
windowWidth  = 800
windowHeight = 800

cellSize :: Float
cellSize     = 8       -- 100×100 grid in 800px window

gridSize :: Int
gridSize     = 100

sensingRadius :: Int
sensingRadius = 10

-- | Resource generation parameters
resourceScarcity :: Float
resourceScarcity = 0.05  -- Lower value = fewer resources

foodRegenChance, waterRegenChance :: Float
foodRegenChance  = 0.005  -- Chance of new food appearing per tick
waterRegenChance = 0.003  -- Chance of new water appearing per tick

-- | Cell Types
data Cell
  = Empty
  | Obstacle
  | Food  { nutrition   :: Float
          , currentLevel :: Float }
  | Water { hydration    :: Float
          , currentLevel :: Float }
  deriving (Eq, Show)

type Grid = Array (Int,Int) Cell

-- | Agents
data Agent = Agent
  { agentId  :: Int
  , pos      :: (Int,Int)
  , health   :: Float
  , hunger   :: Float
  , thirst   :: Float
  , energy   :: Float
  , age      :: Int
  , strategy :: StrategyParams
  , fitness  :: Float
  } deriving Show

-- | Strategy parameters for agent decision-making
data StrategyParams = StrategyParams
  { weightHunger      :: Float  -- How much to prioritize food when hungry
  , weightThirst      :: Float  -- How much to prioritize water when thirsty
  , weightDistance    :: Float  -- How much to consider distance to resources
  , weightExploration :: Float  -- How much to value exploring unknown areas
  , energyConservation:: Float  -- How much to prioritize energy conservation
  , mutationRate      :: Float  -- Rate of mutation for evolution
  } deriving (Eq, Show)

type World = (Grid, [Agent], StdGen, Int)

main :: IO ()
main = do
  gen0 <- getStdGen
  let (grid0, gen1) = randomGrid gen0
      agents0       = initAgents gen1 10          -- only 10 agents
      world0        = (grid0, agents0, gen1, 1)
  playIO
    (InWindow "Ecosystem" (windowWidth,windowHeight) (100,100))
    black
    10           -- slow tick rate for visibility
    world0
    drawWorld
    handleEvent
    updateWorld

-- | Randomly initialize the grid
randomGrid :: StdGen -> (Grid, StdGen)
randomGrid gen = (array bnds cells, gen')
  where
    bnds = ((0,0),(gridSize-1,gridSize-1))
    (cells, gen') = go [] gen [ (i,j) | i <- [0..gridSize-1], j <- [0..gridSize-1] ]
    go acc g []     = (acc, g)
    go acc g (p:ps) =
      let (r, g1) = randomR (0::Float,1) g
          cell
            | r < 0.01  = Obstacle
            | r < 0.03  = Food  1 1      -- Reduced initial food (from 0.1 to 0.03)
            | r < 0.05  = Water 1 1      -- Reduced initial water (from 0.05 to 0.05)
            | otherwise = Empty
      in go ((p,cell):acc) g1 ps

-- | Initialize N agents at random positions
initAgents :: StdGen -> Int -> [Agent]
initAgents gen n = zipWith mkAgent [1..n] posList
  where
    posList = take n $ randomRs ((0,0),(gridSize-1,gridSize-1)) gen
    baseStr = StrategyParams 1 1 1 0.5 0.3 0.1
    mkAgent i p = Agent i p 1 0 0 50 0 baseStr 0

-- | Create a new generation of agents through evolution
evolveAgents :: [Agent] -> StdGen -> Int -> [Agent]
evolveAgents oldAgents gen count
  | null oldAgents = initAgents gen count  -- If no agents survived, create new ones
  | otherwise = 
      let -- Sort agents by fitness (highest first)
          sortedAgents = sortOn (negate . fitness) oldAgents
          
          -- Take the top performers (or all if fewer than half survived)
          eliteCount = max 1 (length sortedAgents `div` 2)
          elites = take eliteCount sortedAgents
          
          -- Generate new agents through breeding and mutation
          (newAgents, gen') = runState (replicateM count (breedAgent elites)) gen
      in newAgents

-- | Breed a new agent from elite parents with possible mutations
breedAgent :: [Agent] -> State StdGen Agent
breedAgent parents = do
  -- Select two parents weighted by fitness
  parent1 <- selectParent parents
  parent2 <- selectParent parents
  
  -- Create new position
  pos' <- randomGridPos
  
  -- Crossover strategy parameters with mutation
  strategy' <- crossoverStrategy (strategy parent1) (strategy parent2)
  
  -- Return new agent
  return $ Agent 
    { agentId  = agentId parent1  -- Reuse ID for now
    , pos      = pos'
    , health   = 1
    , hunger   = 0
    , thirst   = 0
    , energy   = 50
    , age      = 0
    , strategy = strategy'
    , fitness  = 0
    }

-- | Select a parent weighted by fitness
selectParent :: [Agent] -> State StdGen Agent
selectParent parents = do
  let totalFitness = sum $ map fitness parents
      normalizedFitness = map (\a -> fitness a / totalFitness) parents
      
  r <- state $ randomR (0, 1)
  
  -- Select based on cumulative probability
  return $ selectWithProbability r (zip normalizedFitness parents) 0
  where
    selectWithProbability _ [(_, a)] _ = a
    selectWithProbability r ((p, a):rest) acc
      | r <= acc + p = a
      | otherwise = selectWithProbability r rest (acc + p)
    selectWithProbability _ [] _ = head parents  -- Fallback

-- | Crossover and mutate strategy parameters
crossoverStrategy :: StrategyParams -> StrategyParams -> State StdGen StrategyParams
crossoverStrategy StrategyParams{..} StrategyParams{weightHunger=wh2, weightThirst=wt2, weightDistance=wd2, weightExploration=we2, energyConservation=ec2, mutationRate=mr2} = do
  -- Average parameters with random variation based on mutation rate
  let avgRate = (mutationRate + mr2) / 2
      mutate val = do
        shouldMutate <- state $ randomR (0, 1)
        if shouldMutate < avgRate
          then state $ randomR (val * 0.5, val * 1.5)  -- 50% mutation range
          else return val
  
  -- Apply mutations
  wh' <- mutate ((weightHunger + wh2) / 2)
  wt' <- mutate ((weightThirst + wt2) / 2)
  wd' <- mutate ((weightDistance + wd2) / 2)
  we' <- mutate ((weightExploration + we2) / 2)
  ec' <- mutate ((energyConservation + ec2) / 2)
  mr' <- mutate avgRate
  
  return $ StrategyParams 
    { weightHunger = wh'
    , weightThirst = wt'
    , weightDistance = wd'
    , weightExploration = we'
    , energyConservation = ec'
    , mutationRate = mr'
    }

-- | Generate a random position on the grid
randomGridPos :: State StdGen (Int, Int)
randomGridPos = do
  x <- state $ randomR (0, gridSize-1)
  y <- state $ randomR (0, gridSize-1)
  return (x, y)

-- | Drawing
drawWorld :: World -> IO Picture
drawWorld (grid, agents, _, genNum) = return . Pictures $
  [ drawCells grid
  , drawAgents agents
  , drawStats agents genNum
  ]

drawCells :: Grid -> Picture
drawCells gr = Pictures
  [ translate x y
      $ color (cellColor c)
      $ rectangleSolid s s
  | ((i,j),c) <- assocs gr
  , let x = fromIntegral i * s - fromIntegral windowWidth/2 + s/2
        y = fromIntegral j * s - fromIntegral windowHeight/2 + s/2
        s = cellSize
  ]
  where
    cellColor = \case
      Empty    -> greyN 0.1
      Obstacle -> greyN 0.5
      Food{currentLevel=lv}  -> withAlpha lv green
      Water{currentLevel=lv} -> withAlpha lv blue

drawAgents :: [Agent] -> Picture
drawAgents = Pictures . map drawA
  where
    drawA Agent{..} =
      let (i,j) = pos
          x = fromIntegral i * cellSize - fromIntegral windowWidth/2 + cellSize/2
          y = fromIntegral j * cellSize - fromIntegral windowHeight/2 + cellSize/2
      in translate x y $ color (healthColor health) $ circleSolid (cellSize*0.4)
    healthColor h = makeColor h (1-h) 0.5 1

drawStats :: [Agent] -> Int -> Picture
drawStats agents genNum = translate (-380) 360 . scale 0.1 0.1 . color white . Text $
  unlines
    [ "Generation: "  ++ show genNum
    , "Agents: "      ++ show (length agents)
    , printf "Avg Health: %.2f" (avg health :: Float)
    , printf "Avg Hunger: %.2f" (avg hunger :: Float)
    , printf "Avg Thirst: %.2f" (avg thirst :: Float)
    , printf "Avg Energy: %.2f" (avg energy :: Float)
    , printf "Avg Age: %.1f" (avg (fromIntegral . age :: Agent -> Float))
    , printf "Avg Fitness: %.1f" (avg fitness :: Float)
    , ""
    , "Strategy Parameters (Avg):"
    , printf "- Hunger Weight: %.2f" (avgStrategy weightHunger :: Float)
    , printf "- Thirst Weight: %.2f" (avgStrategy weightThirst :: Float)
    , printf "- Distance Weight: %.2f" (avgStrategy weightDistance :: Float)
    , printf "- Exploration: %.2f" (avgStrategy weightExploration :: Float)
    , printf "- Energy Conservation: %.2f" (avgStrategy energyConservation :: Float)
    ]
  where
    avg :: (Agent -> Float) -> Float
    avg f = if null agents then 0 else sum (map f agents) / fromIntegral (length agents)
    
    avgStrategy :: (StrategyParams -> Float) -> Float
    avgStrategy f = if null agents then 0 else sum (map (f . strategy) agents) / fromIntegral (length agents)

-- | Event handling (stub)
handleEvent :: Event -> World -> IO World
handleEvent _ w = return w

-- | World update per tick
updateWorld :: Float -> World -> IO World
updateWorld _ (grid, agents, gen, genNum) = do
  -- 1) Generate new resources randomly
  let (grid', gen') = generateResources grid gen

  -- 2) step each agent (move, consume, update stats)
  let (agents', grid'', gen'') = stepAllAgents grid' agents gen'

  -- 3) filter out dead agents
  let survivors = filter (\a -> energy a > 0 && health a > 0) agents'
      allDead = null survivors

  -- 4) if all dead, reset with evolution and fresh grid
  if allDead
    then do
      -- Start a new generation with a fresh grid
      let (freshGrid, gen''') = randomGrid gen''
          -- Apply evolution to create the next generation based on fitness
          evolvedAgents = evolveAgents agents gen''' 10
      return (freshGrid, evolvedAgents, gen''', genNum+1)
    else
      return (grid'', survivors, gen'', genNum)

-- | Randomly generate new resources
generateResources :: Grid -> StdGen -> (Grid, StdGen)
generateResources grid gen = 
  let (foodGen, waterGen) = split gen
      emptySpots = [(i, j) | ((i, j), cell) <- assocs grid, cell == Empty]
      
      -- Generate food
      (grid', gen') = addResources grid foodGen foodRegenChance emptySpots (\_ -> Food 1 1)
      
      -- Generate water
      (grid'', gen'') = addResources grid' waterGen waterRegenChance emptySpots (\_ -> Water 1 1)
  in 
      (grid'', gen'')

-- | Helper function to add resources to the grid
addResources :: Grid -> StdGen -> Float -> [(Int, Int)] -> ((Int, Int) -> Cell) -> (Grid, StdGen)
addResources grid gen _ [] _ = (grid, gen)
addResources grid gen chance spots mkResource = 
  let (addResource, gen') = randomR (0 :: Float, 1) gen
      (idx, gen'') = randomR (0, length spots - 1) gen'
      pos = spots !! idx
      newSpots = take idx spots ++ drop (idx + 1) spots
      
      grid' = if addResource < chance && not (null spots)
              then grid // [(pos, mkResource pos)]
              else grid
  in
      if addResource < chance && not (null spots)
      then addResources grid' gen'' chance newSpots mkResource
      else (grid', gen'')

stepAllAgents :: Grid -> [Agent] -> StdGen -> ([Agent], Grid, StdGen)
stepAllAgents grid as gen = foldr stepAndUpdate ([], grid, gen) as
  where
    stepAndUpdate agent (agents, g, gen') = 
      let (agent', g', gen'') = stepAgent g agent gen'
      in (agent':agents, g', gen'')

-- | Single-agent update with resource consumption
stepAgent :: Grid -> Agent -> StdGen -> (Agent, Grid, StdGen)
stepAgent grid ag@Agent{..} gen =
  let newPos@(i',j') = decideMove grid ag strategy
      cell = grid ! newPos

      -- Handle food consumption
      (consumed, hunger', grid') = case cell of
        Food {currentLevel = lv} | lv > 0 -> 
          -- Completely consume the food resource (set cell to Empty)
          (lv, max 0 (hunger - (lv * 0.5)), grid // [(newPos, Empty)])
        _ -> (0, hunger + 0.02, grid)  -- Increased hunger rate

      -- Handle water consumption
      (drank, thirst', grid'') = case grid' ! newPos of
        Water {currentLevel = lv} | lv > 0 -> 
          -- Completely consume the water resource (set cell to Empty)
          (lv, max 0 (thirst - (lv * 0.5)), grid' // [(newPos, Empty)])
        _ -> (0, thirst + 0.02, grid')  -- Increased thirst rate

      -- Energy is depleted by movement
      e' = max 0 (energy - 1 - (0.1 * (hunger + thirst)))
      
      -- Health is a function of hunger and thirst
      health' = calcHealth (hunger', thirst')
      
      age' = age + 1
      
      -- Update fitness based on survival and resource consumption
      resourceBonus = consumed + drank
      fit' = fitness + (1 + resourceBonus * 2)
  in 
      ( ag { pos     = newPos
           , hunger  = hunger'
           , thirst  = thirst'
           , energy  = e'
           , health  = health'
           , age     = age'
           , fitness = fit'
           }
      , grid''
      , gen
      )

-- | Heuristic: move toward nearest resource within sensing radius
decideMove :: Grid -> Agent -> StrategyParams -> (Int,Int)
decideMove grid Agent{pos=(x,y), hunger=h, thirst=t, energy=e} StrategyParams{..} =
  let candidates =
        [ (dx, dy)
        | dx <- [-sensingRadius..sensingRadius]
        , dy <- [-sensingRadius..sensingRadius]
        , (dx,dy) /= (0,0)
        , let tx = x + dx
              ty = y + dy
        , tx>=0, ty>=0, tx<gridSize, ty<gridSize
        ]

      -- Calculate Manhattan distance (steps needed to reach)
      manhattanDist (dx, dy) = abs dx + abs dy
      
      -- Calculate score for a potential move
      score (dx,dy) =
        let cell = grid ! (x+dx, y+dy)
            dist = fromIntegral $ manhattanDist (dx, dy)
            isResource = case cell of
                           Food{}  -> True
                           Water{} -> True
                           _       -> False
                           
            -- Obstacle avoidance (give negative score)
            obstacleScore = case cell of
                              Obstacle -> -100
                              _        -> 0
                              
            -- Resource urgency based on agent needs
            urgency = case cell of
                        Food{}  -> weightHunger * h  -- Prioritize food when hungry
                        Water{} -> weightThirst * t  -- Prioritize water when thirsty
                        _       -> 0
                        
            -- Distance penalty (closer is better)
            distScore = if dist > 0 
                        then weightDistance / dist
                        else weightDistance
                        
            -- Energy conservation - discourage movement when energy is low
            energyFactor = if e < 10
                           then energyConservation * (1 - e / 50)
                           else 0
                           
            -- Exploration score for non-resource cells
            explorationScore = if isResource 
                               then 0 
                               else weightExploration * (1 / (dist + 1))
                               
            -- Combined score with more sophisticated weighting
            totalScore = urgency * distScore + 
                         explorationScore - 
                         energyFactor * dist +
                         obstacleScore
        in totalScore

      -- Sort by score in descending order
      sorted = sortOn (negate . score) candidates
      
      -- Choose the best move or stay put if low energy and nothing attractive nearby
      best = if null sorted || (e < 5 && all (\p -> score p <= 0) sorted)
             then (0,0)  -- Stay put to conserve energy
             else head sorted

      -- Convert to a single step in the best direction
      step d = signum d
      nx = clamp 0 (gridSize-1) (x + step (fst best))
      ny = clamp 0 (gridSize-1) (y + step (snd best))
  in (nx, ny)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- | Health function - inverse relationship with hunger and thirst
-- Health decreases more rapidly as hunger and thirst increase
calcHealth :: (Float,Float) -> Float
calcHealth (h,t) = max 0 (1 - (h^2 + t^2) / 4)