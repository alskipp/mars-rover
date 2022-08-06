#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.hspec])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (foldl')
import Data.Ix
import Test.Hspec

main :: IO ()
main = do
  putStrLn "Hello Mars!"
  tests

-- Data Types --

data Bounds = Bounds
  { horizontal :: (Int, Int),
    vertical :: (Int, Int)
  }
  deriving stock (Show, Eq)

mkBounds :: Int -> Int -> Bounds
mkBounds x y = Bounds (0, x) (0, y)

data Direction
  = F
  | L
  | R
  deriving stock (Show, Eq)

data Orientation
  = N
  | E
  | S
  | W
  deriving stock (Show, Eq, Enum)

data RoverLocation = RoverLocation {x :: Int, y :: Int, orientation :: Orientation}
  deriving stock (Show, Eq)

data MarsState = MarsState
  { bounds :: Bounds,
    rovers :: [(RoverLocation, [Direction])]
  }
  deriving stock (Show, Eq)

-- If the rover gets lost the error case will contain the previous location
-- If the rover fails to land there won't be a previous location in the error case
type Result = Either (Maybe RoverLocation) RoverLocation

-- Functions --

updateOrientation :: Orientation -> Direction -> Orientation
updateOrientation orientation = \case
  F -> orientation
  L -> turnLeft orientation
  R -> turnRight orientation
  where
    turnLeft N = W
    turnLeft d = pred d
    turnRight W = N
    turnRight d = succ d

isValidRoverLocation :: Bounds -> RoverLocation -> Bool
isValidRoverLocation Bounds {..} RoverLocation {..} =
  inRange horizontal x && inRange vertical y

landingLocation :: Bounds -> RoverLocation -> Result
landingLocation bounds location =
  if isValidRoverLocation bounds location
    then Right location
    else Left Nothing

updateRoverLocation :: Bounds -> Result -> Direction -> Result
updateRoverLocation bounds location direction = do
  currentLocation@RoverLocation {..} <- location
  mkResult currentLocation $ case direction of
    F -> moveForward currentLocation orientation
    L -> RoverLocation {orientation = updateOrientation orientation L, ..}
    R -> RoverLocation {orientation = updateOrientation orientation R, ..}
  where
    moveForward RoverLocation {..} = \case
      N -> RoverLocation {y = succ y, ..}
      E -> RoverLocation {x = succ x, ..}
      S -> RoverLocation {y = pred y, ..}
      W -> RoverLocation {x = pred x, ..}
    mkResult current newLocation =
      if isValidRoverLocation bounds newLocation
        then Right newLocation
        else Left (Just current)

runCommands :: Bounds -> (RoverLocation, [Direction]) -> Result
runCommands bounds (location, directions) =
  foldl' (updateRoverLocation bounds) (landingLocation bounds location) directions

runSimulation :: MarsState -> [Result]
runSimulation MarsState {..} =
  fmap (runCommands bounds) rovers

-- Tests --

tests :: IO ()
tests = hspec $ do
  describe "Update orientation" $ do
    it "should not change direction when moving forward" $ do
      updateOrientation N F `shouldBe` N
    it "should rotate clockwise" $ do
      updateOrientation E R `shouldBe` S
      updateOrientation W R `shouldBe` N
    it "should rotate counter clockwise" $ do
      updateOrientation N L `shouldBe` W
      updateOrientation S L `shouldBe` E

  describe "Rover location checking" $ do
    it "should be invalid location" $ do
      isValidRoverLocation (mkBounds 2 2) (RoverLocation 3 1 N) `shouldBe` False
    it "should be valid location" $ do
      isValidRoverLocation (mkBounds 2 2) (RoverLocation 0 1 N) `shouldBe` True

  describe "Update rover location" $ do
    it "should rotate and stay in same location" $ do
      updateRoverLocation (mkBounds 0 0) (Right $ RoverLocation 0 0 N) L `shouldBe` Right (RoverLocation 0 0 W)
      updateRoverLocation (mkBounds 0 0) (Right $ RoverLocation 0 0 W) R `shouldBe` Right (RoverLocation 0 0 N)
    it "should move north 1 unit" $ do
      updateRoverLocation (mkBounds 2 2) (Right $ RoverLocation 0 0 N) F `shouldBe` Right (RoverLocation 0 1 N)
    it "should move east 1 unit" $ do
      updateRoverLocation (mkBounds 2 2) (Right $ RoverLocation 0 0 E) F `shouldBe` Right (RoverLocation 1 0 E)
    it "should be lost!" $ do
      updateRoverLocation (mkBounds 0 0) (Right $ RoverLocation 0 0 E) F `shouldBe` Left (Just $ RoverLocation 0 0 E)

  describe "Run commands" $ do
    it "should move east 2 units then rotate westwards" $ do
      runCommands (mkBounds 2 2) (RoverLocation 0 0 E, [F, F, R, R]) `shouldBe` Right (RoverLocation 2 0 W)

  describe "Run Mars simulation" $ do
    it "should fail to land!" $ do
      runSimulation mars1 `shouldBe` [Left Nothing]
    it "should have one success, one fail to land, one lost" $ do
      runSimulation mars2
        `shouldBe` [ Right $ RoverLocation 2 0 S,
                     Left Nothing,
                     Left $ Just (RoverLocation 0 2 W)
                   ]
  where
    mars1 = MarsState (mkBounds 2 2) [(RoverLocation 3 0 N, [F, F])]
    mars2 =
      MarsState
        (mkBounds 2 2)
        [ (RoverLocation 1 1 E, [F, R, F]),
          (RoverLocation 5 0 N, [F, F]),
          (RoverLocation 0 0 N, [F, F, L, F])
        ]
