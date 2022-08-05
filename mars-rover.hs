#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.hspec])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec
import Data.Ix

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

type Result = Either RoverLocation RoverLocation

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

updateRoverLocation :: Bounds -> RoverLocation -> Direction -> Result
updateRoverLocation bounds previousRoverLocation@RoverLocation {..} =
  mkResult . \case
    F -> moveForward orientation
    L -> RoverLocation {orientation = updateOrientation orientation L, ..}
    R -> RoverLocation {orientation = updateOrientation orientation R, ..}
  where
    moveForward = \case
      N -> RoverLocation {y = succ y, ..}
      E -> RoverLocation {x = succ x, ..}
      S -> RoverLocation {y = pred y, ..}
      W -> RoverLocation {x = pred x, ..}
    mkResult location =
      if isValidRoverLocation bounds location
        then Right location
        else Left previousRoverLocation

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
      updateRoverLocation (mkBounds 0 0) (RoverLocation 0 0 N) L `shouldBe` Right (RoverLocation 0 0 W)
      updateRoverLocation (mkBounds 0 0) (RoverLocation 0 0 W) R `shouldBe` Right (RoverLocation 0 0 N)
    it "should move north 1 unit" $ do
      updateRoverLocation (mkBounds 2 2) (RoverLocation 0 0 N) F `shouldBe` Right (RoverLocation 0 1 N)
    it "should move east 1 unit" $ do
      updateRoverLocation (mkBounds 2 2) (RoverLocation 0 0 E) F `shouldBe` Right (RoverLocation 1 0 E)
    it "should be lost!" $ do
      updateRoverLocation (mkBounds 0 0) (RoverLocation 0 0 E) F `shouldBe` Left (RoverLocation 0 0 E)
