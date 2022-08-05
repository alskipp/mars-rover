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

isValidLocation :: Bounds -> RoverLocation -> Bool
isValidLocation Bounds {..} RoverLocation {..} =
  inRange horizontal x && inRange vertical y

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
      isValidLocation (mkBounds 2 2) (RoverLocation 3 1 N) `shouldBe` False
    it "should be valid location" $ do
      isValidLocation (mkBounds 2 2) (RoverLocation 0 1 N) `shouldBe` True
