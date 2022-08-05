#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.hspec])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

import Test.Hspec

main :: IO ()
main = do
  putStrLn "Hello Mars!"
  tests

-- Data Types --

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
