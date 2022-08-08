#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.hspec p.hspec-megaparsec p.megaparsec])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T _test"

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (foldl', traverse_)
import Data.Ix
import Data.Void
import System.Environment
import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

main :: IO ()
main = do
  (f : _) <- getArgs
  content <- readFile f
  case M.runParser parseMarsState "" content of
    Left e -> putStrLn $ M.errorBundlePretty e
    Right v -> printSimulation v

_test :: IO ()
_test = do
  putStrLn "Hello Mars!"
  printSimulation $
    MarsState
      (mkBounds 4 8)
      [ (RoverLocation 2 3 E, [L, F, R, F, F]),
        (RoverLocation 0 2 N, [F, F, L, F, R, F, F])
      ]
  printSimulation $
    MarsState
      (mkBounds 4 8)
      [ (RoverLocation 2 3 N, [F, L, L, F, R]),
        (RoverLocation 1 0 S, [F, F, R, L, F])
      ]
  runTests

-- ++ Data Types ++ --

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

-- ++ Functions ++ --

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

printSimulation :: MarsState -> IO ()
printSimulation marsState = do
  traverse_ putStrLn (prettyResult <$> runSimulation marsState)

prettyResult :: Result -> String
prettyResult = \case
  Left Nothing -> "Rover failed to land!"
  Left (Just RoverLocation {..}) -> "(" <> show x <> ", " <> show y <> ", " <> show orientation <> ") Lost"
  Right (RoverLocation {..}) -> "(" <> show x <> ", " <> show y <> ", " <> show orientation <> ")"

-- ++ Parsing ++ --

type Parser = M.Parsec Void String

parseNum :: (Num a, Read a) => Parser a
parseNum = read <$> M.some M.digitChar

parseBounds :: Parser Bounds
parseBounds = mkBounds <$> (parseNum <* M.space) <*> parseNum

parseOrientation :: Parser Orientation
parseOrientation =
  M.choice
    [ N <$ M.char 'N',
      E <$ M.char 'E',
      S <$ M.char 'S',
      W <$ M.char 'W'
    ]

parseDirection :: Parser Direction
parseDirection =
  M.choice
    [ F <$ M.char 'F',
      L <$ M.char 'L',
      R <$ M.char 'R'
    ]

parseRoverLocation :: Parser RoverLocation
parseRoverLocation =
  RoverLocation
    <$> (M.char '(' *> parseNum)
    <*> (M.string ", " *> parseNum)
    <*> (M.string ", " *> parseOrientation <* M.char ')')

parseMarsState :: Parser MarsState
parseMarsState =
  MarsState
    <$> (parseBounds <* M.newline)
    <*> M.some (parseRover <* M.many M.newline)
  where
    parseRover :: Parser (RoverLocation, [Direction])
    parseRover =
      (,)
        <$> (parseRoverLocation <* M.space)
        <*> M.many parseDirection

-- ++ Tests ++ --

runTests :: IO ()
runTests = hspec $ do
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

  describe "Parse Mars State" $ do
    it "should parse input" $ do
      M.parse parseMarsState "" "4 8\n(2, 3, N) FLLFR\n(1, 0, S) FFRLF" `shouldParse` marsParse
    it "should not parse input" $ do
      M.parse parseMarsState "" `shouldFailOn` "0 1\n(1, 2, Q) FRF\n(2, 0, W)"
  where
    mars1 = MarsState (mkBounds 2 2) [(RoverLocation 3 0 N, [F, F])]
    mars2 =
      MarsState
        (mkBounds 2 2)
        [ (RoverLocation 1 1 E, [F, R, F]),
          (RoverLocation 5 0 N, [F, F]),
          (RoverLocation 0 0 N, [F, F, L, F])
        ]
    marsParse =
      MarsState
        (mkBounds 4 8)
        [(RoverLocation 2 3 N, [F, L, L, F, R]), (RoverLocation 1 0 S, [F, F, R, L, F])]
