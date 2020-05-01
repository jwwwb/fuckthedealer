module Main where
import qualified Game
import Control.Monad
import System.Random

minDie :: Int
minDie = 0

maxDie :: Int
maxDie = 999

main :: IO ()
main = do
  seed <- forM [1..Game.numCards] $ \i -> do
    randomRIO (minDie, maxDie)
  let game = Game.newGame seed ["Alice", "Bob", "Charlie"]
  putStrLn (show game)
