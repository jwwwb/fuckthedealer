{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Text
import Data.IORef

import qualified Game

import Network.HTTP.Types

import System.Random

import Web.Spock
import Web.Spock.Config


minDie :: Int
minDie = 0

maxDie :: Int
maxDie = 999

newtype ServerState = ServerState { game :: IORef Game.GameState }

-- SpockM takes connection, session and state type params:
type Server a = SpockM () () ServerState a

updateGame :: Game.GameState -> String -> Game.GameState
updateGame game guess = case (Game.readValue guess) of
  -- just ignore the punishment for now
  Just card -> fst (Game.guess game card)
  Nothing -> game

app :: Server ()
app = do
  get root $ do
    game' <- getState >>= (liftIO . readIORef . game)
    text (pack (Game.jsonPlayer game'))
  get ("guess" <//> var) $ \guess -> do
    gameRef <- game <$> getState
    liftIO $ atomicModifyIORef' gameRef $ \game ->
      (updateGame game guess, ())
    redirect "/"


main :: IO ()
main = do
  seed <- forM [1..Game.numCards] $ \i -> do
    randomRIO (minDie, maxDie)
  let game = Game.newGame seed ["Alice", "Bob", "Charlie"]

  st <- ServerState <$> newIORef game
  cfg <- defaultSpockCfg () PCNoDatabase st

  runSpock 8080 (spock cfg app)
