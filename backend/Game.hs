module Game( newGame
           , numCards
           ) where
import Data.List

data Suit = Spades | Clubs | Diamonds | Hearts deriving (Enum, Show)

suits :: [Suit]
suits = [Spades .. ]

numSuits :: Int
numSuits = length suits

numericSuit :: Suit -> Int
numericSuit Spades = 0
numericSuit Clubs = 1
numericSuit Diamonds = 2
numericSuit Hearts = 3

data Value = Two | Three | Four | Five |
             Six | Seven | Eight | Nine |
             Ten | Jack | Queen | King |
             Ace deriving (Enum, Show)

values :: [Value]
values = [Two .. ]

numericValue :: Value -> Int
numericValue Two = 2
numericValue Three = 3
numericValue Four = 4
numericValue Five = 5
numericValue Six = 6
numericValue Seven = 7
numericValue Eight = 8
numericValue Nine = 9
numericValue Ten = 10
numericValue Jack = 11
numericValue Queen = 12
numericValue King = 13
numericValue Ace = 14

numValues :: Int
numValues = length values

numCards :: Int
numCards = numSuits * numValues

data Card = Card Value Suit deriving (Show)

getValue :: Card -> Value
getValue (Card v _) = v

type Deck = [Card]

-- flat map / outer product over all values and suits:
baseDeck :: Deck
baseDeck = values >>= \v -> suits >>= \s -> [Card v s]

data Wrongs = ZeroWrongs | OneWrong | TwoWrongs deriving (Show)

data GuessNumber = FirstGuess | SecondGuess deriving (Show)

data Guesses = Guesses { guessNumber :: GuessNumber
                       , wrongs ::  Wrongs
                       } deriving (Show)

initialGuesses :: Guesses
initialGuesses = Guesses { guessNumber = FirstGuess
                         , wrongs = ZeroWrongs }

-- I have no idea how the asymptotic complexity is, but fuck it:
shuffle :: Ord a => [b] -> [a] -> [b]
shuffle deck seed = fst (unzip sorted)
  where sorted = sortOn snd (zip deck seed)

data GameState = GameState { deck ::  Deck
                           , uncovered :: Int
                           , players :: [String]
                           , dealer :: Int
                           , guesses :: Guesses
                           , guesser :: Int
                           } deriving (Show)

newGame :: [Int] -> [String] -> GameState
newGame seed players = GameState { deck = shuffle baseDeck seed
                                 , uncovered = 0
                                 , players = shuffle players seed
                                 , dealer = 0
                                 , guesses = initialGuesses
                                 , guesser = 1
                                 }

missesBy :: Value -> Value -> Int
missesBy guess truth = abs (numericValue guess) - (numericValue truth)

dealerPunishment :: GuessNumber -> Int
dealerPunishment FirstGuess = 5
dealerPunishment SecondGuess = 2

data Punishment = Punishment Int Int

currentValue :: GameState -> Value
currentValue game = getValue ((deck game) !! (uncovered game))

inc :: Wrongs -> Wrongs
inc ZeroWrongs = OneWrong
inc OneWrong = TwoWrongs
inc TwoWrongs = ZeroWrongs

updateGuesses :: Guesses -> Bool -> Guesses
updateGuesses guesses correct = case (correct, (guessNumber guesses)) of
  (True, _) -> Guesses { guessNumber = FirstGuess
                       , wrongs = ZeroWrongs }
  (_, FirstGuess) -> Guesses { guessNumber = SecondGuess
                             , wrongs = wrongs guesses }
  (_, SecondGuess) -> Guesses { guessNumber = FirstGuess
                              , wrongs = inc (wrongs guesses) }

shouldUpdateDealer :: Guesses -> Bool -> Bool
shouldUpdateDealer guesses correct =
  case (correct, (guessNumber guesses), (wrongs guesses)) of
    (False, SecondGuess, TwoWrongs) -> True
    _ -> False

guess :: GameState -> Value -> (GameState, Punishment)
guess game card = (nextGame, punishment)
  where nextGame = game
        punishment = case missesBy card (currentValue game) of
                       0 -> Punishment (dealer game) dealerPun
                       val -> Punishment (guesser game) val
        dealerPun = dealerPunishment (guessNumber (guesses game))
