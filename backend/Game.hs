module Game( newGame
           , GameState
           , readValue
           , numCards
           , jsonPlayer
           , jsonDealer
           , guess
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

showSuit :: Suit -> String
showSuit Spades = "s"
showSuit Hearts = "h"
showSuit Clubs = "c"
showSuit Diamonds = "d"

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

showValue :: Value -> String
showValue Two = "2"
showValue Three = "3"
showValue Four = "4"
showValue Five = "5"
showValue Six = "6"
showValue Seven = "7"
showValue Eight = "8"
showValue Nine = "9"
showValue Ten = "t"
showValue Jack = "j"
showValue Queen = "q"
showValue King = "k"
showValue Ace = "a"

readValue :: String -> Maybe Value
readValue "2" = Just Two
readValue "3" = Just Three
readValue "4" = Just Four
readValue "5" = Just Five
readValue "6" = Just Six
readValue "7" = Just Seven
readValue "8" = Just Eight
readValue "9" = Just Nine
readValue "t" = Just Ten
readValue "j" = Just Jack
readValue "q" = Just Queen
readValue "k" = Just King
readValue "a" = Just Ace
readValue other = Nothing

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

currentCard :: GameState -> Card
currentCard game = (deck game !! uncovered game)

currentValue :: GameState -> Value
currentValue game = getValue (currentCard game)

showCard :: Card -> String
showCard (Card value suit) = showValue value ++ showSuit suit

join :: String -> String -> String
join a b = a ++ ", " ++ b

showCards :: Deck -> String
showCards [] = ""
showCards [single] = "\"" ++ (showCard single) ++ "\""
showCards deck = foldl1 join (map (\c -> "\"" ++ showCard c ++ "\"") deck)

jsonPlayer :: GameState -> String
jsonPlayer game = "{\"revealed\": " ++ revealed ++ "}"
  where revealed = "[" ++ individuals ++ "]"
        individuals = showCards (take (uncovered game) (deck game))

jsonDealer :: GameState -> String
jsonDealer game = "{\"revealed\": " ++ revealed ++
                  ", \"current\": \"" ++ current ++ "\"}"
  where revealed = "[" ++ individuals ++ "]"
        individuals = showCards (take (uncovered game) (deck game))
        current = showCard (currentCard game)

inc :: Wrongs -> Wrongs
inc ZeroWrongs = OneWrong
inc OneWrong = TwoWrongs
inc TwoWrongs = ZeroWrongs

pass :: [String] -> Int -> Int
pass players number = (number + 1) `mod` (length players)

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
  where nextGame = GameState { deck = (deck game)
                             , uncovered = nextUncovered
                             , players = (players game)
                             , dealer = nextDealer
                             , guesses = nextGuesses
                             , guesser = nextGuesser }
        punishment = case miss of
                       0 -> Punishment (dealer game) dealerPun
                       val -> Punishment (guesser game) val
        nextGuesses = updateGuesses (guesses game) correct
        nextUncovered = case (correct, (guessNumber (guesses game))) of
                          (False, FirstGuess) -> uncovered game
                          _ -> (uncovered game) + 1
        nextGuesser = case (correct, (guessNumber (guesses game))) of
                        (False, FirstGuess) -> guesser game
                        _ -> case (pass' (guesser game)) == nextDealer of
                               True -> pass' (pass' (guesser game))
                               False -> pass' (guesser game)
        nextDealer = case shouldUpdateDealer (guesses game) correct of
                       True -> pass' (dealer game)
                       False -> dealer game
        pass' = pass (players game)
        correct = miss == 0
        miss = missesBy card (currentValue game)
        dealerPun = dealerPunishment (guessNumber (guesses game))
