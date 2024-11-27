import System.Random
import Data.List



-- Playing card related data types

data Suit = Clubs | Hearts | Spades | Diamonds deriving Show
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving Show

data Card = Card Suit Rank

instance Show Card where
    show :: Card -> String
    show (Card suit value) = show value ++ " of " ++ show suit

type Deck = [Card]  -- a deck is nothing more than a collection of cards




-- Player related data types

data PlayerBehaviour = RandomPlayer | PassivePlayer | AggressivePlayer | SmartPlayer deriving (Show, Eq)

data Player = Player {
    name :: String,
    hand :: [Card],
    chips :: Int,
    isDealer :: Bool,
    behaviour :: PlayerBehaviour
} deriving Show




-- Game related data types

data GameState = GameState {
    activePlayers :: [Player],
    deck :: Deck,
    communityCards :: [Card],
    pot :: Int,
    bets :: [Int],
    dealerPosition :: Int,
    smallBlindPosition :: Int,
    bigBlindPosition :: Int
} deriving Show




createDeck :: Deck
createDeck = [Card suit rank |
    suit <- [Clubs,Hearts,Spades,Diamonds],
    rank <- [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]]


shuffleDeck :: Int -> Deck -> Deck
shuffleDeck n deck = [ x | (x, a) <- sortBy cmp (zip deck (randoms (mkStdGen n) :: [Int]))]
    where
        cmp :: (a, Int) -> (a, Int) -> Ordering
        cmp (_, x) (_, y) = compare x y


dealCards :: Int -> Deck -> (Deck, [Card]) -- Tuples the remaining deck and the cards that were dealt out
dealCards 0 cards = (cards, [])
dealCards n [] = ([], [])
dealCards n (first:rest) =
    let (remainingDeck, dealtCards) = dealCards (n-1) rest
    in (remainingDeck, first:dealtCards)



main :: IO ()
main = do
    let deck = createDeck
    putStrLn ("\nDeck:\n")
    putStrLn (show deck ++ "\n")

    let shuffledDeck = shuffleDeck 27 deck
    let deck = shuffledDeck
    putStrLn ("\nShuffled Deck:\n")
    putStrLn (show deck ++ "\n")


    let gameState = GameState {
        activePlayers = [], 
        deck = deck,
        communityCards = [],
        pot = 0,
        bets = [],
        dealerPosition = 0,
        smallBlindPosition = 1,
        bigBlindPosition = 2
    }

    putStrLn ("\nGame State:\n")
    putStrLn (show gameState ++ "\n")


    let (remainingDeck, dealtCards) = dealCards 4 deck
    let deck = remainingDeck

    putStrLn ("\nDeal 4 cards:\n")
    putStrLn (show dealtCards ++ "\n")
    putStrLn (show deck ++ "\n")
