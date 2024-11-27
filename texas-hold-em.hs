import System.Random
import Data.List



-- Playing cards and chips related data types

data Suit = Clubs | Hearts | Spades | Diamonds deriving Show
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving Show

data Card = Card Suit Rank

instance Show Card where
    show :: Card -> String
    show (Card suit value) = show value ++ " of " ++ show suit


newtype Deck = Deck [Card] deriving Show
newtype Hand = Hand [Card] deriving Show
newtype CommunityCards = CommunityCards [Card] deriving Show

type Chips = Int



-- Player related data types

data PlayerBehaviour = RandomPlayer | PassivePlayer | AggressivePlayer | SmartPlayer deriving (Show, Eq)

data Player = Player {
    name :: String,
    hand :: Hand,
    chips :: Chips,
    isDealer :: Bool,
    behaviour :: PlayerBehaviour
} deriving Show




-- Game related data types

data GameState = GameState {
    activePlayers :: [Player],
    deck :: Deck,
    communityCards :: CommunityCards,
    pot :: Chips,
    bets :: [Int],
    dealerPosition :: Int,
    smallBlindPosition :: Int,
    bigBlindPosition :: Int
} deriving Show




createDeck :: Deck
createDeck = Deck [Card suit rank |
    suit <- [Clubs,Hearts,Spades,Diamonds],
    rank <- [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]]


shuffleDeck :: Int -> Deck -> Deck
shuffleDeck n (Deck cards) = Deck [ x | (x, a) <- sortBy cmp (zip cards (randoms (mkStdGen n) :: [Int]))]
    where
        cmp :: (a, Int) -> (a, Int) -> Ordering
        cmp (_, x) (_, y) = compare x y


dealCards :: Int -> Deck -> (Deck, [Card]) -- Tuples the remaining deck and the cards that were dealt out
dealCards 0 (Deck cards) = (Deck cards, [])
dealCards n (Deck []) = (Deck [], [])
dealCards n (Deck (first:rest)) =
    let (Deck remainingCards, dealtCards) = dealCards (n-1) (Deck rest)
    in (Deck remainingCards, first:dealtCards)




main :: IO ()
main = do
    let (Deck cards) = createDeck
    putStrLn "\nDeck:\n"
    putStrLn (show cards ++ "\n")

    let (Deck shuffledCards) = shuffleDeck 27 (Deck cards)
    let cards = shuffledCards
    putStrLn "\nShuffled Deck:\n"
    putStrLn (show cards ++ "\n")


    let gameState = GameState {
        activePlayers = [],
        deck = Deck cards,
        communityCards = CommunityCards [],
        pot = 0,
        bets = [],
        dealerPosition = 0,
        smallBlindPosition = 1,
        bigBlindPosition = 2
    }

    putStrLn "\nGame State:\n"
    putStrLn (show gameState ++ "\n")


    let (Deck remainingCards, dealtCards) = dealCards 4 (Deck cards)
    let cards = remainingCards

    putStrLn "\nDeal 4 cards:\n"
    putStrLn (show dealtCards ++ "\n")
    putStrLn (show cards ++ "\n")
