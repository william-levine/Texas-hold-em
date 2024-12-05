import System.Random
import Control.Monad.State
import Data.List


data CardSuit = Clubs | Hearts | Spades | Diamonds deriving (Show, Eq)

data CardRank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Enum, Ord)

data Card = Card CardSuit CardRank

instance Show Card where
    show :: Card -> String
    show (Card suit rank) = show rank ++ " of " ++ show suit

instance Eq Card where
    (==) :: Card -> Card -> Bool
    (Card _ r1) == (Card _ r2) = r1 == r2
    (/=) :: Card -> Card -> Bool
    (Card _ r1) /= (Card _ r2) = r1 /= r2

instance Ord Card where
    compare (Card _ cr1) (Card _ cr2) = compare cr1 cr2

newtype Deck = Deck [Card] deriving Show

newtype Hand = Hand [Card] deriving Show

data HandRank
    = HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | Straight
    | Flush
    | FullHouse
    | FourOfAKind
    | StraightFlush
    | RoyalFlush
    deriving (Show, Eq, Ord)

data CommunityCards
    = PreFlop
    | Flop [Card]
    | Turn [Card]
    | River [Card]
    deriving Show

type Chips = Int


data PlayerBehaviour = RandomPlayer | PassivePlayer | AggressivePlayer | SmartPlayer deriving (Show, Eq)

data Player = Player {
    name :: String,
    hand :: Hand,
    chips :: Chips,
    isDealer :: Bool,
    behaviour :: PlayerBehaviour
} deriving Show


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
    suit <- [Clubs, Hearts, Spades, Diamonds],
    rank <- [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]]


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


runInitialisation :: GameState
runInitialisation = execState initialiseGame (GameState [] (Deck []) PreFlop 0 [] 0 0 0)

initialiseGame :: State GameState ()
initialiseGame = do
    -- Generate the deck and shuffle it
    let deck = createDeck
    let shuffledDeck = shuffleDeck 15 deck

    -- Generate 5 random players
    let player1 = Player { name = "Player 1", hand = Hand [], chips = 0, isDealer = True,  behaviour = RandomPlayer }
    let player2 = Player { name = "Player 2", hand = Hand [], chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player3 = Player { name = "Player 3", hand = Hand [], chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player4 = Player { name = "Player 4", hand = Hand [], chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player5 = Player { name = "Player 5", hand = Hand [], chips = 0, isDealer = False, behaviour = RandomPlayer }

    -- Update the Game State
    put GameState {
        activePlayers = [player1, player2, player3, player4, player5],
        deck = shuffledDeck,
        communityCards = PreFlop,
        pot = 0,
        bets = [],
        dealerPosition = 0,
        smallBlindPosition = 1,
        bigBlindPosition = 2
    }


getCardRanks :: [Card] -> [CardRank]
getCardRanks = map (\(Card _ rank) -> rank)

getCardSuits :: [Card] -> [CardSuit]
getCardSuits = map (\(Card suit _) -> suit)


isAcePresent :: [Card] -> Bool
isAcePresent = any (\(Card _ rank) -> rank == Ace)


-- Returns all combinations of n cards
cardCombinations :: Int -> [Card] -> [[Card]]
cardCombinations 0 _  = [[]]
cardCombinations _ [] = []
cardCombinations n (x:xs)
    | n > length (x:xs) = []
    | otherwise = map (x:) (cardCombinations (n-1) xs) ++ cardCombinations n xs


-- Ace can be low or high, so the successor of King is Ace
rankSuccessor :: CardRank -> CardRank
rankSuccessor rank
    | rank == King = Ace
    | otherwise = succ rank -- Rank is treated as an enum


getHighestCard :: [Card] -> Card
getHighestCard = maximum


-- Returns a list of the ranks of cards
-- Assumes Ace is LOW
sortByRanksAceLow :: [Card] -> [Card]
sortByRanksAceLow = sort

-- Returns a list of the ranks of cards
-- Assumes Ace is HIGH
sortByRanksAceHigh :: [Card] -> [Card]
sortByRanksAceHigh cards = do 
    let (x:xs) = sort cards
    xs ++ [x]

groupByRanksAceLow :: [Card] -> [[Card]]
groupByRanksAceLow cards = group (sortByRanksAceLow cards)



-- If there is one group with a length of 2 (2 of the cards have the same rank) then it is a Pair
isOnePair :: [Card] -> Bool
isOnePair cards = length (filter (\x -> length x == 2) (groupByRanksAceLow cards)) == 1

-- If there are 2 groups with a length of 2 then there are 2 Pairs - a Two Pair
isTwoPair :: [Card] -> Bool
isTwoPair cards = length (filter (\x -> length x == 2) (groupByRanksAceLow cards)) == 2

-- If there is a group of length 3 then it is a Three of a Kind
isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards = length (filter (\x -> length x == 3) (groupByRanksAceLow cards)) == 1

-- If the cards are consecutive then it is a Straight. However, Ace can be low or high, so both instances must be checked
isStraight :: [Card] -> Bool
isStraight cards =
    correctSuccessor (zip sortedRanks (tail sortedRanks)) ||
    isAcePresent cards && correctSuccessor (zip sortedRanksAceHigh (tail sortedRanksAceHigh))
    where
        correctSuccessor = all (\(Card _ r1, Card _ r2) -> rankSuccessor r1 == r2)
        sortedRanks = sortByRanksAceLow cards
        sortedRanksAceHigh = sortByRanksAceHigh cards

-- If there is 1 group then all the cards have the same suit. This is a Flush
isFlush :: [Card] -> Bool
isFlush cards = length (group (getCardSuits cards)) == 1

-- If there are 2 groups, one of length 2 and one of length 3, then there is a Pair and a Three of a Kind, known as a Full House
isFullHouse :: [Card] -> Bool
isFullHouse cards = 
    case sort (map length (groupByRanksAceLow cards)) of
        [2, 3] -> True
        _      -> False

-- If there is a group of length 4 (4 of the same rank) then it is a Four of a Kind
isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = length (filter (\x -> length x == 4) (groupByRanksAceLow cards)) == 1

-- If a hand is a Straight and a Flush then it is a Straight Flush
isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards

-- If a hand is a Flush and the ranks of the cards are Ten through Ace (high) then it's a Royal Flush
isRoyalFlush :: [Card] -> Bool
isRoyalFlush cards = isFlush cards && getCardRanks (sortByRanksAceHigh cards) == [Ten, Jack, Queen, King, Ace]




-- Functions to update attributes of a game state

updateActivePlayers :: [Player] -> State GameState ()
updateActivePlayers newActivePlayers = modify (\gs -> gs {activePlayers = newActivePlayers})

updateDeck :: Deck -> State GameState ()
updateDeck newDeck = modify (\gs -> gs {deck = newDeck})

updateCommunityCards :: CommunityCards -> State GameState ()
updateCommunityCards newCommunityCards = modify (\gs -> gs { communityCards = newCommunityCards })

updatePot :: Int -> State GameState ()
updatePot newPot = modify (\gs -> gs { pot = newPot })

updateBets :: [Int] -> State GameState ()
updateBets newBets = modify (\gs -> gs { bets = newBets })

updateDealerPosition :: Int -> State GameState ()
updateDealerPosition newDealerPosition = modify (\gs -> gs { dealerPosition = newDealerPosition })

updateSmallBlindPosition :: Int -> State GameState ()
updateSmallBlindPosition newSmallBlindPosition = modify (\gs -> gs { smallBlindPosition = newSmallBlindPosition })

updateBigBlindPosition :: Int -> State GameState ()
updateBigBlindPosition newBigBlindPosition = modify (\gs -> gs { bigBlindPosition = newBigBlindPosition })




main :: IO ()
main = do
    let gameState = runInitialisation

    print gameState