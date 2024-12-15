import System.Random
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Ord


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
    compare :: Card -> Card -> Ordering
    compare (Card _ cr1) (Card _ cr2) = compare cr1 cr2

newtype Deck = Deck [Card] deriving Show

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
    holeCards :: [Card],
    hand :: Maybe ([Card], [Card], HandRank), -- (primary hand, kickers, hand rank)
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


getCardRank :: Card -> CardRank
getCardRank (Card _ rank) = rank

getCardRanks :: [Card] -> [CardRank]
getCardRanks = map getCardRank


getCardSuit :: Card -> CardSuit
getCardSuit (Card suit _) = suit

getCardSuits :: [Card] -> [CardSuit]
getCardSuits = map getCardSuit


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
cardRankSuccessor :: CardRank -> CardRank
cardRankSuccessor rank
    | rank == King = Ace
    | otherwise = succ rank -- Rank is treated as an enum


getHighestCard :: [Card] -> Card
getHighestCard = maximum


-- Returns a list of the ranks of cards
-- Assumes Ace is LOW
sortByCardRanksAceLow :: [Card] -> [Card]
sortByCardRanksAceLow = sort

-- Returns a list of the ranks of cards
-- Assumes Ace is HIGH
sortByCardRanksAceHigh :: [Card] -> [Card]
sortByCardRanksAceHigh cards = do
    let (x:xs) = sort cards
    xs ++ [x]

groupByCardRanksAceLow :: [Card] -> [[Card]]
groupByCardRanksAceLow cards = group (sortByCardRanksAceLow cards)


-- Gets the third element from a tuple (similar to 'fst' and 'snd')
third :: (a, b, c) -> c
third (_, _, c) = c


-- If there is one group with a length of 2 (2 of the cards have the same rank) then it is a Pair
getOnePair :: [Card] -> Maybe [Card]
getOnePair cards = case filter (\x -> length x == 2) (groupByCardRanksAceLow cards) of
    [pair] -> Just pair    -- one pair
    _      -> Nothing

-- If there are 2 groups with a length of 2 then there are 2 Pairs - a Two Pair
getTwoPair :: [Card] -> Maybe [Card]
getTwoPair cards = case filter (\x -> length x == 2) (groupByCardRanksAceLow cards) of
    [pair1, pair2] -> Just (pair1 ++ pair2)
    _              -> Nothing

-- If there is a group of length 3 then it is a Three of a Kind
getThreeOfAKind :: [Card] -> Maybe [Card]
getThreeOfAKind cards = case filter (\x -> length x == 3) (groupByCardRanksAceLow cards) of
    [threeOfAKind] -> Just threeOfAKind
    _        -> Nothing

-- If the cards are consecutive then it is a Straight. However, Ace can be low or high, so both instances must be checked
getStraight :: [Card] -> Maybe [Card]
getStraight cards =
    if correctSuccessors (zip sortedRanks (tail sortedRanks)) || isAcePresent cards && correctSuccessors (zip sortedRanksAceHigh (tail sortedRanksAceHigh)) then
        Just cards
    else
        Nothing

    where
        correctSuccessors = all (\(Card _ r1, Card _ r2) -> cardRankSuccessor r1 == r2)
        sortedRanks = sortByCardRanksAceLow cards
        sortedRanksAceHigh = sortByCardRanksAceHigh cards

-- If there is 1 group then all the cards have the same suit. This is a Flush
getFlush :: [Card] -> Maybe [Card]
getFlush cards = if length (group (getCardSuits cards)) == 1 then Just cards else Nothing

-- If there are 2 groups, one of length 2 and one of length 3, then there is a Pair and a Three of a Kind, known as a Full House
getFullHouse :: [Card] -> Maybe [Card]
getFullHouse cards =
    case sort (map length (groupByCardRanksAceLow cards)) of
        [2, 3] -> Just cards
        _      -> Nothing

-- If there is a group of length 4 (4 of the same rank) then it is a Four of a Kind
getFourOfAKind :: [Card] -> Maybe [Card]
getFourOfAKind cards = case filter (\x -> length x == 4) (groupByCardRanksAceLow cards) of
    [fourOfAKind] -> Just fourOfAKind
    _             -> Nothing

-- If a hand is a Straight and a Flush then it is a Straight Flush
getStraightFlush :: [Card] -> Maybe [Card]
getStraightFlush cards = if isJust (getStraight cards) && isJust (getFlush cards) then Just cards else Nothing

-- If a hand is a Flush and the ranks of the cards are Ten through Ace (high) then it's a Royal Flush
getRoyalFlush :: [Card] -> Maybe [Card]
getRoyalFlush cards =
    if isJust (getFlush cards) && getCardRanks (sortByCardRanksAceHigh cards) == [Ten, Jack, Queen, King, Ace] then
        Just cards
    else
        Nothing


-- Returns the best hand ranking from a given list of cards, along with the cards that form that hand
evaluateHandRanking :: [Card] -> ([Card], HandRank)
evaluateHandRanking [] = error "No cards to evaluate"
evaluateHandRanking cards =
    case getRoyalFlush cards of
    Just hand -> (hand, RoyalFlush)
    Nothing -> case getStraightFlush cards of
        Just hand -> (hand, StraightFlush)
        Nothing -> case getFourOfAKind cards of
            Just hand -> (hand, FourOfAKind)
            Nothing -> case getFullHouse cards of
                Just hand -> (hand, FullHouse)
                Nothing -> case getFlush cards of
                    Just hand -> (hand, Flush)
                    Nothing -> case getStraight cards of
                        Just hand -> (hand, Straight)
                        Nothing -> case getThreeOfAKind cards of
                            Just hand -> (hand, ThreeOfAKind)
                            Nothing -> case getTwoPair cards of
                                Just hand -> (hand, TwoPair)
                                Nothing -> case getOnePair cards of
                                    Just hand -> (hand, OnePair)
                                    Nothing -> (cards, HighCard)


-- Given a list of cards and their associated hand rank, returns a filtered list containing
-- only the hands of the best rank in the list
filterByBestPrimaryHandRanking :: [([Card], HandRank)] -> [([Card], HandRank)]
filterByBestPrimaryHandRanking [] = error "No hands to evaluate"
filterByBestPrimaryHandRanking hands = 
    let sortedHandRankings = sortBy (flip (comparing snd)) hands
        highestRank = snd (head hands)
    in filter (\(_, rank) -> rank == highestRank) sortedHandRankings


filterByPrimaryHandValues :: [([Card], HandRank)] -> [([Card], HandRank)]
filterByPrimaryHandValues [] = error "No hands to evaluate"
filterByPrimaryHandValues hands = 
    let
        comparePrimary :: ([Card], HandRank) -> ([Card], HandRank) -> Ordering
        comparePrimary (primary1, rank1) (primary2, rank2) = 
            case rank1 of
                OnePair -> comparing getPairValue (primary1, rank1) (primary2, rank2)
                _       -> EQ

        rankToValue :: Card -> Int
        rankToValue (Card _ rank) = fromEnum rank

        -- Determine the highest primary hand
        bestHand = maximumBy comparePrimary hands
        bestPrimaryValue = fst bestHand

        getPairValue :: ([Card], HandRank) -> Int
        getPairValue (primary, _) = rankToValue (head primary) -- Pair value is the rank of the first card in the pair

    in filter (\(primary, _) -> primary == bestPrimaryValue) hands     


-- Adds the highest of the remaining cards to complete the 5-card hand 
completeFiveCardHand :: ([Card], HandRank) -> [Card] -> ([Card], [Card], HandRank)
completeFiveCardHand (primaryHand, handRank) allCards =
    let remaining = filter (`notElem` primaryHand) allCards
        sortedRemaining =
            if isAcePresent remaining
            then reverse (sortByCardRanksAceHigh remaining)
            else reverse (sortByCardRanksAceLow remaining)
        kickers = take (5 - length primaryHand) sortedRemaining
    in (primaryHand, kickers, handRank)


-- Add kickers to each hand and return the updated hands.
addKickers :: [Card] -> [([Card], HandRank)] -> [([Card], [Card], HandRank)]
addKickers allCards = map (\hand -> completeFiveCardHand hand allCards)


evaluateBestHand :: [Card] -> ([Card], [Card], HandRank)
evaluateBestHand [] = error "No cards to evaluate"
evaluateBestHand cards =
    let
        allCombinations = cardCombinations 5 cards

        allRankedPrimaryHands = map evaluateHandRanking allCombinations
        
        allRankedPrimaryHandsUnique = map head (group (sortByHandRank allRankedPrimaryHands))

        bestPrimaryHandsByRanking = filterByBestPrimaryHandRanking allRankedPrimaryHandsUnique
        
        [bestHand] = filterByPrimaryHandValues bestPrimaryHandsByRanking
    
        bestFiveCardHand = completeFiveCardHand bestHand cards


        sortByHandRank :: [([Card], HandRank)] -> [([Card], HandRank)]
        sortByHandRank hands = reverse (sortBy (comparing snd) hands)
    
    in bestFiveCardHand


-- Returns the best possible hand formed from a list of cards. i.e. the hand formed using a players hole cards and the community cards
evaluateHand :: Player -> State GameState ()
evaluateHand player = do
    gameState <- get

    let comCards = case communityCards gameState of
                  PreFlop -> []
                  Flop cards -> cards
                  Turn cards -> cards
                  River cards -> cards

    let allCards = comCards ++ holeCards player 

    let (primaryHand, kickers, handrank) = evaluateBestHand allCards

    let updatedPlayer = player {hand = Just (primaryHand, kickers, handrank)}
    let updatedPlayers = map (\p -> if name p == name player then updatedPlayer else p) (activePlayers gameState)

    updateActivePlayers updatedPlayers




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


runInitialisation :: GameState
runInitialisation = execState initialiseGame (GameState [] (Deck []) PreFlop 0 [] 0 0 0)

initialiseGame :: State GameState ()
initialiseGame = do
    -- Generate the deck and shuffle it
    let deck = createDeck
    let shuffledDeck = shuffleDeck 15 deck

    -- Generate 5 random players
    let player1 = Player { name = "Player 1", holeCards = [], hand = Nothing, chips = 0, isDealer = True,  behaviour = RandomPlayer }
    let player2 = Player { name = "Player 2", holeCards = [], hand = Nothing, chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player3 = Player { name = "Player 3", holeCards = [], hand = Nothing, chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player4 = Player { name = "Player 4", holeCards = [], hand = Nothing, chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player5 = Player { name = "Player 5", holeCards = [], hand = Nothing, chips = 0, isDealer = False, behaviour = RandomPlayer }

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


main :: IO ()
main = do
    let gameState = runInitialisation

    print gameState