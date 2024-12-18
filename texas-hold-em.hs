import System.Random
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Ord


data CardSuit = Clubs | Hearts | Spades | Diamonds deriving (Show, Eq)

data CardRank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum, Ord, Bounded)

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
    hand :: ([Card], [Card], HandRank), -- (primary hand, kickers, hand rank)
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


-- Returns all combinations of n cards
cardCombinations :: Int -> [Card] -> [[Card]]
cardCombinations 0 _  = [[]]
cardCombinations _ [] = []
cardCombinations n (x:xs)
    | n > length (x:xs) = []
    | otherwise = map (x:) (cardCombinations (n-1) xs) ++ cardCombinations n xs


isAcePresent :: [Card] -> Bool
isAcePresent = any (\(Card _ rank) -> rank == Ace)


cardRankSuccessor :: CardRank -> CardRank
cardRankSuccessor rank
    | rank == Ace = Two
    | otherwise = succ rank -- Rank is treated as an enum


getHighestCard :: [Card] -> Card
getHighestCard = maximum


compareAceLow :: CardRank -> CardRank -> Ordering
compareAceLow a b
  | a == Ace = LT      -- Ace is considered the lowest
  | b == Ace = GT      -- Ace is considered the lowest
  | otherwise = compare a b  -- Otherwise, use the default comparison

compareAceHigh :: CardRank -> CardRank -> Ordering
compareAceHigh = compare

sortCardRanksAceLow :: [CardRank] -> [CardRank]
sortCardRanksAceLow = sortBy compareAceLow

sortCardRanksAceHigh :: [CardRank] -> [CardRank]
sortCardRanksAceHigh = sort   -- default sort


-- Returns a list of the ranks of cards
-- Assumes Ace is LOW
sortCardsAceLow :: [Card] -> [Card]
sortCardsAceLow = sortBy (\(Card _ rank1) (Card _ rank2) -> compareAceLow rank1 rank2)


-- Returns a list of the ranks of cards
-- Assumes Ace is HIGH
sortCardsAceHigh :: [Card] -> [Card]
sortCardsAceHigh = sortBy (\(Card _ rank1) (Card _ rank2) -> compareAceHigh rank1 rank2)

groupCardsAceLow :: [Card] -> [[Card]]
groupCardsAceLow cards = group (sortCardsAceLow cards)


sortHandsByRank :: [([Card], [Card], HandRank)] -> [([Card], [Card], HandRank)]
sortHandsByRank = sortBy (\(_, _, a) (_, _, b) -> compare b a)


-- Gets the first element from a 3-tuple ('fst' only works for 2-tuples)
first :: (a, b, c) -> a
first (a, _, _) = a

-- Gets the second element from a 3-tuple ('snd' only works for 2-tuples)
second :: (a, b, c) -> b
second (_, b, _) = b

-- Gets the third element from a 3-tuple
third :: (a, b, c) -> c
third (_, _, c) = c


-- If there is one group with a length of 2 (2 of the cards have the same rank) then it is a Pair
getOnePair :: [Card] -> Maybe [Card]
getOnePair cards = case filter (\x -> length x == 2) (groupCardsAceLow cards) of
    [pair] -> Just pair    -- one pair
    _      -> Nothing

-- If there are 2 groups with a length of 2 then there are 2 Pairs - a Two Pair
getTwoPair :: [Card] -> Maybe [Card]
getTwoPair cards = case filter (\x -> length x == 2) (groupCardsAceLow cards) of
    [pair1, pair2] -> Just (pair1 ++ pair2)
    _              -> Nothing

-- If there is a group of length 3 then it is a Three of a Kind
getThreeOfAKind :: [Card] -> Maybe [Card]
getThreeOfAKind cards = case filter (\x -> length x == 3) (groupCardsAceLow cards) of
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
        sortedRanks = sortCardsAceLow cards
        sortedRanksAceHigh = sortCardsAceHigh cards

-- If there is 1 group then all the cards have the same suit. This is a Flush
getFlush :: [Card] -> Maybe [Card]
getFlush cards = if length (group (getCardSuits cards)) == 1 then Just cards else Nothing

-- If there are 2 groups, one of length 2 and one of length 3, then there is a Pair and a Three of a Kind, known as a Full House
getFullHouse :: [Card] -> Maybe [Card]
getFullHouse cards =
    case sort (map length (groupCardsAceLow cards)) of
        [2, 3] -> Just cards
        _      -> Nothing

-- If there is a group of length 4 (4 of the same rank) then it is a Four of a Kind
getFourOfAKind :: [Card] -> Maybe [Card]
getFourOfAKind cards = case filter (\x -> length x == 4) (groupCardsAceLow cards) of
    [fourOfAKind] -> Just fourOfAKind
    _             -> Nothing

-- If a hand is a Straight and a Flush then it is a Straight Flush
getStraightFlush :: [Card] -> Maybe [Card]
getStraightFlush cards = if isJust (getStraight cards) && isJust (getFlush cards) then Just cards else Nothing

-- If a hand is a Flush and the ranks of the cards are Ten through Ace (high) then it's a Royal Flush
getRoyalFlush :: [Card] -> Maybe [Card]
getRoyalFlush cards =
    if isJust (getFlush cards) && getCardRanks (sortCardsAceHigh cards) == [Ten, Jack, Queen, King, Ace] then
        Just cards
    else
        Nothing


-- Returns the best hand ranking from a given list of cards, along with the cards that form that hand
evaluateHandRanking :: [Card] -> ([Card], [Card], HandRank)
evaluateHandRanking [] = error "No cards to evaluate"
evaluateHandRanking cards =
    case getRoyalFlush cards of
    Just hand -> (hand, [], RoyalFlush)
    Nothing -> case getStraightFlush cards of
        Just hand -> (hand, [], StraightFlush)
        Nothing -> case getFourOfAKind cards of
            Just hand -> (hand, [], FourOfAKind)
            Nothing -> case getFullHouse cards of
                Just hand -> (hand, [], FullHouse)
                Nothing -> case getFlush cards of
                    Just hand -> (hand, [], Flush)
                    Nothing -> case getStraight cards of
                        Just hand -> (hand, [], Straight)
                        Nothing -> case getThreeOfAKind cards of
                            Just hand -> (hand, [], ThreeOfAKind)
                            Nothing -> case getTwoPair cards of
                                Just hand -> (hand, [], TwoPair)
                                Nothing -> case getOnePair cards of
                                    Just hand -> (hand, [], OnePair)
                                    Nothing -> (cards, [], HighCard)


-- Given a list of cards and their associated hand rank, returns a filtered list containing only the hands of the best rank in the list
filterByPrimaryHandRanking :: [([Card], [Card], HandRank)] -> [([Card], [Card], HandRank)]
filterByPrimaryHandRanking [] = error "No hands to evaluate"
filterByPrimaryHandRanking hands = 
    let sortedHands = sortHandsByRank hands
        highestRank = third (head sortedHands)
    in filter (\(_, _, rank) -> rank == highestRank) sortedHands


filterByPrimaryHandValues :: [([Card], [Card], HandRank)] -> [([Card], [Card], HandRank)]
filterByPrimaryHandValues [] = error "No hands to evaluate"
filterByPrimaryHandValues hands = 
    let
        compareCards :: ([Card], [Card], HandRank) -> ([Card], [Card], HandRank) -> Ordering
        compareCards (cards1, _, rank1) (cards2, _, rank2) = 
            case rank1 of
                HighCard      -> compareCardLists (sortCardsAceHigh cards1) (sortCardsAceHigh cards2)
                OnePair       -> compare (getPairValue cards1) (getPairValue cards2)
                TwoPair       -> compare (getTwoPairValues cards1) (getTwoPairValues cards2)
                ThreeOfAKind  -> compare (getThreeOfAKindValue cards1) (getThreeOfAKindValue cards2)
                Flush         -> compareCardLists (sortCardsAceHigh cards1) (sortCardsAceHigh cards2)
                FullHouse     -> compare (getFullHouseValues cards1) (getFullHouseValues cards2)
                FourOfAKind   -> compare (getFourOfAKindValue cards1) (getFourOfAKindValue cards2)
                _             -> compare (getCardRank (maximum cards1)) (getCardRank (maximum cards2)) -- Straight, Straight Flush, Royal Flush

        getPairValue :: [Card] -> CardRank
        getPairValue cards = getCardRank (head cards)

        getTwoPairValues :: [Card] -> (CardRank, CardRank)
        getTwoPairValues cards =
            let ranks = map (\(Card suit rank) -> rank) cards
                sortedRanks = sortRanksDescending ranks
                groupedRanks = group sortedRanks
                
                highestPairRank = head (head groupedRanks)
                lowestPairRank = head (last groupedRanks)
            in (highestPairRank, lowestPairRank)

        getThreeOfAKindValue :: [Card] -> CardRank
        getThreeOfAKindValue cards = getCardRank (head cards)

        getFourOfAKindValue :: [Card] -> CardRank
        getFourOfAKindValue cards = getCardRank (head cards)

        getFullHouseValues :: [Card] -> (CardRank, CardRank)
        getFullHouseValues cards =
            let ranks = map (\(Card suit rank) -> rank) cards
                sortedRanks = sortRanksDescending ranks
                groupedRanks = group sortedRanks

                sortedGroupedRanks = sortBy (comparing length) groupedRanks

                pairRank = head (head sortedGroupedRanks)
                threeOfAKindRank = head (last sortedGroupedRanks)

            in (threeOfAKindRank, pairRank)

        sortRanksDescending :: [CardRank] -> [CardRank]
        sortRanksDescending = sortBy (flip compare)

        -- Determine the highest primary hand
        bestCards = maximumBy compareCards hands
        bestCardValue = first bestCards

    in filter (\(cards, _, _) -> cards == bestCardValue) hands 


-- EQ -> cards in each list have the same ranks
-- GT -> cards in the first list have at least 1 card higher
-- LT -> cards in the first list have at least 1 card lower 
compareCardLists :: [Card] -> [Card] -> Ordering
compareCardLists (x:xs) (y:ys) =
    case compare (getCardRank x) (getCardRank y) of
        EQ  -> compareCardLists xs ys
        ord -> ord
compareCardLists [] [] = EQ


-- Useful for comparison of certain hands (Flush and Full House)
-- Also useful for kicker comparisons
filterByHighestCards :: [[Card]] -> [[Card]]
filterByHighestCards cardLists =
    let
        -- Sort the inner lists of cards in reverse order (High -> Low)
        sortedCardLists = map (\cards -> reverse (sortCardsAceHigh cards)) cardLists

        -- Extract the list of cards that contain the highest ranked cards
        bestCards = maximumBy compareCardLists sortedCardLists

        -- Returns True/False depending on whether the input list of cards is equal to the best list of cards
        -- If equal, then the input list of cards is the best list (or one of the equally best lists)
        isBest list = compareCardLists list bestCards == EQ
    in
        -- Filters the sorted lists of cards such that only the best lists remain
        filter isBest sortedCardLists


filterByKickersValues :: [([Card], [Card], HandRank)] -> [([Card], [Card], HandRank)]
filterByKickersValues hands =
    let
        -- Construct a list containing the list of kickers from each hand
        kickers = map (\(_, kickers, _) -> kickers) hands

        -- Get the highest list/s of kickers (the ones that would win)
        highestKickers = filterByHighestCards kickers

        -- Compare the kickers from each hand with the best kickers
        -- If the kickers from the hand exist in the list of the best kickers then that hand would win
        isKickerWinner (_, kickers, _) = kickers `elem` highestKickers
    in
        -- Filter the list of hands to keep those with the best kickers
        filter isKickerWinner hands


-- Wrapper function for 'filterByPrimaryHandRanking'
-- Used to keep track of who a hand belongs to
filterPlayersByPrimaryHandRanking :: [(Player, ([Card], [Card], HandRank))] -> [(Player, ([Card], [Card], HandRank))]
filterPlayersByPrimaryHandRanking playerHands =
    let hands = map snd playerHands
        filteredHands = filterByPrimaryHandRanking hands
    in [ (player, hand) | (player, hand) <- playerHands, hand `elem` filteredHands ]


-- Wrapper function for 'filterByPrimaryHandValues'
-- Used to keep track of who a hand belongs to
filterPlayersByPrimaryHandValues :: [(Player, ([Card], [Card], HandRank))] -> [(Player, ([Card], [Card], HandRank))]
filterPlayersByPrimaryHandValues playerHands =
    let hands = map snd playerHands
        filteredHands = filterByPrimaryHandValues hands
    in [ (player, hand) | (player, hand) <- playerHands, hand `elem` filteredHands ]


-- Wrapper function for 'filterByKickersValues'
-- Used to keep track of who a hand belongs to
filterPlayersByKickers :: [(Player, ([Card], [Card], HandRank))] -> [(Player, ([Card], [Card], HandRank))]
filterPlayersByKickers playerHands =
    let hands = map snd playerHands
        filteredHands = filterByKickersValues hands
    in [ (player, hand) | (player, hand) <- playerHands, hand `elem` filteredHands ]


-- Function to sort the cards within each hand
sortHandCardsByRank :: [([Card], HandRank)] -> [([Card], HandRank)]
sortHandCardsByRank = 
    let
        -- Sort cards with Aces moved to the end
        sortByCardRanks cards =
            let aceHighOrder (Card _ rank1) (Card _ rank2)
                    | rank1 == Ace && rank2 /= Ace = GT  -- Ace goes to the end
                    | rank1 /= Ace && rank2 == Ace = LT  -- Ace goes to the end
                    | otherwise = compare rank1 rank2    -- Otherwise, sort normally
            in sortBy aceHighOrder cards
    in map (\(cards, rank) -> (sortByCardRanks cards, rank))


-- Adds the highest of the remaining cards to complete the 5-card hand 
completeFiveCardHand :: ([Card], [Card], HandRank) -> [Card] -> ([Card], [Card], HandRank)
completeFiveCardHand (primaryHand, _, handRank) allCards =
    let remaining = filter (`notElem` primaryHand) allCards
        sortedRemaining =
            if isAcePresent remaining
            then reverse (sortCardsAceHigh remaining)
            else reverse (sortCardsAceLow remaining)
        kickers = take (5 - length primaryHand) sortedRemaining
    in (primaryHand, kickers, handRank)


-- Add kickers to each hand and return the updated hands.
addKickers :: [Card] -> [([Card], [Card], HandRank)] -> [([Card], [Card], HandRank)]
addKickers allCards = map (\hand -> completeFiveCardHand hand allCards) 


evaluateBestHand :: [Card] -> ([Card], [Card], HandRank)
evaluateBestHand [] = error "No cards to evaluate"
evaluateBestHand cards =
    let
        -- Every 5 card combination from the 7 total cards
        allCombinations = cardCombinations 5 cards

        -- Each possible hand is extracted and ranked. Duplicates removed.
        allRankedPrimaryHands = map evaluateHandRanking allCombinations
        allRankedPrimaryHandsUnique = removeDuplicates allRankedPrimaryHands

        -- Hands are then filtered so that only hands of the best hand ranking remains
        bestPrimaryHandsByRanking = filterByPrimaryHandRanking allRankedPrimaryHandsUnique

        -- Hands are then further filtered by the values (e.g. pair of 7s beats pair of 4s)
        bestPrimaryHandsByValues = filterByPrimaryHandValues bestPrimaryHandsByRanking
        
        -- The best hand is selected as the first hand out of the remaining options
        -- At this point they are all equivalent, so any can be chosen 
        bestHand = head bestPrimaryHandsByValues
    
        -- The kickers are then added to the hand
        -- This is now the best 5 card hand that the player has
        bestFiveCardHand = completeFiveCardHand bestHand cards


        removeDuplicates :: [([Card],[Card], HandRank)] -> [([Card], [Card], HandRank)]
        removeDuplicates hands = map head (group (sortHandsByRank hands))
    
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

    let updatedPlayer = player {hand = (primaryHand, kickers, handrank)}
    let updatedPlayers = map (\p -> if name p == name player then updatedPlayer else p) (activePlayers gameState)

    updateActivePlayers updatedPlayers


determineWinner :: State GameState [Player]
determineWinner = do
    gameState <- get
    
    let players = activePlayers gameState
        hands = map hand players
        playerHands = zip players hands

    let winnersByHandRanking = filterPlayersByPrimaryHandRanking playerHands

    let winnersByCardValues = 
            if length winnersByHandRanking == 1
            then winnersByHandRanking
            else filterPlayersByPrimaryHandValues winnersByHandRanking

    let winners = 
            if length winnersByCardValues == 1 
            then winnersByCardValues
            else filterPlayersByKickers winnersByCardValues

    return (map fst winners)


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
    let player1 = Player { name = "Player 1", holeCards = [], hand = ([],[],HighCard), chips = 0, isDealer = True,  behaviour = RandomPlayer }
    let player2 = Player { name = "Player 2", holeCards = [], hand = ([],[],HighCard), chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player3 = Player { name = "Player 3", holeCards = [], hand = ([],[],HighCard), chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player4 = Player { name = "Player 4", holeCards = [], hand = ([],[],HighCard), chips = 0, isDealer = False, behaviour = RandomPlayer }
    let player5 = Player { name = "Player 5", holeCards = [], hand = ([],[],HighCard), chips = 0, isDealer = False, behaviour = RandomPlayer }

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