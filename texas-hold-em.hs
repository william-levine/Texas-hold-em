import System.Random
import Data.List


data Suit = Clubs|Hearts|Spades|Diamonds deriving Show
data Rank = Ace|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Jack|Queen|King deriving Show

data Card = Card Suit Rank

-- 'Card' is an instance of 'Show', allowing each playing card to be displayed coherently
instance Show Card where
    show :: Card -> String
    show (Card suit value) = show value ++ " of " ++ show suit

newtype Deck = Deck [Card] deriving Show


createDeck :: Deck
createDeck = Deck [Card suit rank |
    suit <- [Clubs,Hearts,Spades,Diamonds],
    rank <- [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]]


shuffleDeck :: Int -> Deck -> Deck
shuffleDeck n (Deck cards) = Deck [ x | (x, a) <- sortBy cmp (zip cards (randoms (mkStdGen n) :: [Int]))]
    where
        cmp :: (a, Int) -> (a, Int) -> Ordering
        cmp (_, x) (_, y) = compare x y




main :: IO ()
main = do
    let Deck deck = createDeck
    print deck