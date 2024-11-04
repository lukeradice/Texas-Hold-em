
module HoldEm where
    import System.Random
    import Data.List

    data Suit = Hearts | Spades | Diamonds | Clubs   deriving(Show, Ord, Eq, Bounded, Enum)
    data CardVal = Two | Three | Four | Five | Six | Seven | Eight |
      Nine | Ten | Jack | Queen | King | Ace  deriving(Show, Ord,  Eq, Bounded, Enum)

    generateDeck :: Deck
    generateDeck = [(cardVal, suit) | suit <- [(minBound :: Suit) ..], cardVal <- [(minBound :: CardVal) ..]]

    type Card = (CardVal, Suit)
    type Deck = [Card]
    type Hand = (Card, Card)
    data Strategy = RandomPlayer | AggressivePlayer | SmartPlayer deriving(Show)

    data Player = Player { name :: String, hand :: Hand, chips :: Int, isDealer :: Bool,
      strategy :: Strategy} deriving(Show)

    type Bet = (Player, Int)
    type CurrentPlayers = [Player]

    data GameState = GameState { activePlayers :: CurrentPlayers , deck :: Deck, communityCards :: [Card],
      currentPot :: Int, bets :: [Bet], currentDealerIndex :: Int, smallBlind :: Int,
      bigBlind :: Int} deriving(Show)


    data Deal = Community | Hole  deriving(Eq)
    dealCards :: Deal -> GameState -> GameState
    dealCards deal state | deal == Hole = state {
                                            activePlayers = dealToPlayers thePlayers theDeck,
                                            deck = drop (2*length thePlayers) theDeck}
                         | null theComCards = state { deck = drop 3 theDeck,
                                                          communityCards = take 5 theDeck}
                         | otherwise = state { deck = tail theDeck,
                                          communityCards = theComCards ++ [head theDeck]}
      where
        theDeck = deck state
        thePlayers = activePlayers state
        theComCards = communityCards state

    dealToPlayers :: CurrentPlayers -> Deck -> CurrentPlayers
    dealToPlayers [] deck = []
    dealToPlayers (p:ps) (c1:c2:cs) = p { hand = (c1, c2) } : dealToPlayers ps cs

    type MyList = [Int]

    shuffleNums :: [Int] -> IO [Int]
    shuffleNums [] = return []
    shuffleNums nums = do
      ind <- randomRIO (0, length nums - 1)
      let outputList = [nums!!ind]
      rest <- shuffleNums (take ind nums ++ drop (ind+1) nums)
      return (outputList ++ rest)

    shuffleDeck :: GameState -> IO GameState
    shuffleDeck state = do
      let theDeck = deck state
      randomIndicies <- shuffleNums [0..(length theDeck -1)]
      let newDeck = [theDeck !! i | i <- randomIndicies]
      return state { deck = newDeck }


    evaluateHand :: [Card] -> PokerHand
    evaluateHand xs | length xs == 2 = if fst (xs!!0) == fst (xs!!1) then TwoPair else HighCard
                    | otherwise = RoyalFlush
      where
        sortedByKind = sortBy (\(a, _) (b, _)-> compare a b) xs
        groupedByKind = groupBy (\a b -> fst a == fst b) sortedByKind
        groupedBySuit = groupBy (\a b -> snd a == snd b) (sortBy (\(_, a) (_, b)-> compare a b) xs)


    longestStraight :: [Card] -> [Card]
    longestStraight xs = getMaxSizeList groupedByStraight
      where groupedByStraight = groupByStraight (map (\x -> [x]) xs) 

    groupByStraight :: [[Card]] -> [[Card]]
    groupByStraight [x] = [x]
    groupByStraight (x:y:xs) | (xVal /= maxBound) && (succ xVal == yVal) = groupByStraight ((x ++ y) : xs) 
                             | otherwise = x : groupByStraight (y:xs)
      where 
        xVal = fst (last x)
        yVal = fst (last y)

    getMaxSizeList :: Ord a => [[(a, b)]] -> [(a, b)]
    getMaxSizeList [x] = x
    getMaxSizeList (x:y:xs) | length x > length y = getMaxSizeList (x:xs)
                            | length y > length x = getMaxSizeList (y:xs)
                            | otherwise = if fst (last x) > fst (last y) then getMaxSizeList (x:xs) else getMaxSizeList (y:xs)

    data PokerHand = HighCard | TwoPair | ThreePair | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush



    -- evaluateHand :: [Card] -> PokerHand
    -- evaluateHand [] = []
    -- evaluateHand xs |

