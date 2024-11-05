
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
    -- type Hand = [Card]
    data Strategy = RandomPlayer | AggressivePlayer | SmartPlayer deriving(Show)

    data Player = Player { name :: String, hand :: [Card], chips :: Int, isDealer :: Bool,
      strategy :: Strategy} deriving(Show)

    type Bet = (Player, Int)
    type CurrentPlayers = [Player]

    data GameState = GameState { activePlayers :: CurrentPlayers , deck :: Deck, communityCards :: [Card],
      currentPot :: Int, bets :: [Bet], currentDealerIndex :: Int, smallBlind :: Int,
      bigBlind :: Int} deriving(Show)

    data Deal = Community | Hole  deriving(Eq)
    dealCards :: Deal -> GameState -> IO GameState
    dealCards deal state = do
      if deal == Hole then return
        state {
          activePlayers = dealToPlayersHole thePlayers theDeck,
          deck = drop (2*length thePlayers) theDeck}
      else
       if null theComCards then
        return state { deck = drop 3 theDeck,
                communityCards = take 3 theDeck,
                activePlayers = dealToPlayersCommunity thePlayers (take 3 theDeck) }
       else
        return state { deck = tail theDeck,
                      communityCards = theComCards ++ [head theDeck],
                      activePlayers = dealToPlayersCommunity thePlayers [head theDeck] }
      where
        theDeck = deck state
        thePlayers = activePlayers state
        theComCards = communityCards state

    dealToPlayersHole :: CurrentPlayers -> Deck -> CurrentPlayers
    dealToPlayersHole [] deck = []
    dealToPlayersHole (p:ps) (c1:c2:cs) = p { hand = hand p ++ [c1, c2] } : dealToPlayersHole ps cs

    dealToPlayersCommunity :: CurrentPlayers -> [Card] -> CurrentPlayers
    dealToPlayersCommunity[] deck = []
    dealToPlayersCommunity (p:ps) cs= p { hand = hand p ++ cs} : dealToPlayersCommunity ps cs

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
    evaluateHand xs | length xs == 2 =
                        if fst (head xs) == fst (xs!!1) then Pair xs
                        else
                          if  fst (head xs) > fst (xs!!1) then HighCard [head xs]
                          else HighCard [xs!!1] --for only hole deal
                    | length theLongestStraight >= 5 && theLongestStraight == highestFlush =
                        if fst (last theLongestStraight) == Ace then RoyalFlush theLongestStraight
                        else StraightFlush theLongestStraight
                    | length highestKind == 4 = FourOfAKind highestKind
                    | length highestKind == 3 && length secondHighestKind >= 2 = FullHouse (highestKind ++ drop (length theLongestStraight - 2) secondHighestKind)
                    | length highestFlush >= 5 = Flush (drop (length highestFlush - 5) highestFlush)
                    | length theLongestStraight >= 5 = Straight (drop (length theLongestStraight - 5) theLongestStraight)
                    | length highestKind == 3 = ThreeOfAKind highestKind
                    | length highestKind == 2 && length secondHighestKind == 2 =
                      TwoPair (highestKind ++ secondHighestKind ++ getMaxSizeList (delete secondHighestKind groupedByKindWithoutHighestKind) ++ secondHighestKind)
                    | length highestKind == 2 = Pair (highestKind ++ concat groupedByKindWithoutHighestKind)
                    | otherwise = HighCard sortedByKind

      where
        sortedByKind = sortBy (\(a, _) (b, _)-> compare a b) xs
        groupedByKind = groupBy (\a b -> fst a == fst b) sortedByKind
        groupedBySuit = groupBy (\a b -> snd a == snd b) (sortBy (\(_, a) (_, b)-> compare a b) sortedByKind)
        theLongestStraight = longestStraight sortedByKind
        highestFlush = getMaxSizeList groupedBySuit
        highestKind = getMaxSizeList groupedByKind
        groupedByKindWithoutHighestKind = delete highestKind groupedByKind
        secondHighestKind = getMaxSizeList groupedByKindWithoutHighestKind


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


    data PokerHand = HighCard {cards :: [Card]} | Pair {cards :: [Card]} | TwoPair {cards :: [Card]} | ThreeOfAKind {cards :: [Card]} | Straight {cards :: [Card]} |
     Flush {cards :: [Card]} | FullHouse {cards :: [Card]} | FourOfAKind {cards :: [Card]} | StraightFlush {cards :: [Card]} | RoyalFlush {cards :: [Card]} deriving(Show, Eq, Ord)

    kickerCardCompare :: [Card] -> [Card] -> Ordering
    kickerCardCompare xs ys = compare (map fst xs) (map fst ys)

    -- instance Ord PokerHand where
    --   compare hand1 hand2 | hand1 /= hand2 = compare hand1 hand2
    --                       | otherwise = kickerCardCompare (cards hand1) (cards hand2)

    determineWinner :: GameState -> [(Player, PokerHand)]
    determineWinner state = winner : filter (\(_,a) -> kickerCardCompare (cards a) (cards (snd winner)) == EQ) (tail winnersRanked)
      where
        hands = [(x, evaluateHand (hand x)) | x <- activePlayers state]
        winnersRanked = sortBy (\(_, a) (_, b)-> compare b a)  hands
        winner = head winnersRanked