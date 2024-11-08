
module HoldEm where
    import System.Random ( randomRIO )
    import Data.List ( delete, groupBy, sortBy )

    data Suit = Hearts | Spades | Diamonds | Clubs   deriving(Show, Ord, Eq, Bounded, Enum)
    data CardVal = LowAce | Two | Three | Four | Five | Six | Seven | Eight |
      Nine | Ten | Jack | Queen | King | Ace  deriving(Show, Ord,  Eq, Bounded, Enum)

    generateDeck :: Deck
    generateDeck = [(cardVal, suit) | suit <- [succ (minBound :: Suit) ..], cardVal <- [succ (minBound :: CardVal) ..]]

    type Card = (CardVal, Suit)
    type Deck = [Card]
    data Strategy = RandomPlayer | AggressivePlayer | SmartPlayer deriving(Show)

    data Player = Player { name :: String, hand :: [Card], chips :: Int, isDealer :: Bool,
      strategy :: Strategy} deriving(Show)

    type Bet = (Player, Int)
    type CurrentPlayers = [Player]

    data GameState = GameState { activePlayers :: CurrentPlayers , deck :: Deck, communityCards :: [Card],
      currentPot :: Int, bets :: [Maybe Bet], currentDealerIndex :: Int, smallBlind :: Int,
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

    data PokerHand = HighCard {cards :: [Card]} | Pair {cards :: [Card]} | TwoPair {cards :: [Card]} | ThreeOfAKind {cards :: [Card]} | Straight {cards :: [Card]} |
     Flush {cards :: [Card]} | FullHouse {cards :: [Card]} | FourOfAKind {cards :: [Card]} | StraightFlush {cards :: [Card]} | RoyalFlush {cards :: [Card]} deriving(Show, Eq, Ord)

    evaluateHand :: [Card] -> PokerHand
    evaluateHand xs | length xs == 2 = evaluateHoleHand xs
                    | length theLongestStraight >= 5 && theLongestStraight == highestFlush = determineStaightFlush xs theLongestStraight
                    | length highestKind == 4 = FourOfAKind highestKind
                    | length highestKind == 3 && length secondHighestKind >= 2 = FullHouse (highestKind ++ drop (length theLongestStraight - 2) secondHighestKind)
                    | length highestFlush >= 5 = Flush (drop (length highestFlush - 5) highestFlush)
                    | length theLongestStraight >= 5 = Straight (drop (length theLongestStraight - 5) theLongestStraight)
                    | length highestKind == 3 = ThreeOfAKind highestKind
                    | length highestKind == 2 && length secondHighestKind == 2 = TwoPair (returnTwoPairHand highestKind secondHighestKind groupedByKindWithoutHighestKind)
                    | length highestKind == 2 = Pair (highestKind ++ concat groupedByKindWithoutHighestKind)
                    | otherwise = HighCard sortedByKind

      where
        sortedByKind = sortByKind xs
        groupedByKind = groupBy (\a b -> fst a == fst b) sortedByKind
        groupedBySuit = groupBy (\a b -> snd a == snd b) (sortBy (\(_, a) (_, b)-> compare a b) sortedByKind)
        theLongestStraight = longestStraight (sortByKind (addLowAces xs))
        highestFlush = getMaxSizeList groupedBySuit
        highestKind = getMaxSizeList groupedByKind
        groupedByKindWithoutHighestKind = delete highestKind groupedByKind
        secondHighestKind = getMaxSizeList groupedByKindWithoutHighestKind


    sortByKind :: [Card] -> [Card]
    sortByKind = sortBy (\(a, _) (b, _)-> compare a b)

    evaluateHoleHand :: [Card] -> PokerHand
    evaluateHoleHand xs | fst (head xs) == fst (xs!!1) = Pair xs
                        | fst (head xs) > fst (xs!!1) = HighCard [head xs]
                        | otherwise = HighCard [xs!!1]

    determineStaightFlush :: [Card] -> [Card] -> PokerHand
    determineStaightFlush xs theLongestStraight | fst (last theLongestStraight) == Ace = RoyalFlush theLongestStraight
                                                | otherwise = StraightFlush theLongestStraight

    returnTwoPairHand :: [Card] -> [Card] -> [[Card]] -> [Card]
    returnTwoPairHand highestKind secondHighestKind groupedByKindWithoutHighestKind =
      highestKind ++ secondHighestKind ++ getMaxSizeList (delete secondHighestKind groupedByKindWithoutHighestKind)

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

    kickerCardCompare :: [Card] -> [Card] -> Ordering
    kickerCardCompare xs ys = compare (map fst xs) (map fst ys)

    determineWinner :: GameState -> [(Player, PokerHand)]
    determineWinner state = winner : filter (\(_,a) -> kickerCardCompare (cards a) (cards (snd winner)) == EQ) (tail winnersRanked)
      where
        hands = [(x, evaluateHand (hand x)) | x <- activePlayers state]
        winnersRanked = sortBy (\(_, a) (_, b)-> compare b a)  hands
        winner = head winnersRanked

    addLowAces :: [Card] -> [Card]
    addLowAces [] = []
    addLowAces (x:xs) | fst x == Ace = x :(LowAce, snd x) : addLowAces xs
                      | otherwise = x : addLowAces xs

    -- main :: IO ()
    -- main = do
    --   let player1 = Player {name="wii matt", hand=[], chips=10, isDealer=False, strategy=RandomPlayer}
    --   let player2 = Player {name="gwilym", hand=[], chips=10, isDealer=False, strategy=RandomPlayer}
    --   let player3 = Player {name="miguel", hand=[], chips=10, isDealer=False, strategy=RandomPlayer}
    --   let state = GameState {activePlayers=[player1, player2, player3], deck=generateDeck, communityCards=[], currentPot=0, bets=[], currentDealerIndex=0, smallBlind=10, bigBlind=20}
    --   putStrLn "STARTING GAME"
    --   gameLoop state

    -- gameLoop :: GameState -> IO ()
    -- gameLoop = playRound

    -- playRound :: GameState -> IO GameState
    -- playRound state = do
    --   putStrLn "STARTING NEW ROUND"
    --   state <- return state { activePlayers = clearPlayerHands (activePlayers state)}
    --   state <- shuffleDeck state
    --   state <- dealCards Hole state
    --   bettingRound state

    -- clearPlayerHands :: [Player] -> [Player]
    -- clearPlayerHands ps = [x { hand = [] } | x <- ps]

    -- bettingRound :: GameState -> IO GameState
    -- bettingRound state = do
    --   let players = activePlayers state
    --   let dealer = currentDealerIndex state + 1
    --   let betsThisRound = [(x, 0) | x <- players]
    --   let playersInBetOrder = take dealer players ++ drop dealer players
    --   doPlayerBets state playersInBetOrder betsThisRound
    --   return state

    -- doPlayerBets :: GameState -> [Player] -> [Bet] -> IO GameState
    -- doPlayerBets state ps bs = do
    --    let iobets = [bet p bs | p <- ps]
    --    nonioBets <- sequence iobets
    --    return state {bets = filter (/= Nothing) nonioBets}

    -- bet :: Player -> [Bet] -> IO (Maybe Bet)
    -- bet p bs | strategy p == RandomPlayer = do betRandom p ourCurrentBet betToCall
    --          | otherwise = return Nothing
    --   where
    --     betToCall = snd (minimumBy (\ (_, a) (_, b) -> compare b a) bs)
    --     ourCurrentBet = snd (head filter (\(a, _) -> a == p) bs)

    -- betRandom :: Player -> Int -> Int -> IO (Maybe Bet)
    -- betRandom player ourCurrentBet betToCall = do
    --   foldChance <- randomRIO (1, 100)
    --   if foldChance < 15 then return Nothing
    --   else do
    --     let chipsLeft = chips player
    --         callToMake = betToCall - ourCurrentBet
    --     if callToMake > chipsLeft then return Nothing
    --     else do
    --       let betAmount = callToMake
    --       callOrRaiseChance <- randomRIO (1, 100)
    --       if callOrRaiseChance > 0.5 then do
    --         betAmountPercentage <- randomRIO (0.0, 1.0)
    --         let raise = fromIntegral (round ((chips - callToMake)*betAmountPercentage))
    --             totalAmountBet = betAmount + raise
    --         putStr (name player)
    --         putStr " HAS BETTED "
    --         putStr (show totalAmountBet)
    --         if callToMake /= 0 then
    --           putStr "TO RAISE BY "
    --           putStrLn (show raise)
    --         else putStr ()
    --       else do
    --         putStr (name player)
    --         putStr " HAS BETTED "
    --         putStr betAmount
    --         if callToMake /= 0 then
    --           putStrLn " TO CALL"
    --         else
    --           putStr ()
    --         return Just (player, betAmount)