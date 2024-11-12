{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module HoldEm where
    import System.Random ( randomRIO )
    import Data.List ( delete, groupBy, sortBy, minimumBy )
    import Data.Maybe (fromMaybe)
    import Control.Monad

    data Suit = Hearts 
              | Spades 
              | Diamonds 
              | Clubs deriving(Show, Ord, Eq, Bounded, Enum)

    data CardVal = LowAce 
                | Two 
                | Three 
                | Four 
                | Five 
                | Six 
                | Seven 
                | Eight 
                | Nine 
                | Ten 
                | Jack 
                | Queen 
                | King 
                | Ace  deriving(Show, Ord,  Eq, Bounded, Enum)

    generateDeck :: Deck
    generateDeck = [(val, suit) | suit <- [succ (minBound :: Suit) ..], 
                      val <- [succ (minBound :: CardVal) ..]]

    type Card = (CardVal, Suit)

    type Deck = [Card]

    data Strategy = RandomPlayer 
                  | AggressivePlayer 
                  | SmartPlayer deriving(Show, Eq)

    data Player = Player { name :: String, 
                           hand :: [Card], 
                           chips :: Int, 
                           strategy :: Strategy} deriving(Show, Eq)

    type Bet = (Player, Int)

    data GameState = GameState { nonBustPlayers :: [Player], 
                                 playersInRound :: [Int], 
                                 deck :: Deck, 
                                 communityCards :: [Card],
                                 currentPot :: Int, 
                                 bets :: [Bet], 
                                 currentDealerIndex :: Int, 
                                 smallBlind :: Int,
                                 bigBlind :: Int} deriving(Show)

    data Deal = Community | Hole  deriving(Eq)

    dealCards :: Deal -> GameState -> IO GameState
    dealCards deal state = do
      if deal == Hole then return
        state {
          nonBustPlayers = dealToPlayersHole thePlayers theDeck,
          deck = drop (2*length thePlayers) theDeck}
      else
       if null theComCards then
        return state { 
           deck = drop 3 theDeck,
           communityCards = take 3 theDeck,
           nonBustPlayers = dealToPlayersCommunity thePlayers (take 3 theDeck) }
       else
        return state { 
             deck = tail theDeck,
             communityCards = theComCards ++ [head theDeck],
             nonBustPlayers = dealToPlayersCommunity thePlayers [head theDeck] }
      where
        theDeck = deck state
        thePlayers = nonBustPlayers state
        theComCards = communityCards state

    dealToPlayersHole :: [Player] -> Deck -> [Player]
    dealToPlayersHole [] deck = []
    dealToPlayersHole (p:ps) (c1:c2:cs) = p { hand = hand p ++ [c1, c2] } 
                                          : dealToPlayersHole ps cs

    dealToPlayersCommunity :: [Player] -> [Card] -> [Player]
    dealToPlayersCommunity[] deck = []
    dealToPlayersCommunity (p:ps) cs= p { hand = hand p ++ cs} 
                                      : dealToPlayersCommunity ps cs

    type MyList = [Int]

    randomInt :: Int -> Int -> IO Int
    randomInt x y = randomRIO (x, y)

    shuffleNums :: [Int] -> IO [Int]
    shuffleNums [] = return []
    shuffleNums nums = do
      ind <- randomInt 0 (length nums - 1)
      let outputList = [nums!!ind]
      rest <- shuffleNums (take ind nums ++ drop (ind+1) nums)
      return (outputList ++ rest)

    shuffleDeck :: GameState -> IO GameState
    shuffleDeck state = do
      let theDeck = deck state
      randomIndicies <- shuffleNums [0..(length theDeck -1)]
      let newDeck = [theDeck !! i | i <- randomIndicies]
      return state { deck = newDeck }

    data PokerHand = HighCard {cards :: [Card]}
                   | Pair {cards :: [Card]}
                   | TwoPair {cards :: [Card]}
                   | ThreeOfAKind {cards :: [Card]}
                   | Straight {cards :: [Card]}
                   | Flush {cards :: [Card]}
                   | FullHouse {cards :: [Card]}
                   | FourOfAKind {cards :: [Card]}
                   | StraightFlush {cards :: [Card]}
                   | RoyalFlush {cards :: [Card]} deriving(Show, Eq, Ord)

    evaluateHand :: [Card] -> PokerHand
    evaluateHand xs | length xs == 2 = evaluateHoleHand xs
                    | length longestStraight >= 5 &&
                      longestStraight == highestFlush =
                        getTypeOfStraightFlush xs longestStraight
                    | length highestKind == 4 =
                        FourOfAKind highestKind
                    | length highestKind == 3 &&
                      length sndHighestKind >= 2 =
                        FullHouse (highestKind ++
                          drop (length sndHighestKind - 2) sndHighestKind)
                    | length highestFlush >= 5 =
                        Flush (drop (length highestFlush - 5) highestFlush)
                    | length longestStraight >= 5 =
                        Straight (
                            drop (length longestStraight - 5) longestStraight)
                    | length highestKind == 3 =
                        ThreeOfAKind (highestKind ++
                          concat kindsGroupedWithoutHighest)
                    | length highestKind == 2 && 
                      length sndHighestKind == 2 = 
                        TwoPair (
           getTwoPairHand highestKind sndHighestKind kindsGroupedWithoutHighest)
                    | length highestKind == 2 = 
                        Pair (highestKind ++ concat kindsGroupedWithoutHighest)
                    | otherwise = HighCard kindsSorted

      where
        kindsSorted = sortByKind xs
        kindsGrouped = groupBy (\a b -> fst a == fst b) kindsSorted
        suitsGrouped = groupBy (\a b -> snd a == snd b) 
                             (sortBy (\(_, a) (_, b)-> compare a b) kindsSorted)
        longestStraight = getLongestStraight (sortByKind (addLowAces xs))
        highestFlush = getMaxSizeList suitsGrouped
        highestKind = getMaxSizeList kindsGrouped
        kindsGroupedWithoutHighest = delete highestKind kindsGrouped
        sndHighestKind = getMaxSizeList kindsGroupedWithoutHighest

    sortByKind :: [Card] -> [Card]
    sortByKind = sortBy (\(a, _) (b, _)-> compare a b)

    evaluateHoleHand :: [Card] -> PokerHand
    evaluateHoleHand xs | fst (head xs) == fst (xs!!1) = Pair xs
                        | fst (head xs) > fst (xs!!1) = HighCard [head xs]
                        | otherwise = HighCard [xs!!1]

    getTypeOfStraightFlush :: [Card] -> [Card] -> PokerHand
    getTypeOfStraightFlush xs straight | fst (last straight) == Ace = 
                                          RoyalFlush straight
                                       | otherwise = 
                                          StraightFlush straight

    getTwoPairHand :: [Card] -> [Card] -> [[Card]] -> [Card]
    getTwoPairHand highestKind sndHighestKind kindsGroupedWithoutHighest =
      highestKind ++ 
      sndHighestKind ++ 
      getMaxSizeList (delete sndHighestKind kindsGroupedWithoutHighest)

    getLongestStraight :: [Card] -> [Card]
    getLongestStraight xs = getMaxSizeList groupedByStraight
      where groupedByStraight = groupByStraight (map (\x -> [x]) xs)

    groupByStraight :: [[Card]] -> [[Card]]
    groupByStraight [x] = [x]
    groupByStraight (x:y:xs) | (xVal /= maxBound) && (succ xVal == yVal) = 
                                groupByStraight ((x ++ y) : xs)
                             | otherwise = 
                                x : groupByStraight (y:xs)
      where
        xVal = fst (last x)
        yVal = fst (last y)

    getMaxSizeList :: Ord a => [[(a, b)]] -> [(a, b)]
    getMaxSizeList [x] = x
    getMaxSizeList (x:y:xs) | length x > length y = getMaxSizeList (x:xs)
                            | length y > length x = getMaxSizeList (y:xs)
                            | otherwise = 
                                if fst (last x) > fst (last y) then 
                                  getMaxSizeList (x:xs) 
                                else getMaxSizeList (y:xs)

    kickerCardCompare :: [Card] -> [Card] -> Ordering
    kickerCardCompare xs ys = compare (map fst xs) (map fst ys)

    determineWinner :: GameState -> [(Player, PokerHand)]
    determineWinner state = winner : filter 
              (\(_,a) -> kickerCardCompare (cards a) (cards (snd winner)) == EQ) 
              (tail winnersRanked)
      where
        hands = [(x, evaluateHand (hand x)) | x <- nonBustPlayers state]
        winnersRanked = sortBy (\(_, a) (_, b)-> compare b a)  hands
        winner = head winnersRanked

    addLowAces :: [Card] -> [Card]
    addLowAces [] = []
    addLowAces (x:xs) | fst x == Ace = x :(LowAce, snd x) : addLowAces xs
                      | otherwise = x : addLowAces xs

    main :: IO ()
    main = do
      let player1 = Player {name="wii matt", hand=[], chips=100, 
                            strategy=RandomPlayer}
          player2 = Player {name="gwilym", hand=[], chips=100, 
                            strategy=RandomPlayer}
          player3 = Player {name="miguel", hand=[], chips=100, 
                            strategy=RandomPlayer}
          players = [player1, player2, player3]
          state = GameState {nonBustPlayers=players, 
                             playersInRound=[0..length players -1], 
                             deck=generateDeck, 
                             communityCards=[], 
                             currentPot=0, 
                             bets=[], 
                             currentDealerIndex=0, 
                             smallBlind=10, 
                             bigBlind=20}
      putStrLn (concat (replicate 100 "*"))
      putStrLn "STARTING GAME"
      gameLoop state

    gameLoop :: GameState -> IO ()
    gameLoop state = do
      state <- playRound state
      return ()

    playRound :: GameState -> IO GameState
    playRound state = do
      putStrLn "STARTING NEW ROUND"
      state <- return state { nonBustPlayers = 
                                clearPlayerHands (nonBustPlayers state)}
      state <- shuffleDeck state
      state <- dealCards Hole state
      bettingRound state
      -- state <- dealCards Community state
      -- state <- dealCards Community state
      -- state <- dealCards Community state

    clearPlayerHands :: [Player] -> [Player]
    clearPlayerHands ps = [x { hand = [] } | x <- ps]

    bettingRound :: GameState -> IO GameState
    bettingRound state = do
      let players = nonBustPlayers state
          dealer = currentDealerIndex state 
          playersInBetOrder = drop (dealer+1) players ++ take (dealer+1) players
      state <- return state {bets = [(x, 0) | x <- players]}
      print players
      print playersInBetOrder 
      doPlayerBets state playersInBetOrder
      return state

    doPlayerBets :: GameState -> [Player] -> IO GameState
    doPlayerBets state [] = do
      let playersStillIn = 
              correctTurnOrder [p | (i, p) <- zip [0..] (nonBustPlayers state), 
                                  i `elem` playersInRound state] (bets state)
          highestBet = getBetToCall (bets state)
          playersWhoNeedToCall = [pc | (pc, bet) <- bets state, 
                                 elem pc playersStillIn && bet < snd highestBet]

      print playersStillIn
      if null playersWhoNeedToCall then do
        return state {playersInRound = [0..length (nonBustPlayers state) -1]}
      else
        doPlayerBets state playersWhoNeedToCall

    doPlayerBets state (p:ps) = do
          playerBet <- bet p (bets state)
          let playerIndex = getPlayerIndex p (nonBustPlayers state)
          print playerIndex
          if playerBet == Nothing then do
            state <- return state {playersInRound = 
                                     delete playerIndex (playersInRound state) }
            doPlayerBets state ps
          else do
            let theBet = fromMaybe (p, 0) playerBet
                updatedBets = updateBetValue (bets state) theBet
                outdatedNonBustPlayers = nonBustPlayers state
            state <- return state {
              nonBustPlayers = 
                   updatePlayersChips playerIndex theBet outdatedNonBustPlayers, 
              bets = updatedBets }
            doPlayerBets state ps

    bet :: Player -> [Bet] -> IO (Maybe Bet)
    bet p bs | strategy p == RandomPlayer = do
                  bet <- betRandom p ourCurrentBet betToCall
                  when (bet == Nothing) $ do
                          putStr (name p)
                          putStrLn " HAS FOLDED"
                  return bet
             | otherwise = return Nothing
      where
        betToCall = snd (getBetToCall bs)
        ourCurrentBet = snd (head (filter (\(a, _) -> a == p) bs))

    correctTurnOrder :: [Player] -> [Bet] -> [Player]
    correctTurnOrder ps bs = drop (lastBetIndex+1) ps ++ take (lastBetIndex+1) ps
      where 
        highestBet = getBetToCall bs
        lastBetIndex = head [i | (i, p) <- zip [0..] ps, p == fst highestBet]

    updatePlayersChips :: Int -> Bet -> [Player] -> [Player]
    updatePlayersChips plIndex bet outdatedNonBustPlayers = 
        take plIndex outdatedNonBustPlayers ++ 
        [fst bet] ++ 
        drop (plIndex+1) outdatedNonBustPlayers

    getPlayerIndex :: Player -> [Player] -> Int
    getPlayerIndex pl [] = error "no player"
    getPlayerIndex pl (p:ps) | hand p == hand pl = 0
                             |  otherwise = 1 + getPlayerIndex pl ps

    updateBetValue :: [Bet] -> Bet -> [Bet]
    updateBetValue bs b = map (\(a, v) -> if hand a == hand (fst b) then 
                                            (fst b, v + snd b) 
                                          else (a, v)) bs
      where betVal = snd b

    getBetToCall :: [Bet] -> Bet
    getBetToCall bs = minimumBy (\ (_, a) (_, b) -> compare b a) bs

    randomPercentage :: IO Float
    randomPercentage = do randomRIO (0.0, 1.0)

    damper :: Float
    damper = 0.85

    betRandom :: Player -> Int -> Int -> IO (Maybe Bet)
    betRandom player ourCurrentBet betToCall = do
      putStrLn ""
      putStr "bet to call is"
      print betToCall
      foldChance <- randomInt 1 100
      if foldChance < 15 then return Nothing
      else do

        let chipsLeft = chips player
            callToMake = betToCall - ourCurrentBet
        if callToMake > chipsLeft then return Nothing
        else do

          let betAmount = callToMake
          callOrRaiseChance <- randomInt 1 100

          if chipsLeft /= callToMake && callOrRaiseChance > 50 then do

            amountRaisedPercentage <- randomPercentage
            let raise = ceiling (fromIntegral 
                      (chips player - callToMake)*amountRaisedPercentage*damper)
                totalAmountBet = betAmount + raise

            putStr (name player)
            putStr " HAS BET "
            print totalAmountBet
            when (callToMake /= 0) $ do
              putStr "TO RAISE BY "
              print raise
            player <- return player {chips = chipsLeft - totalAmountBet}
            let bet = Just (player, totalAmountBet)
            return bet
          else do

            putStr (name player)
            if betAmount == 0 then do
              putStrLn " HAS CHECKED"
            else do
              putStr " HAS BET "
              print betAmount
              when (callToMake /= 0) $ do
                putStrLn " TO CALL"

            player <- return player {chips = chipsLeft - betAmount}
            let bet = Just (player, betAmount)
            return bet