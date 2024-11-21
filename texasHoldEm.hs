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
                           strategy :: Strategy,
                           playerIndex :: PlayerIndex} deriving(Show, Eq)

    type PlayerIndex = Int
    type BetQuantity = Int
    type Bet = (PlayerIndex, BetQuantity)

    data GameState = GameState { nonBustPlayers :: [Player],
                                 playersInRound :: [Int],
                                 deck :: Deck,
                                 communityCards :: [Card],
                                 currentPot :: Int,
                                 bets :: [Bet],
                                 currentDealerIndex :: Int,
                                 smallBlind :: Int,
                                 bigBlind :: Int,
                                 allInBets :: [Int] } deriving(Show)

    data Deal = Community | Hole  deriving(Eq)

    dealCards :: Deal -> GameState -> IO GameState
    dealCards deal state = do
      if deal == Hole then do
        return state {
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
                        FourOfAKind (take 5 (highestKind ++
                          concat kindsGroupedWithoutHighest))
                    | length highestKind == 3 &&
                      length sndHighestKind >= 2 =
                        FullHouse (take 5 (highestKind ++
                          drop (length sndHighestKind - 2) sndHighestKind))
                    | length highestFlush >= 5 =
                        Flush (take 5
                            (drop (length highestFlush - 5) highestFlush))
                    | length longestStraight >= 5 =
                        Straight (take 5 (
                            drop (length longestStraight - 5) longestStraight))
                    | length highestKind == 3 =
                        ThreeOfAKind (take 5 (highestKind ++
                          concat kindsGroupedWithoutHighest))
                    | length highestKind == 2 &&
                      length sndHighestKind == 2 =
                        TwoPair (take 5 (
          getTwoPairHand highestKind sndHighestKind kindsGroupedWithoutHighest))
                    | length highestKind == 2 =
                        Pair (take 5 (highestKind ++
                                   reverse (concat kindsGroupedWithoutHighest)))
                    | otherwise = HighCard (take 5 kindsSorted)

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

    determineWinner :: GameState -> [(PlayerIndex, PokerHand)]
    determineWinner state = winner : filter
              (\(_,a) -> kickerCardCompare (cards a) (cards (snd winner)) == EQ)
              (tail winnersRanked)
      where
        players = filter (\x -> playerIndex x `elem` playersInRound state) (nonBustPlayers state)
        hands = [(playerIndex x, evaluateHand (hand x)) | x <- players]
        winnersRanked = sortBy (\(_, a) (_, b)-> compare b a)  hands
        winner = head winnersRanked

    addLowAces :: [Card] -> [Card]
    addLowAces [] = []
    addLowAces (x:xs) | fst x == Ace = x :(LowAce, snd x) : addLowAces xs
                      | otherwise = x : addLowAces xs

    main :: IO ()
    main = do
      let player1 = Player {name="wii matt", hand=[], chips=100,
                            strategy=RandomPlayer, playerIndex=0}
          player2 = Player {name="gwilym", hand=[], chips=100,
                            strategy=RandomPlayer, playerIndex=1}
          player3 = Player {name="miguel", hand=[], chips=100,
                            strategy=RandomPlayer, playerIndex=2}
          players = [player1, player2, player3]
          state = GameState {nonBustPlayers=players,
                             playersInRound=[0..length players -1],
                             deck=[],
                             communityCards=[],
                             currentPot=0,
                             bets=[],
                             currentDealerIndex=0,
                             smallBlind=10,
                             bigBlind=20,
                             allInBets = replicate (length players) 0 }
      putStrLn $ concat (replicate 100 "*")
      putStrLn "STARTING GAME"
      putStrLn ""
      gameLoop state 0

    gameLoop :: GameState -> Int -> IO ()
    gameLoop state count = do
      state <- playRound state
      let players = nonBustPlayers state
      if length players == 1 then do
        putStrLn $ concat (replicate 100 "*")
        putStrLn ""
        putStr $ name (head players) ++ " HAS WON ALL THE CHIPS !!!"
        putStrLn $ "IT TOOK HIM " ++ show count ++ " ROUNDS"
      else
        if count == 2 then do
          putStrLn (concat (replicate 100 "*"))
          putStrLn ""
          let maxChip = maximum [chips p | p <- players]
              winners = [name p | p <- players, chips p == maxChip]
              sortedByChips = sortBy (\p1 p2 -> compare (chips p2) (chips p1)) players
              standings = [name p ++ " WITH " ++ show (chips p) ++ " CHIPS | " |
                          p <- sortedByChips]
          putStrLn "FINAL STANDINGS: "
          putStrLn $ unwords standings
          putStrLn $ "WINNER(S): " ++ show winners
        else do
          let newDealer = (currentDealerIndex state + 1) `mod` length players
          putStrLn $ "new dealer assigned? " ++ show newDealer
          gameLoop state {currentDealerIndex = newDealer} (count+1)

    playRound :: GameState -> IO GameState
    playRound state = do
      putStrLn "STARTING NEW ROUND"
      putStrLn ""
      state <- return state { deck = generateDeck,
                              nonBustPlayers =
                       clearPlayerHands (nonBustPlayers state),
                       playersInRound = [0..(length (nonBustPlayers state) -1)],
                       communityCards = [],
                       allInBets = replicate (length (nonBustPlayers state)) 0}
      state <- shuffleDeck state
      state <- payBlinds state
      state <- initiateBets state Hole
      state <- initiateBets state Community
      state <- initiateBets state Community
      state <- initiateBets state Community
      putStrLn ""
      state <- payout state
      removeBustPlayers state [0..length (nonBustPlayers state)]

    removeBustPlayers :: GameState -> [Int] -> IO GameState
    removeBustPlayers state [] = return state
    removeBustPlayers state (i:is) = do
      if i /= length (nonBustPlayers state) && chips (nonBustPlayers state !! i) == 0 then do
        let player = nonBustPlayers state !! i
        putStrLn $ name player ++ " HAS GONE BUST! BETTER LUCK NEXT TIME, BUDDY"
        let players = delete player (nonBustPlayers state)
        let pIndex = playerIndex player
        state <- return state { nonBustPlayers = map
          (\x -> if playerIndex x > pIndex then
                    x {playerIndex = playerIndex x - 1}
                 else x) players }
        removeBustPlayers state (take (length is -1) is)
      else
        removeBustPlayers state is

    initiateBets :: GameState -> Deal -> IO GameState
    initiateBets state deal = do
      let allIns = filter (/=0) (allInBets state)
      let playersIn = length (playersInRound state)
      if playersIn > 1 then do
        state <- dealCards deal state
        if length allIns < playersIn - 1 then do
          state <- bettingRound state
          putStrLn ""
          putStr $ "CURRENT POT IS " ++ show (currentPot state)
          return state
        else
          return state
      else
        return state

    clearPlayerHands :: [Player] -> [Player]
    clearPlayerHands ps = [x { hand = [] } | x <- ps]

    bettingRound :: GameState -> IO GameState
    bettingRound state = do
      if length (playersInRound state) == 1 then
        return state
      else do
        let players = nonBustPlayers state
            dealer = currentDealerIndex state
            playersInBetOrder = drop (dealer+1) players ++ take (dealer+1) players
            unfoldedPlayers = [p | p <- playersInBetOrder, playerIndex p `elem` playersInRound state]
        state <- return state {bets = [(x, 0) | x <- [0..(length players -1)]]}
        putStrLn ""
        doPlayerBets state unfoldedPlayers

    payBlinds :: GameState -> IO GameState
    payBlinds state = do
      state <- payBlind state sBlind
      payBlind state bBlind
      where
        bBlind = bigBlind state
        sBlind = smallBlind state

    payBlind :: GameState -> Int -> IO GameState
    payBlind state blind = do
      if chips updatedPlayer > 0 then do
        putStr $ name player ++ " HAS BET " ++ show blind
        putStrLn $ " ON THE" ++ blindStr ++ " BLIND"
        let newState = state { nonBustPlayers =
            swap players updatedPlayer blindIndex,
            currentPot = currentPot state + blind }
        return newState
      else do --for all in blind      
        let betPaid = chips updatedPlayer + blind
        newState <- recordAllInBet state (blindIndex, betPaid)
        putStrLn $ " ON THE" ++ blindStr ++ " BLIND"
        return newState { nonBustPlayers =
          swap players (updatedPlayer {chips = 0}) blindIndex,
          allInBets = swap (allInBets state) betPaid blindIndex,
          currentPot = currentPot state + betPaid }

      where
        players = nonBustPlayers state
        dealerIndex = currentDealerIndex state
        blindIndex = if blind == smallBlind state then
           (dealerIndex + 1) `mod` length players
        else
          (dealerIndex + 2) `mod` length players
        blindStr = if blind == smallBlind state then " SMALL" else " BIG"
        player = players !! blindIndex
        updatedPlayer = player {chips = chips player - blind}

    getAllInWinners :: [(PlayerIndex, PokerHand)] -> GameState ->
                                                            [(PlayerIndex, Int)]
    getAllInWinners ps state = sortBy (\(_, a) (_, b) -> compare a b)
                                   [(p, allIns!!p)| (p, _) <- ps, allIns!!p > 0]
      where allIns = allInBets state

    payWinners :: GameState -> [Player] -> [(PlayerIndex, PokerHand)] -> Int
                                                                 -> IO GameState
    payWinners state ps [] potLeft = return state {currentPot = potLeft}
    payWinners state ps (w:ws) potLeft = do
      let allInWins = getAllInWinners ws state
      if null allInWins then do
        let player = players !! fst w
            winning = currentPot state `div` length (w:ws)
            updatedPlayer = player {chips = chips player + winning}
        putStr $ name player ++ " WINS " ++ show winning
        if length ps > 1 then do
          putStrLn $ " CHIPS WITH A HAND OF " ++ show (snd w)
        else do
          putStrLn " CHIPS AS ALL OTHER PLAYERS FOLDED"
        state <- return state {nonBustPlayers =
                                             swap players updatedPlayer (fst w)}
        payWinners state ps ws (potLeft - winning)
      else do
        let allInBet = head allInWins
            winning = snd allInBet*length ps `div` length (w:ws)
            pIndex = fst allInBet
            player = players !! pIndex
            updatedPlayer = player {chips = chips player + winning}
        let hand = head (filter (\(a,_) -> a == pIndex) (w:ws))
        putStr $ name player ++ " WINS " ++ show winning
        putStr $ " CHIPS FROM SIDEPOT WITH A HAND OF " ++ show (snd hand)
        state <- return state {
          nonBustPlayers = swap players updatedPlayer pIndex,
          allInBets = map
                        (\x -> if x >= snd allInBet then x-snd allInBet else x)
                        (allInBets state)}

        payWinners state ps (delete (pIndex, snd hand) (w:ws)) (potLeft - winning)
      where
        players = nonBustPlayers state

    payout :: GameState -> IO GameState
    payout state = do
       payWinners state players winners pot
      where
        players = [nonBustPlayers state!!i | i <- playersInRound state]
        winners = determineWinner state
        pot = currentPot state
        remaining = pot `mod` length winners

    doPlayerBets :: GameState -> [Player] -> IO GameState
    doPlayerBets state [] = do
      let playersStillIn =
              correctTurnOrder [p | (i, p) <- zip [0..] (nonBustPlayers state),
                                  i `elem` playersInRound state] (bets state)
          highestBet = getBetToCall (bets state)
          playersWhoNeedToCall =
            filter (not . wentAllIn state) [nonBustPlayers state!!fst b |
                                            b <- bets state,
                                            fst b `elem` playersInRound state &&
                                              snd b < snd highestBet]
      if null playersWhoNeedToCall then do
        return state
      else
        doPlayerBets state playersWhoNeedToCall

    doPlayerBets state (p:ps) = do
      if length (playersInRound state) /= 1 then do
        if not (skipBecauseOfAllIn state p) then do
          playerBet <- bet p (bets state)
          if playerBet == Nothing then do
            state <- return state {playersInRound =
                                delete (playerIndex p) (playersInRound state) }
            doPlayerBets state ps
          else do
            let theBet = fromMaybe (playerIndex p, 0) playerBet
                updatedBets = updateBetValue (bets state) theBet
                outdatedNonBustPlayers = nonBustPlayers state
            state <- return state {
              nonBustPlayers =
                updatePlayersChips theBet outdatedNonBustPlayers,
                bets = updatedBets,
                currentPot = currentPot state + snd theBet }
            state <- if snd theBet /= 0 && chips p == 0 then do
                        recordAllInBet state theBet
                      else return state
            doPlayerBets state ps
        else
          doPlayerBets state ps
      else
        return state

    wentAllIn :: GameState -> Player -> Bool
    wentAllIn state p = (allInBets state !! playerIndex p) > 0

    skipBecauseOfAllIn :: GameState -> Player -> Bool
    skipBecauseOfAllIn state p | allIns!!playerIndex p > 0 = True
                                | amount == length playersIn - 1  = True
                                | otherwise = False
      where
        allIns = allInBets state
        playersIn = playersInRound state
        amount = length (filter (/=0) allIns)


    bet :: Player -> [Bet] -> IO (Maybe Bet)
    bet p bs | strategy p == RandomPlayer = do
                  bet <- betRandom p ourCurrentBet betToCall
                  when (bet == Nothing) $ do
                          putStr $ name p ++ " HAS FOLDED"
                  return bet
             | otherwise = return Nothing
      where
        betToCall = snd (getBetToCall bs)
        ourCurrentBet = snd (head (filter (\(a, _) -> a == playerIndex p) bs))

    correctTurnOrder :: [Player] -> [Bet] -> [Player]
    correctTurnOrder ps bs = drop (lastBetIndex+1) ps ++
                             take (lastBetIndex+1) ps
      where
        highestBet = getBetToCall bs
        lastBetIndex = head [i | (i, p) <- zip [0..] ps,
                              playerIndex p == fst highestBet]

    updatePlayersChips :: Bet -> [Player] -> [Player]
    updatePlayersChips bet outdatedNonBustPlayers =
      swap outdatedNonBustPlayers updatedPlayer pIndex
      where
        pIndex = fst bet
        player = outdatedNonBustPlayers!!fst bet
        updatedPlayer = player {chips = chips player - snd bet}

    updateBetValue :: [Bet] -> Bet -> [Bet]
    updateBetValue bs b = map (\(a, v) -> if a == fst b then
                                            (a, v + snd b)
                                          else (a, v)) bs

    getBetToCall :: [Bet] -> Bet
    getBetToCall = minimumBy (\ (_, a) (_, b) -> compare b a)

    randomPercentage :: IO Float
    randomPercentage = do randomRIO (0.0, 1.0)

    damper :: Float
    damper = 0.6

    recordAllInBet :: GameState -> Bet -> IO GameState
    recordAllInBet state bet = do
      putStr $ pName ++ " HAS GONE ALL IN WITH BET OF " ++ show (snd bet)
      return state {
        allInBets = swap allInData (snd bet) pIndex }
      where
        pIndex = fst bet
        allInData = allInBets state
        pName = name (nonBustPlayers state !! pIndex)

    swap :: [a] -> a -> Int -> [a]
    swap list item index = take index list ++ [item] ++ drop (index+1) list

    raise :: Player -> Int -> IO Bet
    raise pl call = do 
      amountRaisedPercentage <- randomPercentage
      let raise = ceiling (fromIntegral
                (chips pl - call)*amountRaisedPercentage*damper)
          totalAmountBet = call + raise

      putStr $ name pl ++ " HAS BET " ++ show totalAmountBet
      when (call /= 0) $ do
        putStrLn $ " TO RAISE BY " ++ show raise
      return (playerIndex pl, totalAmountBet)

    callOrCheck :: Player -> Int -> Int -> IO Bet
    callOrCheck pl betAmount callToMake = do
      putStr $ name pl
      if betAmount == 0 then do
        putStrLn " HAS CHECKED"
      else do
        putStr $ " HAS BET " ++ show betAmount
        when (callToMake /= 0) $ do
          putStrLn " TO CALL"
      return (playerIndex pl, betAmount)

    betRandom :: Player -> Int -> Int -> IO (Maybe Bet)
    betRandom player ourCurrentBet betToCall = do
      putStrLn ""
      putStrLn $ "bet to call is " ++ show betToCall
      
      foldChance <- randomInt 1 100
      if foldChance < 10 then return Nothing
      else do

        let chipsLeft = chips player
            callToMake = betToCall - ourCurrentBet
            betAmount = callToMake

        raiseChance <- randomInt 1 100
        if chipsLeft > callToMake && raiseChance > 40 then do
          bet <- raise player callToMake
          return $ Just bet
        else do
          if chipsLeft <= callToMake then do
            allInChance <- randomInt 1 100
            if allInChance > 70 then 
              return $ Just (playerIndex player, chipsLeft)
            else return Nothing
          else do
            bet <- callOrCheck player betAmount callToMake
            return $ Just bet
              