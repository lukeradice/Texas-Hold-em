{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module HoldEm where
    import System.Random ( randomRIO )
    import Data.List ( delete, groupBy, sortBy, minimumBy, find )
    import Data.Maybe (fromMaybe)
    import Control.Monad
    import Data.Char (isSpace, toLower, isDigit)
    import Control.Concurrent

    data Suit = Diamonds
              | Spades
              | Hearts
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
    generateDeck = [(val, suit) | suit <- [(minBound :: Suit) ..],
                      val <- [succ (minBound :: CardVal) ..]]

    type Card = (CardVal, Suit)

    type Deck = [Card]

    data Strategy = RandomPlayer
                  | PassivePlayer
                  | AggressivePlayer
                  | SmartPlayer
                  | HumanPlayer deriving(Show, Eq)


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
                                 roundwiseBets :: [Bet],
                                 gamewiseBets :: [Bet],
                                 currentDealerIndex :: Int,
                                 smallBlind :: Int,
                                 bigBlind :: Int,
                                 lastBetterIndex :: Int,
                                 allInBets :: [Int] } deriving(Show)

    data Deal = Community | Hole  deriving(Eq)

    data BetChances = BetChances {
      raiseThreshold :: Int, foldThreshold :: Int, allInCallThreshold :: Int}

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
    evaluateHand xs
      | length xs == 2 = evaluateHoleHand xs kindsSorted
      | length longestStraight >= 5 &&
        longestStraight == highestFlush =
          returnStraightFlush xs longestStraight
      | length highestKind == 4 =
          returnKindHand highestKind kindsGroupedWithoutHighest
      | length highestKind == 3 &&
        length sndHighestKind >= 2 =
          returnFullHouse highestKind sndHighestKind
      | length highestFlush >= 5 = returnFlush highestFlush
      | length longestStraight >= 5 = returnStraight longestStraight
      | length highestKind == 3 =
           returnKindHand highestKind kindsGroupedWithoutHighest
      | length highestKind == 2 && length sndHighestKind == 2 =
          returnTwoPair
            highestKind sndHighestKind kindsGroupedWithoutHighest
      | length highestKind == 2 =
          returnKindHand highestKind kindsGroupedWithoutHighest
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

    returnFullHouse :: [Card] -> [Card] -> PokerHand
    returnFullHouse highestKind sndHighestKind =
      FullHouse (take 5
            (highestKind ++ drop (length sndHighestKind - 2) sndHighestKind))

    returnFlush :: [Card] -> PokerHand
    returnFlush highestFlush = Flush (take 5
                                  (drop (length highestFlush - 5) highestFlush))

    returnStraight :: [Card] -> PokerHand
    returnStraight longestStraight = Straight (take 5 (
                             drop (length longestStraight - 5) longestStraight))

    sortByKind :: [Card] -> [Card]
    sortByKind = sortBy (\(a, _) (b, _)-> compare b a)

    evaluateHoleHand :: [Card] -> [Card] -> PokerHand
    evaluateHoleHand xs kindsSorted
      | fst (head xs) == fst (xs!!1) = Pair xs
      | otherwise = HighCard kindsSorted

    returnStraightFlush :: [Card] -> [Card] -> PokerHand
    returnStraightFlush xs straight | fst (last straight) == Ace =
                                          RoyalFlush straight
                                       | otherwise =
                                          StraightFlush straight

    returnTwoPair :: [Card] -> [Card] -> [[Card]] -> PokerHand
    returnTwoPair highestKind sndHighestKind kindsGroupedWithoutHighest =
      TwoPair (take 5 (highestKind ++ sndHighestKind ++
             getMaxSizeList (delete sndHighestKind kindsGroupedWithoutHighest)))

    returnKindHand :: [Card] -> [[Card]] -> PokerHand
    returnKindHand highestKind kindsGroupedWithoutHighest
      | highKindLength == 4 = FourOfAKind hand
      | highKindLength == 3 = ThreeOfAKind hand
      | highKindLength == 2 = Pair hand
        where
          highKindLength = length highestKind
          hand = take 5 (highestKind ++ concat kindsGroupedWithoutHighest)

    getLongestStraight :: [Card] -> [Card]
    getLongestStraight xs = getMaxSizeList groupedByStraight
      where groupedByStraight = groupByStraight (map (\x -> [x]) xs)

    groupByStraight :: [[Card]] -> [[Card]]
    groupByStraight [x] = [x]
    groupByStraight (x:y:xs) | (yVal /= maxBound) && (succ yVal == xVal) =
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

    compareHand :: PokerHand -> PokerHand -> Ordering
    compareHand hand1 hand2 | hand1{cards=[]} == hand2{cards=[]} =
                                compare
                                  (map fst (cards hand1))
                                  (map fst (cards hand2))
                            | otherwise = compare hand1 hand2

    determineWinner :: GameState -> [(PlayerIndex, PokerHand)]
    determineWinner state = winner : filter
                              (\(_,a) -> compareHand a (snd winner) == EQ)
                              (tail winnersRanked)
      where
        players = filter (\x -> playerIndex x `elem` playersInRound state) (nonBustPlayers state)
        hands = [(playerIndex x, evaluateHand (hand x)) | x <- players]
        winnersRanked = sortBy (\(_, a) (_, b)-> compareHand b a)  hands
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
                            strategy=AggressivePlayer, playerIndex=1}
          player3 = Player {name="miguel", hand=[], chips=100,
                            strategy=SmartPlayer, playerIndex=2}
          players = [player1, player2, player3]
          state = GameState {nonBustPlayers=players,
                             playersInRound=[0..length players -1],
                             deck=[],
                             communityCards=[],
                             currentPot=0,
                             roundwiseBets=[],
                             gamewiseBets=[],
                             currentDealerIndex=0,
                             smallBlind=3,
                             bigBlind=6,
                             lastBetterIndex=0,
                             allInBets = replicate (length players) 0 }
      putStrLn $ concat (replicate 100 "*")
      putStrLn "STARTING GAME\n"
      gameLoop state 0

    gameLoop :: GameState -> Int -> IO ()
    gameLoop state count = do
      state <- playRound state
      let players = nonBustPlayers state
      if length players == 1 then do
        putStrLn $ concat (replicate 100 "*")
        putStr $ "\n" ++ name (head players) ++ " HAS WON ALL THE CHIPS !!!"
        putStrLn $ " IT TOOK HIM " ++ show (count+1) ++ " ROUND(S)"
      else
        if count == 10 then do
          putStrLn $ concat (replicate 100 "*")
          let maxChip = maximum [chips p | p <- players]
              winners = [name p | p <- players, chips p == maxChip]
              sortedByChips = sortBy
                              (\p1 p2 -> compare (chips p2) (chips p1)) players
              standings = [name p ++ " WITH " ++ show (chips p) ++ " CHIPS | "
                            | p <- sortedByChips]
          putStrLn $ "\nFINAL STANDINGS AFTER " ++ show count ++ " ROUNDS:"
          putStrLn $ unwords standings
          putStrLn $ "WINNER(S): " ++ show winners
        else do
          let newDealer = (currentDealerIndex state + 1) `mod` length players
          gameLoop state {currentDealerIndex = newDealer} (count+1)

    playRound :: GameState -> IO GameState
    playRound state = do
      putStrLn "\nSTARTING NEW ROUND\n"
      state <- return state { deck = generateDeck,
                              nonBustPlayers =
                       clearPlayerHands (nonBustPlayers state),
                       playersInRound = [0..(length players -1)],
                       communityCards = [],
                       allInBets = replicate (length players) 0,
                       gamewiseBets = [(x, 0) | x <- [0..(length players -1)]],
                       roundwiseBets = [(x, 0) | x <- [0..(length players -1)]]}
      state <- shuffleDeck state
      state <- payBlinds state
      state <- initiateBets state Hole
      state <- initiateBets state Community
      state <- initiateBets state Community
      state <- initiateBets state Community
      putStrLn ""
      state <- payout state
      removeBustPlayers state [0..length players-1]
      where
        players = nonBustPlayers state

    removeBustPlayers :: GameState -> [Int] -> IO GameState
    removeBustPlayers state [] = return state
    removeBustPlayers state (i:is) = do
      let nonBustPl = nonBustPlayers state
      if i /= length nonBustPl && chips (nonBustPl !! i) == 0 then do
        let player = nonBustPl !! i
        putStrLn $ name player ++ " HAS GONE BUST! BETTER LUCK NEXT TIME, BUDDY"
        let players = delete player nonBustPl
        let pIndex = playerIndex player
        state <- return state { nonBustPlayers = map
          (\x -> if playerIndex x > pIndex then
                    x {playerIndex = playerIndex x - 1}
                 else x) players }
        removeBustPlayers state (i : take (length is -1) is)
      else
        removeBustPlayers state is

    initiateBets :: GameState -> Deal -> IO GameState
    initiateBets state deal = do
      let allIns = filter (/=0) (allInBets state)
      let playersIn = length (playersInRound state)
      if playersIn > 1 then do
        state <- dealCards deal state
        --checking if we should skip to showdown because of all in bets
        if length allIns < playersIn - 1 then do
          state <- bettingRound state
          putStrLn $ "\nCURRENT POT IS " ++ show (currentPot state)
          return state
        else do --despite skipping, inform community card dealing
          putStrLn $ "\nCOMMUNITY CARDS ARE NOW : " ++
                      show (communityCards state)
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
        let comCards = communityCards state
            dealerIndex = currentDealerIndex state
            playersInBetOrder = drop (dealerIndex+1) players ++
                                take (dealerIndex+1) players
            unfoldedPlayers = [p | p <- playersInBetOrder,
                                playerIndex p `elem` playersInRound state]
        if null comCards then do
          putStrLn "\nPRE-FLOP BETTING ROUND"
        else do
          putStrLn $ "\nCOMMUNITY CARDS ARE NOW : " ++ show comCards
        state <- doPlayerBets state unfoldedPlayers
        return state {roundwiseBets = [(x, 0) | x <- [0..(length players -1)]]}
      where
        players = nonBustPlayers state


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
        newState <- recordAllInBet state (blindIndex, -betPaid)
        putStrLn $ "ON THE" ++ blindStr ++ " BLIND"
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

    getMinAllInWinners :: [(PlayerIndex, PokerHand)] -> GameState ->
                                                            [(PlayerIndex, Int)]
    getMinAllInWinners ps state = filter (\x -> snd x == minAllIn) allInWinners
      where
        allIns = allInBets state
        allInWinners =  sortBy (\(_, a) (_, b) -> compare a b)
                                   [(p, allIns!!p)| (p, _) <- ps, allIns!!p > 0]
        minAllIn = snd (head allInWinners)

    payWinners :: GameState -> [Player] -> [(PlayerIndex, PokerHand)] -> Int
                                                         -> Bool -> IO GameState
    payWinners state ps ws potLeft isMainPot = do
      when (length ws > 1) $ do putStrLn "MORE THAN ONE!!!!!!!!!!!!!!"
      let allInWins = getMinAllInWinners ws state
      -- when (length allInWins > 1) $ do putStrLn "MORE THAN ONE!!!!!!!!!!!!!!"
      if null allInWins then do
        let winning = potLeft `div` length ws 
            winningPlayerIndicies = map fst ws
            winnersPayed = map (\x ->
                if playerIndex x `elem` winningPlayerIndicies then
                  x {chips = chips x + winning}
                else x)
                players
        outputPotWinners ps ws winning isMainPot
        -- let winString = if length ps > 1 then 
        --                   unwords [name (ps!!fst w) ++ " WINS " ++ 
        --                           show winning ++
        --                           " CHIPS WITH A HAND OF " ++ 
        --                            show (snd w) ++ "\n"| w <- ws]
        --                 else 
        --                   name (head ps) ++ " WINS " ++ show winning ++ 
        --                     " CHIPS AS ALL OTHER PLAYERS FOLDED"
        -- if isMainPot then do putStrLn "MAIN POT WINNERS:"
        -- else do putStrLn "SIDE POT WINNERS:"
        -- putStrLn winString
        return state {nonBustPlayers = winnersPayed, 
                      currentPot = potLeft  - (winning*length ws)}

        -- let player = players !! fst w
        --     winning = potLeft `div` length (w:ws)
        --     updatedPlayer = player {chips = chips player + winning}
        -- putStr $ name player ++ " WINS " ++ show winning
        -- if length ps > 1 then do
        --   putStrLn $ " CHIPS WITH A HAND OF " ++ show (snd w)
        -- else do
        --   putStrLn " CHIPS AS ALL OTHER PLAYERS FOLDED"
        -- state <- return state {nonBustPlayers =
        --                                      swap players updatedPlayer (fst w)}
        -- payWinners state ps ws (potLeft - winning) False
      else do
        --when there's all in winners, pay out the smallest all in bet like above
        --then you need to calculate the winner again with this all in better
        -- putStrLn $ "LOOKING AT ALL IN BET: " ++ show (allInBets state)
        let allInBet = head allInWins
        state <- payoutSidePot state allInBet ps ws potLeft (potLeft == currentPot state)
        let allInWinnerIndicies = [i | (i, _) <- allInWins]
            playersWithoutAllInsIdx = filter (`notElem` allInWinnerIndicies) (playersInRound state)
            playersWithoutAllIns = [p | p <- nonBustPlayers state, playerIndex p `elem` playersWithoutAllInsIdx]
        if length playersWithoutAllIns > 0 then
          payWinners state playersWithoutAllIns (determineWinner state{playersInRound = playersWithoutAllInsIdx}) (currentPot state) False
        else 
          return state
      where
        players = nonBustPlayers state


    outputPotWinners :: [Player] -> [(PlayerIndex, PokerHand)] -> Int -> Bool 
                                                                         -> IO()
    outputPotWinners ps ws winning isMainPot = do
      -- putStrLn $ "WINNERS: " ++ show ws
      -- putStrLn $ "FROM PLAYERS " ++ show ps
      let winString = if length ps > 1 then 
              unwords [name (head(filter (\p -> playerIndex p == fst w) ps)) ++ 
                        " WINS " ++ show winning ++ " CHIPS WITH A HAND OF " ++ 
                        show (snd w) ++ "\n"| w <- ws]
                    else 
                      name (head ps) ++ " WINS " ++ show winning ++ 
                        " CHIPS AS ALL OTHER PLAYERS FOLDED"
      if isMainPot then do putStrLn "MAIN POT WINNERS:"
      else do putStrLn "SIDE POT WINNERS:"
      putStrLn winString
        --     winning = snd allInBet*length ps `div` length (w:ws)
        --     pIndex = fst allInBet
        --     player = players !! pIndex
        --     updatedPlayer = player {chips = chips player + winning}
        --     hand = head (filter (\(a,_) -> a == pIndex) (w:ws))
        -- putStr $ name player ++ " WINS " ++ show winning
        -- putStrLn $ " CHIPS FROM MAIN/SIDEPOT WITH A HAND OF " ++ show (snd hand)
        -- state <- return state {
        --   nonBustPlayers = swap players updatedPlayer pIndex,
        --   allInBets = map
        --                 (\x -> if x >= snd allInBet then x-snd allInBet else x)
        --                 (allInBets state)}
        -- if non (null ws) then 
        --   payWinners state ps ws (potLeft - winning)
        -- else 
        --   payWinners state ps (determineWinner )
        -- payWinners state ps (delete (pIndex, snd hand) (w:ws)) (potLeft - winning)
    

    payoutSidePot ::  GameState -> Bet -> [Player] -> [(PlayerIndex, PokerHand)]
                                               -> Int -> Bool -> IO GameState
    payoutSidePot state allInBet ps ws potLeft isMainPot = do
      --looks at the chip contributions to this all in pot 
      -- putStrLn $ "DEALER INDEX " ++ show (currentDealerIndex state)
      -- putStrLn $ "ALL INS " ++(show (allInBets state))
      let addedBlinds = if isMainPot then smallBlindVal + bigBlindVal else 0
          allInTotalWinning = sum
                      [min (snd x) (snd allInBet) | x <- gamewiseBets state]
                      + addedBlinds

          updatedGamewiseBets =
                   map (\x -> if snd x >= snd allInBet then (fst x, snd x - snd allInBet) else (fst x, 0))
                   (gamewiseBets state)
          updatedAllIns = map (\x -> if x >= snd allInBet then x - snd allInBet
                                     else x)
                          (allInBets state)
          winning = allInTotalWinning `div` length ws
          winningPlayerIndicies = map fst ws
          winnersPayed = map (\x ->
            if playerIndex x `elem` winningPlayerIndicies then
              x {chips = chips x + winning}
            else x)
            players
      outputPotWinners ps ws winning isMainPot
      return state {currentPot = currentPot state - allInTotalWinning,
                    nonBustPlayers = winnersPayed,
                    allInBets = updatedAllIns}
      where
        dealerIdx = currentDealerIndex state
        players = nonBustPlayers state
        allIns = allInBets state
        --in case for all in on blind
        smallBlindVal = if allIns !! ((dealerIdx + 1) `mod` length allIns) < 0
          then abs (allIns !!  ((dealerIdx + 1) `mod` length allIns))
          else smallBlind state
        bigBlindVal = if allIns !! ((dealerIdx + 2) `mod` length allIns) < 0
          then abs (allIns !!  ((dealerIdx + 2) `mod` length allIns))
          else bigBlind state

    -- payWinners2 :: GameState -> [Player] -> [(PlayerIndex, PokerHand)] -> Int
    --                                                              -> IO GameState
    -- payWinners2 state ps [] potLeft = return state {currentPot = potLeft}
    -- payWinners2 state ps (w:ws) potLeft = do
    --   let allInWins = getAllInWinners ws state
    --   if null allInWins then do
    --     let player = players !! fst w
    --         winning = currentPot state `div` length (w:ws)
    --         updatedPlayer = player {chips = chips player + winning}
    --     putStr $ name player ++ " WINS " ++ show winning
    --     if length ps > 1 then do
    --       putStrLn $ " CHIPS WITH A HAND OF " ++ show (snd w)
    --     else do
    --       putStrLn " CHIPS AS ALL OTHER PLAYERS FOLDED"
    --     state <- return state {nonBustPlayers =
    --                                          swap players updatedPlayer (fst w)}
    --     payWinners2 state ps ws (potLeft - winning)
    --   else do
    --     let allInBet = head allInWins
    --         winning = snd allInBet*length ps `div` length (w:ws)
    --         pIndex = fst allInBet
    --         player = players !! pIndex
    --         updatedPlayer = player {chips = chips player + winning}
    --     let hand = head (filter (\(a,_) -> a == pIndex) (w:ws))
    --     putStr $ name player ++ " WINS " ++ show winning
    --     putStr $ " CHIPS FROM SIDEPOT WITH A HAND OF " ++ show (snd hand)
    --     state <- return state {
    --       nonBustPlayers = swap players updatedPlayer pIndex,
    --       allInBets = map
    --                     (\x -> if x >= snd allInBet then x-snd allInBet else x)
    --                     (allInBets state)}

    --     payWinners2 state ps (delete (pIndex, snd hand) (w:ws)) (potLeft - winning)
    --   where
    --     players = nonBustPlayers state

    payout :: GameState -> IO GameState
    payout state = do
       payWinners state players winners pot True
      where
        players = [nonBustPlayers state!!i | i <- playersInRound state]
        winners = determineWinner state
        pot = currentPot state
        remaining = pot `mod` length winners

    doPlayerBets :: GameState -> [Player] -> IO GameState
    doPlayerBets state [] = do
      let playersStillIn =
              correctTurnOrder [p | p <- nonBustPlayers state,
                              playerIndex p `elem` playersInRound state] state
          highestBet = getBetToCall (roundwiseBets state)
          playersWhoNeedToCall =
            filter (not . wentAllIn state) [nonBustPlayers state!!fst b |
                                            b <- roundwiseBets state,
                                            fst b `elem` playersInRound state &&
                                              snd b < snd highestBet]
      if null playersWhoNeedToCall then do
        return state {gamewiseBets = updateGameBetValues
                                     (roundwiseBets state) (gamewiseBets state)}

      else doPlayerBets state playersWhoNeedToCall

    doPlayerBets state (p:ps) = do
      if length (playersInRound state) /= 1 then do
        if not (skipBecauseOfAllIn state p) then do
          -- threadDelay (1 * 200000) -- so human player has time to read
          playerBet <- bet p state (roundwiseBets state)
          state <- return state {lastBetterIndex = playerIndex p}
          if playerBet == Nothing then do
            state <- return state {playersInRound =
                                delete (playerIndex p) (playersInRound state) }
            doPlayerBets state ps
          else do
            let theBet = fromMaybe (playerIndex p, 0) playerBet
                updatedRoundwiseBets = updateRoundBetValue (roundwiseBets state) theBet
                outdatedNonBustPlayers = nonBustPlayers state
            state <- return state {
              nonBustPlayers =
                updatePlayersChips theBet outdatedNonBustPlayers,
                roundwiseBets = updatedRoundwiseBets,
                currentPot = currentPot state + snd theBet }
            state <- if snd theBet /= 0 &&
                        chips (nonBustPlayers state !! playerIndex p) == 0 then
                          do recordAllInBet state theBet
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
                               | betToCall /= ourCurrentBet = False
                               | amount == length playersIn - 1  = True
                               | otherwise = False
      where
        allIns = allInBets state
        playersIn = playersInRound state
        amount = length (filter (/=0) allIns)
        bs = roundwiseBets state
        betToCall = snd (getBetToCall bs)
        ourCurrentBet = snd (head (filter (\(a, _) -> a == playerIndex p) bs))


    bet :: Player -> GameState -> [Bet] -> IO (Maybe Bet)
    bet p state bs | strategy p == RandomPlayer = do
                      betRandom p ourCurrentBet betToCall
                   | strategy p == PassivePlayer = do
                      betPassive p ourCurrentBet betToCall
                   | strategy p == AggressivePlayer = do
                      betAggressive p ourCurrentBet betToCall
                   | strategy p == HumanPlayer = do
                      betHuman p state ourCurrentBet betToCall
                   | strategy p == SmartPlayer = do
                      betSmart p state ourCurrentBet betToCall
                   | otherwise = return Nothing
      where
        betToCall = snd (getBetToCall bs)
        ourCurrentBet = snd (head (filter (\(a, _) -> a == playerIndex p) bs))

    correctTurnOrder :: [Player] -> GameState -> [Player]
    correctTurnOrder ps state = drop indexInThisList ps ++
                             take indexInThisList ps
      where
        lastBetIndex = lastBetterIndex state
        indexInThisList = head [i | (p, i) <- zip ps [0..],
                              playerIndex p == lastBetIndex ]

    updatePlayersChips :: Bet -> [Player] -> [Player]
    updatePlayersChips bet outdatedNonBustPlayers =
      swap outdatedNonBustPlayers updatedPlayer pIndex
      where
        pIndex = fst bet
        player = outdatedNonBustPlayers!!fst bet
        updatedPlayer = player {chips = chips player - snd bet}

    updateRoundBetValue :: [Bet] -> Bet -> [Bet]
    updateRoundBetValue bs b = map (\(a, v) -> if a == fst b then
                                            (a, v + snd b)
                                          else (a, v)) bs

    updateGameBetValues :: [Bet] -> [Bet] -> [Bet]
    updateGameBetValues roundwiseBets oldGamewiseBets =
      [(x,a+b) | ((x,a),(_,b))<- zip roundwiseBets oldGamewiseBets]

    getBetToCall :: [Bet] -> Bet
    getBetToCall = minimumBy (\ (_, a) (_, b) -> compare b a)

    randomPercentage :: IO Float
    randomPercentage = do randomRIO (0.0, 1.0)

    damper :: Float
    damper = 0.6

    recordAllInBet :: GameState -> Bet -> IO GameState
    recordAllInBet state bet = do
      putStrLn $ "ALL INS BEFORE ADDING NEW ONE " ++ show (allInBets state)
      putStrLn $ pName ++ " IS ALL IN WITH BET OF " ++ show (abs (snd bet))
      putStrLn $ "ROUNDWISE BETS SHOULD BE CLEAR " ++ show (roundwiseBets state)
      let totalAllInBetAmount = if snd bet < 0 then snd bet
                                else  snd (head 
                        (filter (\x -> fst x == fst bet) (roundwiseBets state)))
      putStrLn $ "TOTAL ALL IN BET AMOUNT " ++ show (totalAllInBetAmount)
      putStrLn $ "NOEW ALL IN LIST " ++ show (swap allInData totalAllInBetAmount pIndex)
      -- let prevBetTotal = snd (head (filter (\x -> fst x == fst bet) betData)) 
      return state {
        allInBets = swap allInData totalAllInBetAmount pIndex }
      where
        betData = roundwiseBets state
        pIndex = fst bet
        allInData = allInBets state
        pName = name (nonBustPlayers state !! pIndex)

    swap :: [a] -> a -> Int -> [a]
    swap list item index = take index list ++ [item] ++ drop (index+1) list

    randomRaise :: Player -> Int -> IO Bet
    randomRaise pl call = do
      amountRaisedPercentage <- randomPercentage
      let raise = ceiling (fromIntegral
                (chips pl - call)*amountRaisedPercentage)
      when (chips pl - call + raise > 0) $ do outputRaise pl raise call
      return (playerIndex pl, call + raise)

    outputRaise :: Player -> Int -> Int -> IO ()
    outputRaise pl raise call = do
      putStr $ name pl ++ " HAS BET " ++ show (call + raise)
      if call /= 0 then
        putStrLn $ " TO RAISE BY " ++ show raise
      else putStr "\n"

    callOrCheck :: Player -> Int -> IO Bet
    callOrCheck pl callToMake = do
      putStr $ name pl
      if callToMake == 0 then do
        putStrLn " HAS CHECKED"
      else do
        putStr $ " HAS BET " ++ show callToMake
        putStrLn " TO CALL"
      return (playerIndex pl, callToMake)

    fold :: Player -> IO (Maybe Bet)
    fold p = do
      putStrLn $ name p ++ " HAS FOLDED"
      return Nothing

    betRandom :: Player -> Int -> Int -> IO (Maybe Bet)
    betRandom pl currBet betToCall = do
      --1/3 chance between calling/raising/folding
      let randomBetChances = BetChances{
        foldThreshold = 66, raiseThreshold = 50, allInCallThreshold = 50}
      randomBetter pl currBet betToCall randomBetChances

    betPassive :: Player -> Int -> Int -> IO (Maybe Bet)
    betPassive pl currBet betToCall = do
      let randomBetChances = BetChances{
        foldThreshold = 90, raiseThreshold = 100, allInCallThreshold = 85}
      randomBetter pl currBet betToCall randomBetChances

    betAggressive :: Player -> Int -> Int -> IO (Maybe Bet)
    betAggressive pl currBet betToCall = do
      let randomBetChances = BetChances{
        foldThreshold = 99, raiseThreshold = 20, allInCallThreshold = 30}
      randomBetter pl currBet betToCall randomBetChances

    getNextHighestCard :: [Card] -> CardVal
    getNextHighestCard cards = fst (head removedHighest)
      where removedHighest = filter (\x -> fst x /= fst (head cards)) cards

    betSmart :: Player -> GameState -> Int -> Int -> IO (Maybe Bet)
    betSmart pl state currBet betToCall = do
      -- print $ length (filter (`notElem` hand pl) generateDeck)
      estimatedWin <- estimateWinChance pl state plHand
      -- putStrLn "estimatedWin"
      -- print estimatedWin
      -- print plHand
      getSmartBet state pl estimatedWin currBet betToCall
      where
        plHand = evaluateHand (hand pl)

    getSmartBet :: GameState -> Player -> Double -> Int -> Int -> IO (Maybe Bet)
    getSmartBet state pl estimatedWin currBet betToCall =
           decideSmartBet pl betRoundsLeft estimatedWin params currBet betToCall
        where
          params = getSmartBetParams estimatedWin
          betRoundsLeft = case length (communityCards state) of
            0 -> 4
            3 -> 3
            4 -> 2
            5 -> 1

    -- makeOver75BetDecision :: Player -> Int -> Double -> Int -> Int -> Maybe Bet
    -- makeOver75BetDecision pl roundsLeft estimatedWin currBet betToCall = do
    -- -- aim to bet 30-60% of your starting chips by the end of 4 betting rounds
    --   let goalBetPercentage = 0.3*(estimatedWin - 0.75) / 0.25 + 0.3
    --       percentageToBetThisRound = 
    --           (goalBetPercentage - fromIntegral(currBet/totalChipsBeforeRound))
    --             / fromIntegral roundsLeft
    --   if percentageToBetThisRound <= 0 then --call
    --     Just (playerIndex pl, betToCall - currBet)
    --   else --raise
    --     Just (playerIndex pl, 
    --     ceiling (percentageToBetThisRound * fromIntegral totalChipsBeforeRound))
    --   where
    --     totalChipsBeforeRound = currBet + chips pl

    data SmartBetParams = SmartBetParams {
      bottomEstWinRange :: Double,
      lowerBoundBetPercentage :: Double,
      betPercentageRangeSize :: Double,
      betPercentageFoldThreshold :: Double
    }

    getSmartBetParams :: Double -> SmartBetParams
    getSmartBetParams estimatedWin
      | estimatedWin >= 0.75 = SmartBetParams {
          bottomEstWinRange=0.75, lowerBoundBetPercentage=0.4,
          betPercentageRangeSize=0.3, betPercentageFoldThreshold=1}
      | estimatedWin >= 0.5 = SmartBetParams {
            bottomEstWinRange=0.5, lowerBoundBetPercentage=0.2,
            betPercentageRangeSize=0.2, betPercentageFoldThreshold=0.45}
      | estimatedWin >= 0.25 = SmartBetParams {
            bottomEstWinRange=0.25, lowerBoundBetPercentage=0.1,
            betPercentageRangeSize=0.05, betPercentageFoldThreshold=0.2}
      | otherwise = SmartBetParams {
            bottomEstWinRange=0, lowerBoundBetPercentage=0,
            betPercentageRangeSize=0.1, betPercentageFoldThreshold=0.15}


    decideSmartBet :: Player -> Int -> Double -> SmartBetParams -> Int -> Int
                                                               -> IO (Maybe Bet)
    decideSmartBet pl roundsLeft estimatedWin params currBet betToCall = do
      -- putStrLn $ "CURRENT BET IS " ++ show currBet
      -- aim to bet goalBet% of your starting chips by the end of the 4 rounds
      let goalBetPercentage = betRangeSize*(estimatedWin - bottomRange) / 0.25 +
                                                                   lowerBoundBet
          --calculate how much of that goal bet percentage to aim to bet now
          --to be on trajectory to meet that goal bet percentage                                                         
          percentageToBetThisRound = (goalBetPercentage -
                                     fromIntegral currBet/totalChipsBeforeRound)
                                                       / fromIntegral roundsLeft
          betIWant = ceiling (percentageToBetThisRound * totalChipsBeforeRound)

      -- putStrLn $ show goalBetPercentage ++ " IS THE GOAL BET PERCENTAGE"
      -- putStrLn $ show percentageToBetThisRound ++ " IS HOW MUCH WE WANNA BET THIS ROUND"
      --if we don't need to bet more this round to meet target bet %
      if percentageToBetThisRound <= 0 || currBet + betIWant < betToCall then
        if min 1 (fromIntegral betToCall/totalChipsBeforeRound) > foldThreshold then
          fold pl
        else do --call
          if plChips > betToMeetCall then do
            call <- callOrCheck pl betToMeetCall
            return $ Just call
          else
            return $ Just (playerIndex pl, plChips)
      --if we need to put a bet forward to meet target %, will have to call or
      --raise
      else do --raise
        putStrLn $ "SMART BET " ++ show betIWant ++ " FROM CHIPS: " ++ show plChips
        -- if currBet + betIWant > betToCall then do --raise
        outputRaise pl betIWant betToCall
        return $ Just (playerIndex pl, betIWant)
        -- else do --call
        --     if plChips > betToMeetCall then do
        --       call <- callOrCheck pl betToMeetCall
        --       return $ Just call
        --     else
        --       return $ Just (playerIndex pl, plChips)
      where
        betToMeetCall = betToCall - currBet
        totalChipsBeforeRound = fromIntegral (currBet + chips pl)
        bottomRange = bottomEstWinRange params
        lowerBoundBet = lowerBoundBetPercentage params
        betRangeSize = betPercentageRangeSize params
        foldThreshold = betPercentageFoldThreshold params
        plChips = chips pl

    -- total the amount of hands that beat players current hand
    estimateWinChance :: Player -> GameState -> PokerHand -> IO Double
    estimateWinChance pl state plHand = do
      --assumes independence for simplicity
      return $ ((totalHands - loseHands) / totalHands)
      -- **numOpponents
      where
        numOpponents = fromIntegral (length (playersInRound state) - 1)
        comCards = communityCards state
        otherCards = filter (`notElem` hand pl) generateDeck
        combinationsOfTwo = getTwoHandCombos otherCards
        totalHands = fromIntegral (length combinationsOfTwo)
        loseHands = fromIntegral (length (filter
           (\x -> compareHand (evaluateHand (x ++ comCards)) plHand == GT) combinationsOfTwo))

    getTwoHandCombos :: [Card] -> [[Card]]
    getTwoHandCombos [] = []
    getTwoHandCombos (c:cs) = [[c, x] | x <- cs] ++ getTwoHandCombos cs


    betHuman :: Player -> GameState -> Int -> Int -> IO (Maybe Bet)
    betHuman pl state currBet betToCall = do
      putStrLn $ "\nYOUR CHIPS: " ++ show (chips pl)
      putStrLn $ "BET TO CALL: " ++ show (snd (getBetToCall (roundwiseBets state)))
      putStrLn $ "YOUR CURRENT BET: " ++ show currBet
      putStrLn $ "YOUR HAND: " ++ show (take 2 (hand pl))
      putStrLn $ "COMMUNITY CARDS: " ++ show (communityCards state)
      getAction pl currBet betToCall

    randomBetter :: Player -> Int -> Int -> BetChances -> IO (Maybe Bet)
    randomBetter pl currBet betToCall betChances = do
      putStrLn $ "\nbet to call is " ++ show betToCall

      foldChance <- randomInt 1 100
      if foldChance > foldThreshold betChances then fold pl
      else do

        let chipsLeft = chips pl
            callToMake = betToCall - currBet

        raiseChance <- randomInt 1 100
        if chipsLeft > callToMake &&
           raiseChance > raiseThreshold betChances then do
              bet <- randomRaise pl callToMake
              return $ Just bet
        else do
          if chipsLeft <= callToMake then do
            allInChance <- randomInt 1 100
            if allInChance > allInCallThreshold betChances then
              return $ Just (playerIndex pl, chipsLeft)
            else fold pl
          else do
            bet <- callOrCheck pl callToMake
            return $ Just bet

    inpError :: String -> IO Bool
    inpError err = do
      putStrLn $ err ++ ", TRY AGAIN"
      return False

    checkRaise :: Player -> String -> Int -> IO Bool
    checkRaise pl str call
        | length str <= 5 = inpError "INVALID INPUT"
        | length [c | (c, i) <- zip (take 5 str) [0..], c == "raise"!!i] == 5
            = if foldr ((&&) . isDigit) True (drop 5 str) then
                if parsedInt <= 0 then do
                  inpError "CAN'T RAISE BY ZERO OR LESS"
                else do
                  if chips pl >= call + parsedInt then
                    return True
                  else do
                    inpError "YOU DON'T HAVE ENOUGH CHIPS TO BET THAT"
              else
                inpError "INVALID INPUT"
        | otherwise = inpError "INVALID INPUT"
      where parsedInt = read (drop 5 str)

    getAction :: Player -> Int -> Int -> IO (Maybe Bet)
    getAction pl currBet betToCall = do
      putStrLn $ "\n" ++ name pl ++ ", CHOOSE ACTION: raise x, fold, call"
      rawStr <- getLine
      let inp = map toLower (filter (not . isSpace) rawStr)
      case inp of
        "fold" -> do
          putStrLn $ "\n" ++ name pl ++ ", YOU FOLDED"
          return Nothing
        "call" -> do
          let callAmount = min (chips pl) betToCall
          if betToCall == 0 then do
            putStrLn $ "\n" ++ name pl ++ ", YOU CHECKED"
          else do
            putStrLn $ "\n" ++ name pl ++ ", YOU BET " ++
              show callAmount ++ " TO CALL"
          return (Just (playerIndex pl, callAmount))
        _ -> do
          raiseValid <- checkRaise pl inp betToCall
          if raiseValid then do
            let raiseAmount = read (drop 5 inp)
            let betAmount = betToCall + raiseAmount
            let bet = (playerIndex pl, betAmount)
            putStrLn $ "\n" ++ name pl ++ ", YOU BET " ++ show betAmount ++
              " CHIPS" ++ " TO RAISE BY " ++ show raiseAmount
            return $ Just bet
          else getAction pl currBet betToCall

-- trial 1: 
-- M:11
-- G:9
-- W:1

-- trial 2: (greater blinds ratio)
-- M: 11
-- G: 8
-- W: 3

-- trial 3: (max 100 rounds)
-- M: 13
-- G: 7
-- W: 2
