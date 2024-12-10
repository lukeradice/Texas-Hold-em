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

    -- | the type of deal used at start of a round
    data Deal = Community | Hole  deriving(Eq)

    data BetChances = BetChances {
      raiseThreshold :: Int, foldThreshold :: Int, allInCallThreshold :: Int}

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

    -- | calculated parameters based on game situationused in smart betting 
    -- | action function to drive behaviour
    data SmartBetParams = SmartBetParams {
      bottomEstWinRange :: Double,
      lowerBoundBetPercentage :: Double,
      betPercentageRangeSize :: Double,
      betPercentageFoldThreshold :: Double
    }

    -- | generates the complete unshuffled 52 card deck
    generateDeck :: Deck
    generateDeck = [(val, suit) | suit <- [(minBound :: Suit) ..],
                      val <- [succ (minBound :: CardVal) ..]]

    -- | depending on if its Hole/Community deal deals appropriate amount of 
    -- | cards depending on amount of community cards already in
    dealCards :: Deal -> GameState -> IO GameState
    dealCards deal state = do
      if deal == Hole then do
        return state { --two cards for each
          nonBustPlayers = dealToPlayersHole thePlayers theDeck,
          deck = drop (2*length thePlayers) theDeck}
      else
       if null theComCards then
        return state { --the flop
           deck = drop 3 theDeck,
           communityCards = take 3 theDeck,
           nonBustPlayers = dealToPlayersCommunity thePlayers (take 3 theDeck) }
       else
        return state { --3rd and 4th betting round community deals
             deck = tail theDeck,
             communityCards = theComCards ++ [head theDeck],
             nonBustPlayers = dealToPlayersCommunity thePlayers [head theDeck] }
      where
        theDeck = deck state
        thePlayers = nonBustPlayers state
        theComCards = communityCards state

    -- | gives two cards from top of deck to each player
    dealToPlayersHole :: [Player] -> Deck -> [Player]
    dealToPlayersHole [] deck = []
    dealToPlayersHole (p:ps) (c1:c2:cs) = p { hand = hand p ++ [c1, c2] }
                                          : dealToPlayersHole ps cs

    -- | adds community cards dealt to each players hand
    dealToPlayersCommunity :: [Player] -> [Card] -> [Player]
    dealToPlayersCommunity[] deck = []
    dealToPlayersCommunity (p:ps) cs= p { hand = hand p ++ cs}
                                      : dealToPlayersCommunity ps cs

    -- | for getting random number in range x to y
    randomInt :: Int -> Int -> IO Int
    randomInt x y = randomRIO (x, y)

    -- | for getting random list of deck indices for shuffling
    shuffleNums :: [Int] -> IO [Int]
    shuffleNums [] = return []
    shuffleNums nums = do
      ind <- randomInt 0 (length nums - 1)
      let outputList = [nums!!ind]
      rest <- shuffleNums (take ind nums ++ drop (ind+1) nums)
      return (outputList ++ rest)

    -- | shuffles the order of the deck in gamestate
    shuffleDeck :: GameState -> IO GameState
    shuffleDeck state = do
      let theDeck = deck state
      randomIndicies <- shuffleNums [0..(length theDeck -1)]
      let newDeck = [theDeck !! i | i <- randomIndicies]
      return state { deck = newDeck }

    -- | determines user hand, gets PokerHand type with the cards making up the 
    -- | hand passed in
    evaluateHand :: [Card] -> PokerHand
    evaluateHand xs
      | length xs == 2 = evaluateHoleHand xs kindsSorted --straight/pair on hole
      | length longestStraight >= 5 && --straight and royal flush --straight and royal flush --straight and royal flush --straight and royal flush
         --straight and royal flush
         --straight and royal flush
         --straight and royal flush --straight and royal flush
         --straight and royal flush
        longestStraight == highestFlush =
          returnStraightFlush xs longestStraight
      | length highestKind == 4 = --four of a kind
          returnKindHand highestKind kindsGroupedWithoutHighest
      | length highestKind == 3 && --full house --full house --full house --full house
         --full house
         --full house
         --full house --full house
         --full house
        length sndHighestKind >= 2 =
          returnFullHouse highestKind sndHighestKind
      | length highestFlush >= 5 = returnFlush highestFlush --flush
      | length longestStraight >= 5 = returnStraight longestStraight --straight
      | length highestKind == 3 = --three of a kind
           returnKindHand highestKind kindsGroupedWithoutHighest
      | length highestKind == 2 && length sndHighestKind == 2 = --two pair
          returnTwoPair
            highestKind sndHighestKind kindsGroupedWithoutHighest
      | length highestKind == 2 = --one pair
          returnKindHand highestKind kindsGroupedWithoutHighest
      | otherwise = HighCard (take 5 kindsSorted) --high card

      where --useful groupings for determining and returning hand
        kindsSorted = sortByKind xs
        kindsGrouped = groupBy (\a b -> fst a == fst b) kindsSorted
        suitsGrouped = groupBy (\a b -> snd a == snd b)
                             (sortBy (\(_, a) (_, b)-> compare a b) kindsSorted)
        longestStraight = getLongestStraight (sortByKind (addLowAces xs))
        highestFlush = getMaxSizeList suitsGrouped
        highestKind = getMaxSizeList kindsGrouped
        kindsGroupedWithoutHighest = delete highestKind kindsGrouped
        sndHighestKind = getMaxSizeList kindsGroupedWithoutHighest

    -- | returns the five card full house hand that was determined
    returnFullHouse :: [Card] -> [Card] -> PokerHand
    returnFullHouse highestKind sndHighestKind =
      FullHouse (take 5
            (highestKind ++ drop (length sndHighestKind - 2) sndHighestKind))

    -- | returns the five card flush hand that was determined
    returnFlush :: [Card] -> PokerHand
    returnFlush highestFlush = Flush (take 5
                                  (drop (length highestFlush - 5) highestFlush))

    -- | returns the five card straight hand that was determined
    returnStraight :: [Card] -> PokerHand
    returnStraight longestStraight = Straight (take 5 (
                             drop (length longestStraight - 5) longestStraight))

    -- | sorts a list of cards by their kind in descending order
    sortByKind :: [Card] -> [Card]
    sortByKind = sortBy (\(a, _) (b, _)-> compare b a)

    -- | determines the whether hole hand is high card or pair useful for smart
    -- | player decision evaluation
    evaluateHoleHand :: [Card] -> [Card] -> PokerHand
    evaluateHoleHand xs kindsSorted
      | fst (head xs) == fst (xs!!1) = Pair xs
      | otherwise = HighCard kindsSorted

    -- | returns five card straight flush hand that was determined
    returnStraightFlush :: [Card] -> [Card] -> PokerHand
    returnStraightFlush xs straight | fst (last straight) == Ace =
                                          RoyalFlush straight
                                       | otherwise =
                                          StraightFlush straight

    -- | returns five card straight two pair hand that was determined
    returnTwoPair :: [Card] -> [Card] -> [[Card]] -> PokerHand
    returnTwoPair highestKind sndHighestKind kindsGroupedWithoutHighest =
      TwoPair (take 5 (highestKind ++ sndHighestKind ++
             getMaxSizeList (delete sndHighestKind kindsGroupedWithoutHighest)))

    -- | returns five card two/three/four of a kind hand that was determined
    returnKindHand :: [Card] -> [[Card]] -> PokerHand
    returnKindHand highestKind kindsGroupedWithoutHighest
      | highKindLength == 4 = FourOfAKind hand
      | highKindLength == 3 = ThreeOfAKind hand
      | highKindLength == 2 = Pair hand
        where
          highKindLength = length highestKind
          hand = take 5 (highestKind ++ concat kindsGroupedWithoutHighest)

    -- | returns the highest straight with the greatest amount of cards
    getLongestStraight :: [Card] -> [Card]
    getLongestStraight xs = getMaxSizeList groupedByStraight
      where groupedByStraight = groupByStraight (map (\x -> [x]) xs)

    -- | groups a sorted one list of one card list into lists of straights
    groupByStraight :: [[Card]] -> [[Card]]
    groupByStraight [x] = [x]
    groupByStraight (x:y:xs) | (yVal /= maxBound) && (succ yVal == xVal) =
                                groupByStraight ((x ++ y) : xs)
                             | otherwise =
                                x : groupByStraight (y:xs)
      where
        xVal = fst (last x)
        yVal = fst (last y)

    -- | returns the first list of greatest length in list
    getMaxSizeList :: Ord a => [[(a, b)]] -> [(a, b)]
    getMaxSizeList [x] = x
    getMaxSizeList (x:y:xs) | length x > length y = getMaxSizeList (x:xs)
                            | length y > length x = getMaxSizeList (y:xs)
                            | otherwise =
                                if fst (last x) > fst (last y) then
                                  getMaxSizeList (x:xs)
                                else getMaxSizeList (y:xs)

    -- | determines which of two poker hands is greater 
    compareHand :: PokerHand -> PokerHand -> Ordering
    compareHand hand1 hand2 | hand1{cards=[]} == hand2{cards=[]} =
                              --if the hand ranks are same compare cards
                                compare
                                  (map fst (cards hand1))
                                  (map fst (cards hand2))
                            | otherwise = --compare by standard hand rank
                              compare hand1 hand2

    -- | gets list of players with the best hands at the end of a round 
    determineWinner :: GameState -> [(PlayerIndex, PokerHand)]
    determineWinner state = winner : filter --get hands equal to winner
                              (\(_,a) -> compareHand a (snd winner) == EQ)
                              (tail winnersRanked)
      where
        players = filter (\x -> playerIndex x `elem` playersInRound state)
                         (nonBustPlayers state) --get players still in
        --get players with their hand in list
        hands = [(playerIndex x, evaluateHand (hand x)) | x <- players]
        --rank list on basis of hand
        winnersRanked = sortBy (\(_, a) (_, b)-> compareHand b a)  hands
        winner = head winnersRanked

    -- | adds LowAce for each ace in cards so ace low straights can form
    addLowAces :: [Card] -> [Card]
    addLowAces [] = []
    addLowAces (x:xs) | fst x == Ace = x :(LowAce, snd x) : addLowAces xs
                      | otherwise = x : addLowAces xs

    -- | plays hands/games until all but 1 player or bust or 100 rounds played
    gameLoop :: GameState -> Int -> IO ()
    gameLoop state count = do
      state <- playRound state
      let players = nonBustPlayers state
      if length players == 1 then do --one player remaining
        putStrLn $ concat (replicate 100 "*")
        putStr $ "\n" ++ name (head players) ++ " HAS WON ALL THE CHIPS !!!"
        putStrLn $ " IT TOOK HIM " ++ show (count+1) ++ " ROUND(S)"
      else
        if count == 100 then do --final count reached, rank remaining players
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
        else do --if ending conditions not met, play another hand/game
          let newDealer = (currentDealerIndex state + 1) `mod` length players
          gameLoop state {currentDealerIndex = newDealer} (count+1)

    -- | plays out the sequence of betting rounds for one hand and payout
    playRound :: GameState -> IO GameState
    playRound state = do
      putStrLn "\nSTARTING NEW ROUND\n"
      --reshuffle deck, clear hands and community cards, set bets to 0 and 
      --add back in all the non bust players
      state <- return state { deck = generateDeck,
                              nonBustPlayers =
                       clearPlayerHands (nonBustPlayers state),
                       playersInRound = [0..(length players -1)],
                       communityCards = [],
                       allInBets = replicate (length players) 0,
                       gamewiseBets = [(x, 0) | x <- [0..(length players -1)]],
                       roundwiseBets = [(x, 0) | x <- [0..(length players -1)]]}
      --sequence of betting rounds
      state <- shuffleDeck state
      state <- initiateBets state Hole
      state <- initiateBets state Community
      state <- initiateBets state Community
      state <- initiateBets state Community
      putStrLn ""
      state <- payout state
      removeBustPlayers state [0..length players-1]
      where
        players = nonBustPlayers state

    -- | empties players hand card arrays 
    clearPlayerHands :: [Player] -> [Player]
    clearPlayerHands ps = [x { hand = [] } | x <- ps]

    -- | removes bust players from players list, adjust remaining players' 
    -- | indices
    removeBustPlayers :: GameState -> [Int] -> IO GameState
    removeBustPlayers state [] = return state
    removeBustPlayers state (i:is) = do
      let nonBustPl = nonBustPlayers state
      if i /= length nonBustPl && chips (nonBustPl !! i) == 0 then do
        let player = nonBustPl !! i
        putStrLn $ name player ++ " HAS GONE BUST! BETTER LUCK NEXT TIME, BUDDY"
        let players = delete player nonBustPl
        let pIndex = playerIndex player
        --update non bust players list player indexes
        state <- return state { nonBustPlayers = map
          (\x -> if playerIndex x > pIndex then
                    x {playerIndex = playerIndex x - 1}
                 else x) players }
        removeBustPlayers state (i : take (length is -1) is)
      else
        removeBustPlayers state is

    -- | initiates betting rounds unless you need to skip them due to all ins or 
    -- | when there's only 1 player left
    initiateBets :: GameState -> Deal -> IO GameState
    initiateBets state deal = do
      let allIns = filter (/=0) (allInBets state)
      let playersIn = length (playersInRound state)
      if playersIn > 1 then do
        state <- dealCards deal state
        --checking if we should skip to showdown because of all in bets
        if length allIns < playersIn - 1 then do
          state <- bettingRound state deal
          putStrLn $ "\nCURRENT POT IS " ++ show (currentPot state)
          return state
        else do --despite skipping, inform community card dealing to showdown
          putStrLn $ "\nCOMMUNITY CARDS ARE NOW : " ++
                      show (communityCards state)
          return state
      else
        return state

    -- | gets players in right order for betting round, announces round and com
    -- | cards before player bets and clears roundwise betting data after
    bettingRound :: GameState -> Deal -> IO GameState
    bettingRound state deal = do
      let comCards = communityCards state
          dealerIndex = currentDealerIndex state
          playersInBetOrder = drop (dealerIndex+1) players ++
                              take (dealerIndex+1) players
          unfoldedPlayers = [p | p <- playersInBetOrder,
                              playerIndex p `elem` playersInRound state]
      state <- if null comCards then do
        putStrLn "\nPRE-FLOP BETTING ROUND"
        state <- payBlinds state
        doPlayerBets state (drop 2 unfoldedPlayers) --skipped as they did blinds
      else do
        putStrLn $ "\nCOMMUNITY CARDS ARE NOW : " ++ show comCards
        doPlayerBets state unfoldedPlayers
      return state {roundwiseBets = [(x, 0) | x <- [0..(length players -1)]]}
      where
        players = nonBustPlayers state

    -- | initiates blind payments for small and big blind
    payBlinds :: GameState -> IO GameState
    payBlinds state = do
      state <- payBlind state sBlind
      payBlind state bBlind
      where
        bBlind = bigBlind state
        sBlind = smallBlind state

    -- | does payment for a blind and outputs to console it does so
    payBlind :: GameState -> Int -> IO GameState
    payBlind state blind = do
      if chips updatedPlayer > 0 then do
        putStr $ name player ++ " HAS BET " ++ show blind
        putStrLn $ " ON THE" ++ blindStr ++ " BLIND"
        --updates chip, bet and pot values
        let newState = state { nonBustPlayers =
            swap players updatedPlayer blindIndex,
            currentPot = currentPot state + blind,
            roundwiseBets = map
              (\(x,y) -> if x == blindIndex then (x,blind) else (x,y))
              (roundwiseBets state) }
        return newState
      else do --for all in blind      
        let betPaid = chips updatedPlayer + blind
        --update roundwise bet value to show all in and chips and pot changed
        state <- return state {roundwiseBets = map
              (\(x,y) -> if x == blindIndex then (x,betPaid) else (x,y))
              (roundwiseBets state) }
        newState <- recordAllInBet state (blindIndex, betPaid)
        putStrLn $ "ON THE" ++ blindStr ++ " BLIND"
        return newState { nonBustPlayers =
          swap players (updatedPlayer {chips = 0}) blindIndex,
          currentPot = currentPot state + betPaid }
      where
        players = nonBustPlayers state
        dealerIndex = currentDealerIndex state
        --determine the blind index and string based on the input amount given
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

    -- | distributes appropriate chips to winnners for the main pot and any
    -- | sidepots
    payWinners :: GameState -> [Player] -> [(PlayerIndex, PokerHand)] -> Int
                                                         -> Bool -> IO GameState
    payWinners state [] _ _ _  = return state
    payWinners state playersIn winners potLeft isMainPot = do
      let allInWins = getMinAllInWinners winners state
      --if no all in winners can distribute current pot among all players left
      if null allInWins then do
        let winning = potLeft `div` length winners
            winningPlayerIndicies = map fst winners
            winnersPayed = map (\x ->
                if playerIndex x `elem` winningPlayerIndicies then
                  x {chips = chips x + winning}
                else x)
                players
        outputPotWinners playersIn winners winning isMainPot

        return state {nonBustPlayers = winnersPayed,
                      currentPot = potLeft  - (winning*length winners)}

      else do
        -- | when there's all in winners, pay out the smallest all in bet like 
        -- | above but with reduced pot according to all in bets    
        let allInBet = head allInWins
            allIns = allInBets state
            allInWinners =  sortBy (\(_, a) (_, b) -> compare b a)
                              [(p, allIns!!p)| (p, _) <- winners, allIns!!p > 0]

        state <- payoutSidePot state allInBet playersIn winners potLeft isMainPot

        let otherPlayersToConsider =  filter
              (\x -> fst x /= fst allInBet && snd x > 0 && fst x `elem`
              map playerIndex playersIn) (gamewiseBets state)
            otherPlayersIdx = map fst otherPlayersToConsider
            totalPlayersConsidered = [players!!i | i <- otherPlayersIdx]

        --after calculating main pot (or multiple all in pots), payout the rest
        payWinners
          state totalPlayersConsidered (determineWinner state{playersInRound = otherPlayersIdx}) (currentPot state) False

      where
        players = nonBustPlayers state

    -- | console output of main and side pot winners
    outputPotWinners :: [Player] -> [(PlayerIndex, PokerHand)] -> Int -> Bool
                                                                         -> IO()
    outputPotWinners playersIn winners winning isMainPot = do
      let winString = if length playersIn > 1 then
              unwords [name (head
              --this gets player name from his index stored in first tuple of w
                           (filter (\p -> playerIndex p == fst w) playersIn)) ++
                        " WINS " ++ show winning ++ " CHIPS WITH A HAND OF " ++
                        show (snd w) ++ "\n"| w <- winners]
                    else
                      name (head playersIn) ++ " WINS " ++ show winning ++
                        " CHIPS AS ALL OTHER PLAYERS FOLDED"
      if isMainPot then do putStrLn "MAIN POT WINNERS:"
      else do putStrLn "SIDE POT WINNERS:"
      putStrLn winString

    -- | for a sidepot/mainpot calculation because of an all in, evaluates 
    -- | total amount in the pot and pays out
    payoutSidePot ::  GameState -> Bet -> [Player] -> [(PlayerIndex, PokerHand)]
                                               -> Int -> Bool -> IO GameState
    payoutSidePot state allInBet ps ws potLeft isMainPot = do
      --looks at the chip contributions to this all in pot 
      let allInTotalWinning = sum
                      [min (snd x) (snd allInBet) | x <- gamewiseBets state]
          -- | update gamewise bet and all in information to inform the
          -- | calculations of the other pots
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
                    allInBets = updatedAllIns,
                    gamewiseBets = updatedGamewiseBets}
      where
        dealerIdx = currentDealerIndex state
        players = nonBustPlayers state
        allIns = allInBets state

    -- | intiitates payout for winners of a hand
    payout :: GameState -> IO GameState
    payout state = do
       payWinners state players winners pot True --true is to say its main pot
      where
        players = [nonBustPlayers state!!i | i <- playersInRound state]
        winners = determineWinner state
        pot = currentPot state

    -- | the recurring betting of a single betting round for all the players in
    -- | until all players have called
    doPlayerBets :: GameState -> [Player] -> IO GameState
    doPlayerBets state [] = do
      --determine who needs to call and call function with those players again
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
      if length (playersInRound state) /= 1 then do --checks for round winner
        if not (skipBecauseOfAllIn state p) then do --checks if player all in
          when (checkForHumanPlayers (nonBustPlayers state)) $ do
             threadDelay (1 * 2000000) -- so human player has time to read
          playerBet <- bet p state (roundwiseBets state) -- do bet
          state <- return state {lastBetterIndex = playerIndex p}
          if playerBet == Nothing then do -- a fold
            state <- return state {playersInRound =
                                delete (playerIndex p) (playersInRound state) }
            doPlayerBets state ps
          else do
            let theBet = fromMaybe (playerIndex p, 0) playerBet
                updatedRoundwiseBets =
                                updateRoundBetValue (roundwiseBets state) theBet
                outdatedNonBustPlayers = nonBustPlayers state
            --update chip and bets values for players in game state
            state <- return state {
              nonBustPlayers =
                updatePlayersChips theBet outdatedNonBustPlayers,
                roundwiseBets = updatedRoundwiseBets,
                currentPot = currentPot state + snd theBet }
            --records all in bet if it occurred
            state <- if snd theBet /= 0 &&
                        chips (nonBustPlayers state !! playerIndex p) == 0 then
                          do recordAllInBet state theBet
                     else return state
            doPlayerBets state ps
        else
          doPlayerBets state ps
      else
        return state

    -- | checks if player has gone all in in the current round
    wentAllIn :: GameState -> Player -> Bool
    wentAllIn state p = (allInBets state !! playerIndex p) > 0

    -- | check if a human player is in the game
    checkForHumanPlayers :: [Player] -> Bool
    checkForHumanPlayers = foldr (\ p -> (||) (strategy p == HumanPlayer)) False

    -- | checks if you're all in or the rest of the players you're betting with
    -- | are all in
    skipBecauseOfAllIn :: GameState -> Player -> Bool
    skipBecauseOfAllIn state p | wentAllIn state p = True
                               | betToCall /= ourCurrentBet = False
                               | amountOfAllIns == length playersIn - 1  = True
                               | otherwise = False
      where
        allIns = allInBets state
        playersIn = playersInRound state
        amountOfAllIns = length (filter (/=0) allIns)
        bs = roundwiseBets state
        betToCall = snd (getBetToCall bs)
        ourCurrentBet = snd (head (filter (\(a, _) -> a == playerIndex p) bs))

    -- | handles the commencement of betting based on each playing style
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

    -- | used when players who've bet previously in a round need to call, this
    -- | gives you the order of players to bet after the last bet 
    correctTurnOrder :: [Player] -> GameState -> [Player]
    correctTurnOrder ps state = drop indexInThisList ps ++
                             take indexInThisList ps
      where
        lastBetIndex = lastBetterIndex state
        indexInThisList = head [i | (p, i) <- zip ps [0..],
                              playerIndex p == lastBetIndex ]

    -- | alters list of players chip values according to what they bet
    updatePlayersChips :: Bet -> [Player] -> [Player]
    updatePlayersChips bet outdatedNonBustPlayers =
      swap outdatedNonBustPlayers updatedPlayer pIndex
      where
        pIndex = fst bet
        player = outdatedNonBustPlayers!!fst bet
        updatedPlayer = player {chips = chips player - snd bet}

    -- | increases the bet value for a player's in the bets list which will be
    -- | stored in game state 
    updateRoundBetValue :: [Bet] -> Bet -> [Bet]
    updateRoundBetValue bs b = map (\(a, v) -> if a == fst b then
                                            (a, v + snd b)
                                          else (a, v)) bs

    -- | updates the gamewise bet values from a list of roundwise bets,
    -- | necessary to track for calculating side pots in payout
    updateGameBetValues :: [Bet] -> [Bet] -> [Bet]
    updateGameBetValues roundwiseBets oldGamewiseBets =
      [(x,a+b) | ((x,a),(_,b))<- zip roundwiseBets oldGamewiseBets]

    -- | gets the highest bet from a list of bets, get the total bet to call
    getBetToCall :: [Bet] -> Bet
    getBetToCall = minimumBy (\ (_, a) (_, b) -> compare b a)

    -- | returns random float value between 0.0 and 1.0
    randomPercentage :: IO Float
    randomPercentage = do randomRIO (0.0, 1.0)

    -- | used during testing to make random player rounds last longer and not 
    -- | raise as high
    damper :: Float
    damper = 0.6

    -- | take a bet, outputs it as an all in bet and stores the total bet value
    -- | from that player in game state in the all in bets list in gamestate
    recordAllInBet :: GameState -> Bet -> IO GameState
    recordAllInBet state bet = do
      putStrLn $ pName ++ " IS ALL IN WITH BET OF " ++ show (snd bet)
      --get total bet from user in this hand up to and including this all in
      let totalAllInBetAmount = snd (head
                        (filter (\x -> fst x == fst bet) (roundwiseBets state)))
      return state {
        allInBets = swap allInData totalAllInBetAmount pIndex }
      where
        betData = roundwiseBets state
        pIndex = fst bet
        allInData = allInBets state
        pName = name (nonBustPlayers state !! pIndex)

    -- | from a list swaps an item out for another item at a given index
    swap :: [a] -> a -> Int -> [a]
    swap list item index = take index list ++ [item] ++ drop (index+1) list

    -- | get a random amount of reamining chips to raise call and return as bet
    randomRaise :: Player -> Int -> IO Bet
    randomRaise pl call = do
      amountRaisedPercentage <- randomPercentage
      let raise = ceiling (fromIntegral
                (chips pl - call)*amountRaisedPercentage)
      when (chips pl - call + raise > 0) $ do outputRaise pl raise call
      return (playerIndex pl, call + raise)

    -- | output to the console about a raise 
    outputRaise :: Player -> Int -> Int -> IO ()
    outputRaise pl raise call = do
      putStr $ name pl ++ " HAS BET " ++ show (call + raise)
      if call /= 0 then
        putStrLn $ " TO RAISE BY " ++ show raise
      else putStr "\n"

    -- | returns call/check of the current bet and outputs so to the console
    callOrCheck :: Player -> Int -> IO Bet
    callOrCheck pl callToMake = do
      putStr $ name pl
      if callToMake == 0 then do
        putStrLn " HAS CHECKED"
      else do
        putStr $ " HAS BET " ++ show callToMake
        putStrLn " TO CALL"
      return (playerIndex pl, callToMake)

    -- | returns Nothing bet, a fold and outputs so tot the console
    fold :: Player -> IO (Maybe Bet)
    fold p = do
      putStrLn $ name p ++ " HAS FOLDED"
      return Nothing

    -- | the betting action of a random player in a betting round
    betRandom :: Player -> Int -> Int -> IO (Maybe Bet)
    betRandom pl currBet betToCall = do
      --1/3 chance between calling/raising/folding
      let randomBetChances = BetChances{
        foldThreshold = 66, raiseThreshold = 50, allInCallThreshold = 50}
      randomBetter pl currBet betToCall randomBetChances

    -- | the betting action of a passive player in a betting round
    betPassive :: Player -> Int -> Int -> IO (Maybe Bet)
    betPassive pl currBet betToCall = do
      let randomBetChances = BetChances{
        foldThreshold = 90, raiseThreshold = 100, allInCallThreshold = 85}
      randomBetter pl currBet betToCall randomBetChances

    -- | the betting action of an aggressive player in a betting round
    betAggressive :: Player -> Int -> Int -> IO (Maybe Bet)
    betAggressive pl currBet betToCall = do
      let randomBetChances = BetChances{
        foldThreshold = 99, raiseThreshold = 20, allInCallThreshold = 30}
      randomBetter pl currBet betToCall randomBetChances

    -- | determines the betting action for players which use threshold values
    -- | in bet chances to weight betting decisions
    randomBetter :: Player -> Int -> Int -> BetChances -> IO (Maybe Bet)
    randomBetter pl currBet betToCall betChances = do
      putStrLn $ "\nbet to call is " ++ show betToCall

      foldChance <- randomInt 1 100
      if foldChance > foldThreshold betChances then fold pl --fold
      else do

        let chipsLeft = chips pl
            callToMake = betToCall - currBet

        raiseChance <- randomInt 1 100
        if chipsLeft > callToMake &&
           raiseChance > raiseThreshold betChances then do --raise
              bet <- randomRaise pl callToMake
              return $ Just bet
        else do
          if chipsLeft <= callToMake then do
            allInChance <- randomInt 1 100
            if allInChance > allInCallThreshold betChances then --all in call
              return $ Just (playerIndex pl, chipsLeft)
            else fold pl --folding on all in call 
          else do
            bet <- callOrCheck pl callToMake --call
            return $ Just bet

    -- | the betting action of a smart player in a betting round, get the smart
    -- | bet and return it
    betSmart :: Player -> GameState -> Int -> Int -> IO (Maybe Bet)
    betSmart pl state currBet betToCall = do
      estimatedWin <- estimateWinChance pl state plHand
      getSmartBet state pl estimatedWin currBet betToCall
      where
        plHand = evaluateHand (hand pl)

    -- | determine how many rounds left in hand and initates smart bet action
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

    -- | return the parameters of smart bet behaviour based on the estimated
    -- | win chance
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

    -- | based on estimated win chance, smart bet parameters, remaining rounds, 
    -- | our current gamewise bet and the bet to call in the round, make a smart
    -- | betting decision
    decideSmartBet :: Player -> Int -> Double -> SmartBetParams -> Int -> Int
                                                               -> IO (Maybe Bet)
    decideSmartBet pl roundsLeft estimatedWin params currBet betToCall = do
      -- aim to bet goalBet% of your starting chips by the end of the 4 rounds
      let goalBetPercentage = betRangeSize*(estimatedWin - bottomRange) / 0.25 +
                                                                   lowerBoundBet
          --calculate how much of that goal bet percentage to aim to bet now
          --to be on trajectory to meet that goal bet percentage                                                         
          percentageToBetThisRound = (goalBetPercentage -
                                     fromIntegral currBet/totalChipsBeforeRound)
                                                       / fromIntegral roundsLeft
          betIWant = ceiling (percentageToBetThisRound * totalChipsBeforeRound)

      --if we don't need to bet more this round to meet target bet %
      if percentageToBetThisRound <= 0 || currBet + betIWant <= betToCall then
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
        outputRaise pl (betIWant-betToMeetCall) betToMeetCall
        return $ Just (playerIndex pl, betIWant)

      where
        betToMeetCall = betToCall - currBet
        totalChipsBeforeRound = fromIntegral (currBet + chips pl)
        bottomRange = bottomEstWinRange params
        lowerBoundBet = lowerBoundBetPercentage params
        betRangeSize = betPercentageRangeSize params
        foldThreshold = betPercentageFoldThreshold params
        plChips = chips pl

    -- | total the amount of possible hands that beat players current hand
    estimateWinChance :: Player -> GameState -> PokerHand -> IO Double
    estimateWinChance pl state plHand = do
      -- return hand percentile
      return $ ((totalHands - loseHands) / totalHands)
      where
        comCards = communityCards state
        -- get combinations of two card hands opponent could have
        otherCards = filter (`notElem` hand pl) generateDeck
        combinationsOfTwo = getTwoHandCombos otherCards
        totalHands = fromIntegral (length combinationsOfTwo)
        --count how many of those card hands can form a superior poker hand
        loseHands = fromIntegral (length (filter
           (\x -> compareHand (evaluateHand (x ++ comCards)) plHand == GT)
           combinationsOfTwo))

    -- | get all the possible two card hands an opponent can have from the given
    -- | cards
    getTwoHandCombos :: [Card] -> [[Card]]
    getTwoHandCombos [] = []
    getTwoHandCombos (c:cs) = [[c, x] | x <- cs] ++ getTwoHandCombos cs

    -- | the betting action of a human player, output relevant round information
    -- | and initiate console inputs
    betHuman :: Player -> GameState -> Int -> Int -> IO (Maybe Bet)
    betHuman pl state currBet betToCall = do
      putStrLn $ "\nYOUR CHIPS: " ++ show (chips pl)
      putStrLn $ "BET TO CALL: " ++ show (snd (getBetToCall (roundwiseBets state)))
      putStrLn $ "YOUR CURRENT BET: " ++ show currBet
      putStrLn $ "YOUR HAND: " ++ show (take 2 (hand pl))
      putStrLn $ "COMMUNITY CARDS: " ++ show (communityCards state)
      getHumanAction pl currBet betToCall

    -- | takes the console input for a human player's betting decision
    getHumanAction :: Player -> Int -> Int -> IO (Maybe Bet)
    getHumanAction pl currBet betToCall = do
      putStrLn $ "\n" ++ name pl ++ ", CHOOSE ACTION: raise x, fold, call"
      rawStr <- getLine
      let inp = map toLower (filter (not . isSpace) rawStr)
      case inp of
        "fold" -> do
          putStrLn $ "\n" ++ name pl ++ ", YOU FOLDED"
          return Nothing
        "call" -> do
          let callAmount = min (chips pl) betToCall --consider all in call
          if betToCall == 0 then do
            putStrLn $ "\n" ++ name pl ++ ", YOU CHECKED"
          else do
            putStrLn $ "\n" ++ name pl ++ ", YOU BET " ++
              show callAmount ++ " TO CALL"
          return (Just (playerIndex pl, callAmount))
        _ -> do --for raise and catches other random inputs too
          raiseValid <- checkRaise pl inp betToCall
          if raiseValid then do
            let raiseAmount = read (drop 5 inp)
            let betAmount = betToCall + raiseAmount
            let bet = (playerIndex pl, betAmount)
            putStrLn $ "\n" ++ name pl ++ ", YOU BET " ++ show betAmount ++
              " CHIPS" ++ " TO RAISE BY " ++ show raiseAmount
            return $ Just bet
          else --irregular inputs or raises will have to input again 
            getHumanAction pl currBet betToCall

    -- | outputs a given error to the console and returns false
    inpError :: String -> IO Bool
    inpError err = do
      putStrLn $ err ++ ", TRY AGAIN"
      return False

    -- | validation for the chip figure given to raise
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

    -- | entrypoint for the game, define your players in here and run
    startGame :: IO () -- start of program
    startGame = do
      --test players and state
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
