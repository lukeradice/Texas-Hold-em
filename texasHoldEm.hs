
module HoldEm where
    import System.Random

    data Suit = Hearts | Spades | Diamonds | Clubs   deriving(Show, Eq, Bounded, Enum)
    data CardVal = Two | Three | Four | Five | Six | Seven | Eight |
      Nine | Ten | Jack | Queen | King | Ace  deriving(Show, Eq, Bounded, Enum)

    generateDeck :: Deck
    generateDeck = [(cardVal, suit) | suit <- [(minBound :: Suit) ..], cardVal <- [(minBound :: CardVal) ..]]

    type Card = (CardVal, Suit)
    type Deck = [Card]
    type Hand = (Card, Card)
    data Strategy = Random | Tactical deriving(Show)

    data Player = Player { name :: String, hand :: Hand, chips :: Int, isDealer :: Bool,
      strategy :: Strategy} deriving(Show)

    type Bet = (Player, Int)
    type CurrentPlayers = [Player]

    data GameState = GameState { activePlayers :: CurrentPlayers , deck :: Deck, communityCards :: [Card],
      currentPot :: Int, bets :: [Bet], currentDealerIndex :: Int, smallBlind :: Int,
      bigBlind :: Int} deriving(Show)


    data Deal = Community | Hole  deriving(Eq)
    dealCards :: Deal -> GameState -> GameState
    dealCards deal state | deal == Community = state {
                                                deck = drop 5 (deck state) ++ take 5 (deck state),
                                                communityCards = take 5 (deck state)}
                         | otherwise = state {
                                        activePlayers = dealToPlayers (activePlayers state) (deck state),
                                        deck = drop (2*length (activePlayers state)) (deck state)}

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