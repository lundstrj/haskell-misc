-- Lab 2 (BlackJack)
-- 12/7 2011
-- Johan Lundstroem

-- completed the 18/7 2011. Properties working and everything. /Johan L

-- Task #1
{-| 

size :: Num a => Hand -> a
size Empty             = 0
size (Some card hand)  = 1 + size hand

hand2 = (Some (Card (Numeric 2) Hearts)
        (Some (Card Jack Spades) Empty))
        
-- Hears 2, Spades Jack. Size should be 2.

size hand2
= size (Some (Card (Numeric 2) Hearts)
             (Some (Card Jack Spades) Empty))

Answer:
  Step #1: 1 + size (Some (Card Jack Spades) Empty)
  Step #2: 1 + 1 + size Empty
  Step #3: 1 + 1 + 0 = 2
-}

-- Task #2
module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
import System.Random
import List

-- Gives you an empty hand
empty :: Hand
empty = Empty
prop_empty :: Bool
prop_empty = size empty == 0

-- Gives you a non empty hand (for testing purposes)

notEmpty :: Hand
notEmpty = Some (Card (Numeric 2) Spades) Empty
prop_notEmpty :: Bool
prop_notEmpty = not (size notEmpty == 0)

-- For crying out loud, figuring out the numeric case took
-- me ages. Note to self, try pattern mathching first.

-- Gives you the value of a rank
-- Todo: Johan: Fix the issues with the aces
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank rank | rank == Jack = 10
               | rank == Queen = 10
               | rank == King = 10
               | rank == Ace = 11
--prop_valueRank :: Rank -> Bool
--prop_valueRank (Numeric n) = valueRank (Numeric n) == n
--prop_valeuRank r1 | r1 == Ace then valueRank r1 == 10

-- Took me a little while to figure out how to get values
-- stuck inside a datastructure. But a lot quicker than 
-- valueRank above

-- Gives you the value of a card
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- This one was a pretty straight shot actually. Well done
-- by me.
-- Gives you the value of an entire hand
value :: Hand -> Integer
value Empty = 0
value (Some card hand) = valueHelper (Some card hand)

value_high :: Hand -> Integer
value_high Empty = 0
value_high (Some card hand) = valueCard card + value hand

value_low :: Hand -> Integer
value_low hand = (value_high hand) - (numberOfAces hand)*10

valueHelper :: Hand -> Integer
valueHelper hand | (numberOfAces hand > 0) && (value_high hand > 21) = value_low hand
                 | otherwise = value_high hand

-- Gives you the number of aces in one hand
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Some (Card r s) hand) | r == Ace = 1 + numberOfAces hand
                                    | otherwise = numberOfAces hand

-- Tells you if the hand is bust or not
gameOver :: Hand -> Bool
gameOver hand = (value hand) > 21

-- Tells you who won out of the two hands.
winner :: Hand -> Hand -> Player
winner guest bank = winnerBust guest bank

-- For clarity
winnerScore :: Hand -> Hand -> Player
winnerScore guest bank | (value guest > value bank) = Guest
                       | otherwise = Bank
                       
-- For clarity
winnerBust :: Hand -> Hand -> Player
winnerBust guest bank | (gameOver bank && (not (gameOver guest))) = Guest
                      | (gameOver bank && gameOver guest) = Bank
                      | (gameOver guest) = Bank
                      | ((not (gameOver bank)) && (not (gameOver guest))) = winnerScore guest bank

-- Now, do I recurse over these or is there another way?
-- So, this took me a while. I'm still not sure. Damn this
-- is tricky.

-- ToDo: Fix this function. Fails on first property
-- Adds two hands together
infix 4 <+
(<+) :: Hand -> Hand -> Hand
Empty <+ hand = hand
hand <+ Empty = hand
(Some card hand) <+ h2 = Some card (hand <+ h2) -- mine v2.0 (wtf?)
--hand1 <+ hand2 = handHelper (handHelper Empty hand2) hand1
--(Some card hand) <+ h2 = hand <+ (Some card h2) -- mine
--(Some card hand) <+ h2 = Some card (hand <+ h2) -- Oxanas
--(<+)  (Some card hand) h2 =  Some card (hand <+ h2)

handHelper :: Hand -> Hand -> Hand
handHelper base_hand Empty = base_hand
handHelper base_hand (Some card hand) = handHelper (Some card base_hand) hand

-- Sorts a hand by suit and returns the different suits as separate hands
sortSuits :: Hand -> Hand -> Hand -> Hand -> Hand -> (Hand, Hand, Hand, Hand)
sortSuits Empty spades clubs hearts diamonds = (spades, clubs, hearts, diamonds)
sortSuits (Some (Card rank suit) hand) spades clubs hearts diamonds | suit == Spades = sortSuits hand (Some (Card rank suit) spades) clubs hearts diamonds
                                                                    | suit == Clubs = sortSuits hand spades (Some (Card rank suit) clubs) hearts diamonds
                                                                    | suit == Hearts = sortSuits hand spades clubs (Some (Card rank suit) hearts) diamonds
                                                                    | suit == Diamonds = sortSuits hand spades clubs hearts (Some (Card rank suit) diamonds)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = (p1 <+ (p2 <+ p3)) == ((p1 <+ p2) <+ p3)

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size (p1 <+ p2) == (size p1) + (size p2)

-- Okay, I need to think twice before doing these things
-- Gives you a full deck
fullDeck :: Hand
fullDeck =   ( (fullSuit Hearts) <+ (fullSuit Spades)) <+ ( (fullSuit Clubs) <+ (fullSuit Diamonds))

-- OK
-- Gives you one full suit
fullSuit_old :: Suit -> Hand
fullSuit_old suit = ((Some (Card (Jack) suit)
                    (Some (Card (Queen) suit)
                    (Some (Card (King) suit)
                    (Some (Card (Ace) suit) 
                    (Some (Card (Numeric 2) suit)
                    (Some (Card (Numeric 3) suit)
                    (Some (Card (Numeric 4) suit)
                    (Some (Card (Numeric 5) suit)
                    (Some (Card (Numeric 6) suit)
                    (Some (Card (Numeric 7) suit)
                    (Some (Card (Numeric 8) suit)
                    (Some (Card (Numeric 9) suit)
                    (Some (Card (Numeric 10) suit) Empty))))))))))))))
                
fullSuit :: Suit -> Hand
fullSuit suit = ((Some (Card (Jack) suit)
                  (Some (Card (Queen) suit)
                  (Some (Card (King) suit)
                  (Some (Card (Ace) suit) (fullSuitHelper Empty suit 10))))))
                
fullSuitHelper :: Hand -> Suit -> Integer -> Hand
fullSuitHelper hand suit 1 = hand
fullSuitHelper hand suit counter = fullSuitHelper (Some (Card (Numeric counter) suit) hand) suit (counter-1)
                
prop_fullSuit :: Suit -> Bool
prop_fullSuit s1 = fullSuit_old s1 == fullSuit s1
-- Draws the top card from the deck and puts it in the hand.
-- Returns the remaining deck and the new, increased hand.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty"
draw (Some topcard deck) hand = (deck, (Some topcard hand))

prop_draw :: Hand -> Hand -> Bool
prop_draw Empty hand = size Empty == 0
prop_draw deck hand = size deck == size deck'+1 where (deck', hand') = draw deck hand

-- Woohoo! look at me learning! where is a very useful keyword
-- Plays according to the banks rules. Up to 16.
playBank :: Hand -> Hand
playBank deck = res where (res, disc) = playBankHelper deck Empty

-- I cannot believe that just got accepted by Hugs
-- Helper for the playBank function
playBankHelper :: Hand -> Hand -> (Hand, Hand)
playBankHelper deck hand | value (hand) >= 16 = (hand, deck)
                         | otherwise = playBankHelper deck' hand' where (deck', hand') = draw deck hand


-- Todo: Johan. Make this a bit more radom ;-)
-- Shuffles a deck to make the game a little more interesting
shuffle :: StdGen -> Hand -> Hand
shuffle gen deck = shuffleHelper gen deck Empty


shuffleHelper :: StdGen -> Hand -> Hand -> Hand
shuffleHelper gen Empty ret = ret
shuffleHelper gen deck ret = shuffleHelper gen' the_deck (Some the_card ret) 
                             where ((the_deck, the_card),gen') = getRandomCard gen deck

getRandomCard :: StdGen -> Hand -> ((Hand, Card), StdGen)
getRandomCard gen deck = ((getNth deck Empty n), gen') 
                            where (gen', n) = getRandomN gen (size deck-1)

getRandomN :: StdGen -> Integer -> (StdGen, Integer)
getRandomN gen max = (gen', n) where (n, gen') = randomR (0,max) gen

getNth :: Hand -> Hand -> Integer -> (Hand, Card)
getNth (Some top_card source) dest n | n == 0 = ((source <+ dest), top_card)
                                     | otherwise = getNth source (Some top_card dest) (n-1)


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Some c' h) = c == c' || c `belongsTo` h
                                     
-- Something to do with the IO
implementation = Interface
  { iEmpty = empty
  , iFullDeck = fullDeck
  , iValue = value
  , iGameOver = gameOver
  , iWinner = winner
  , iDraw = draw
  , iPlayBank = playBank
  , iShuffle = shuffle
  }
  
-- To make it all tick
main :: IO()
main = runGame implementation
{-|

-}