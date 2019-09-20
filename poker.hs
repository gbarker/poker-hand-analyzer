-- Simple deck of cards
ranks = [0..12]
suits = [0..3]

card :: Int -> Int -> (Int, Int)
card r s = (r, s)

deck = [card r s | r <- ranks, s <- suits]

-- Some constants
highCard = "High Card"
highCardValue = 0
onePair = "One Pair"
onePairValue =  67108864 
twoPair = "Two Pair"
twoPairValue =  134217728 
threeOfAKind = "Three of a Kind"
threeOfAKindValue =  268435456 
straight = "Straight"
straightValue =  536870912 
flush = "Flush"
flushValue =  1073741824
fullHouse = "Full House"
fullHouseValue =  2147483648 
fourOfAKind = "Four of a Kind"
fourOfAKindValue =  4294967296 
straightFlush = "Straight Flush"
straightFlushValue =  8589934592 

--handTypes = [highCard, onePair, twoPair, threeOfAKind, straight, flush, fullHouse, fourOfAKind, straightFlush]

-- Use a Sum-free sequence for ranking cards: https://en.wikipedia.org/wiki/Sum-free_sequence
cardRankValueMap = zip ranks [2^n | n <- [0..12]]
pairedCardRankValueMap = zip ranks [2^n | n <- [13..25]]
--handTypeValueMap = [2^n | n <- [26..34]]

wheelCardValueSum = 4111
straightCardValueSums = [wheelCardValueSum, 31, 62, 124, 248, 496, 992, 1984, 3968, 7936]

-- Hand identification
handIdentification :: [(Int, Int)] -> (String, Integer)
handIdentification cards@[a,b,c,d,e] 
    | isAFlush == True && isAStraight == True = (straightFlush, straightFlushValue + getStraightKickerValue cards sumOfCardValues)
    | isQuads cards == True = (fourOfAKind, fourOfAKindValue + 0)
    | hasSet cards == True && hasExactlyOnePair cards == True = (fullHouse, fullHouseValue + 0)
    | isAFlush == True = (flush, flushValue + sumOfCardValues)
    | isAStraight == True = (straight, straightValue + getStraightKickerValue cards sumOfCardValues)
    | hasSet cards == True = (threeOfAKind, 0)
    | hasExactlyTwoPair cards == True = (twoPair, 0)
    | hasExactlyOnePair cards == True = (onePair, 0)
    | otherwise = (highCard, highCardValue + sumOfCardValues)
    where   sumOfCardValues= sumCardValues(cards)
            isAFlush = isFlush cards
            isAStraight = isStraight sumOfCardValues

-- Error: wrong number of cards
handIdentification (x:xs) = ("There are five cards in a poker hand...you have supplied " ++ show (length (x:xs)), 0)

-- Hand Identification supporting functions:

isFlush :: [(Int, Int)] -> Bool
isFlush (x:[]) = True
isFlush ((_, s):xs) = if s == head [suit | (rank, suit) <- xs] then isFlush xs else False

isStraight :: Integer -> Bool
isStraight sumOfCardValues = elem sumOfCardValues straightCardValueSums

getStraightKickerValue :: [(Int, Int)] -> Integer -> Integer
getStraightKickerValue cards sumOfCardValues = fromIntegral (if sumOfCardValues == wheelCardValueSum then 5 else maximum (getRanks cards))

-- todo: this can be optimized...right now it's scanning all 5 cards one per card...some kind of running count that stops when we hit four for a single rank?
isQuads :: [(Int, Int)] -> Bool
isQuads cards = length [r | (r, _) <- cards, countCardsWithRank cards r == 4] == 4


            
countUniqueSuits :: [(Int, Int)] -> Int
countUniqueSuits [] = 0
countUniqueSuits ((r, s):xs) = (if elem s (getSuits xs) then 0 else 1) + countUniqueSuits xs

countUniqueRanks :: [(Int, Int)] -> Int
countUniqueRanks [] = 0
countUniqueRanks ((r, s):xs) = (if elem r (getRanks xs) then 0 else 1) + countUniqueRanks xs

countCardsWithRank :: [(Int, Int)] -> Int -> Int
countCardsWithRank cards rank = length [(r, s) | (r, s) <- cards, r == rank]

getQuadAndKickerRank :: [(Int, Int)] -> (Int, Int)
getQuadAndKickerRank cards@((firstRank, _):xs) = if countOfFirstRank == 4 then (firstRank, secondRank) else (secondRank, firstRank)
    where   countOfFirstRank = countCardsWithRank cards firstRank
            secondRank = head [x | (x, _) <- cards, x /= firstRank]

getBoatAndKickerRank :: [(Int, Int)] -> (Int, Int)
getBoatAndKickerRank cards@((firstRank, _):xs) = if countOfFirstRank == 3 then (firstRank, secondRank) else (secondRank, firstRank)
    where   countOfFirstRank = countCardsWithRank cards firstRank
            secondRank = head [x | (x, _) <- cards, x /= firstRank]

hasSet :: [(Int, Int)] -> Bool
hasSet cards = length [r | (r, _) <- cards, countCardsWithRank cards r == 3] == 3

getTripsValue :: [(Int, Int)] -> Integer
getTripsValue cards = fromIntegral ((setRank + 1) * 10000000000000) + kickersValue
    where   setRank = getSetRank cards
            kickersValue = sumCardValues [(r, s) | (r, s) <- cards, r /= setRank]

getTwoPairValue :: [(Int, Int)] -> Integer
getTwoPairValue cards = fromIntegral ((maximum pairRanks + 1) * 10000) + fromIntegral ((minimum pairRanks + 1) * 100) + fromIntegral kickerRank
    where   pairRanks = getPairRanks cards
            kickerRank = head [r | (r, s) <- cards, elem r pairRanks /= True]

getOnePairValue :: [(Int, Int)] -> Integer
getOnePairValue cards = fromIntegral ((pairRank + 1) * 10000000000000) + kickersValue
    where   pairRank = head (getPairRanks cards)
            kickersValue = sumCardValues [(r, s) | (r, s) <- cards, r /= pairRank]

hasExactlyOnePair :: [(Int, Int)] -> Bool
hasExactlyOnePair cards = length [r | (r, _) <- cards, countCardsWithRank cards r == 2] == 2

hasExactlyTwoPair :: [(Int, Int)] -> Bool
hasExactlyTwoPair cards = length [r | (r, _) <- cards, countCardsWithRank cards r == 2] == 4

getSetRank :: [(Int, Int)] -> Int
getSetRank cards = head [r | (r, _) <- cards, countCardsWithRank cards r == 3]

getPairRanks :: [(Int, Int)] -> [Int]
getPairRanks cards = if minPair == maxPair then [minPair] else [minPair, maxPair]
    where   pairs = [r | (r, _) <- cards, countCardsWithRank cards r == 2]
            minPair = minimum pairs
            maxPair = maximum pairs

sumCardValues :: [(Int, Int)] -> Integer
sumCardValues cards = sum [getCardRankValue(card) | card <- cards]

getRanks cards = [r | (r, s) <- cards]

getSuits cards = [s | (r, s) <- cards]

highCards cards = [x | x <- cards, fst x == maximum [fst y | y <- cards]]

getCardRankValue :: (Int, Int) -> Integer
getCardRankValue (rankToMatch, _) = getRankValue rankToMatch

getRankValue :: Int -> Integer
getRankValue rankToMatch = head [x | (r, x) <- cardRankValueMap, r == rankToMatch]

--getHandTypeValue :: String -> Integer
--getHandTypeValue typeToMatch = head [x | (t, x <- handTypeValueMap, t == typeToMatch)]

-- Hand Identification Tests:
testStraightFlush = handIdentification [(5, 2), (6, 2), (7, 2), (8, 2), (9, 2)] == ("Straight Flush",  8589934592 + 512) && handIdentification [(3, 3), (1, 3), (2, 3), (12, 3), (0, 3)] == ("Straight Flush",  8589934592 + 8)
testQuads = handIdentification [(0, 1), (0, 2), (0, 3), (0, 4), (1, 1)] == ("Quads", 101)
testBoat = handIdentification [(0, 1), (0, 2), (0, 3), (1, 1), (1, 2)] == ("Boat", 101)
testFlush = handIdentification [(5, 1), (1, 1), (9, 1), (12, 1), (7, 1)] == ("Flush", 1001010100010)
testStraight = handIdentification [(5, 0), (6, 1), (7, 2), (8, 2), (9, 2)] == ("Straight", 9)
testTrips = handIdentification [(5, 0), (5, 1), (5, 2), (8, 2), (9, 2)] == ("Trips", 60001100000000)
testTwoPair = handIdentification [(5, 0), (5, 1), (4, 2), (4, 2), (9, 2)] == ("Two Pair", 60509)
testOnePair = handIdentification [(5, 0), (5, 1), (4, 2), (3, 2), (9, 2)] == ("One Pair", 60001000011000)
testHighCard = handIdentification [(8, 2), (1, 1), (9, 2), (11, 0), (6, 0)] == ("High Card", 101101000010)

testTooManyCards = handIdentification [(0,0), (1,0), (2,0), (3,0), (4,0), (5, 0)] == ("There are five cards in a poker hand...you have supplied 6", 0)
testTooFewCards = handIdentification [(0,0), (1,0), (2,0), (3,0)] == ("There are five cards in a poker hand...you have supplied 4", 0)

testCountSuits = countUniqueSuits [(1,0),(1,0),(1,0),(1,0),(2,3)] == 2
testIsStraight = isStraight (sumCardValues [(1,0),(1,0),(1,0),(1,0),(2,3)]) == False && isStraight (sumCardValues [(4, 0), (7, 0), (3, 1), (5, 2), (6, 3)]) == True && isStraight (sumCardValues [(12, 0), (0, 0), (1, 1), (2, 2), (3, 3)]) == True
testHighCards = highCards [(0,0),(0,1),(1,1),(1,2)] == [(1,1),(1,2)]


-- Convert to strings for presentation
rankStringMap = zip ranks "23456789TJQKA"
suitStringMap = zip suits "CDHS"

cardString :: (Int, Int) -> [Char]
cardString (cardRank, cardSuit) = head [[rankString, suitString] | 
    (rank, rankString) <- rankStringMap, 
    (suit, suitString) <- suitStringMap, 
    rank == cardRank, 
    suit == cardSuit]

cardStrings cards = [cardString(x) | x <- cards]

--String Conversion Tests
testCardString = cardString(5, 2) == "7H"
testCardStrings = cardStrings [(0,0),(0,1)] == ["2C","2D"] && cardStrings deck == ["2C","2D","2H","2S","3C","3D","3H","3S","4C","4D","4H","4S","5C","5D","5H","5S","6C","6D","6H","6S","7C","7D","7H","7S","8C","8D","8H","8S","9C","9D","9H","9S","TC","TD","TH","TS","JC","JD","JH","JS","QC","QD","QH","QS","KC","KD","KH","KS","AC","AD","AH","AS"]


