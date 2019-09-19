-- Simple deck of cards
ranks = [0..12]
suits = [0..3]

card :: Int -> Int -> (Int, Int)
card r s = (r, s)

deck = [card r s | r <- ranks, s <- suits]


-- Hand identification
handIdentification :: [(Int, Int)] -> (String, Int, Integer)
handIdentification cards@[a,b,c,d,e] 
    | countUniqueSuits cards == 1 && isWheel cards == True = ("Straight Flush", 8, 3)
    | countUniqueSuits cards == 1 && isStraight cards == True = ("Straight Flush", 8, fromIntegral (maximum (getRanks cards)))
    | isQuads cards == True = ("Quads", 7, fromIntegral (100 * (fst (getQuadAndKickerRank cards) + 1) + snd (getQuadAndKickerRank cards)))
    | hasSet cards == True && hasExactlyOnePair cards == True = ("Boat", 6, fromIntegral (100 * (fst (getBoatAndKickerRank cards) + 1) + snd (getBoatAndKickerRank cards)))
    | countUniqueSuits cards == 1 = ("Flush", 5, sumByRank(cards))
    | isWheel cards == True = ("Straight", 4, 3)
    | isStraight cards == True = ("Straight", 4, fromIntegral (maximum (getRanks cards)))
    | hasSet cards == True = ("Trips", 3, getTripsValue cards)
    | hasExactlyTwoPair cards == True = ("Two Pair", 2, getTwoPairValue cards)
    | hasExactlyOnePair cards == True = ("One Pair", 1, getOnePairValue cards)
    | otherwise = ("High Card", 0, sumByRank(cards))

-- Error: wrong number of cards
handIdentification (x:xs) = ("There are five cards in a poker hand...you have supplied " ++ show (length (x:xs)), 0, 0)

-- Hand Identification supporting functions:

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
            kickersValue = sumByRank [(r, s) | (r, s) <- cards, r /= setRank]

getTwoPairValue :: [(Int, Int)] -> Integer
getTwoPairValue cards = fromIntegral ((maximum pairRanks + 1) * 10000) + fromIntegral ((minimum pairRanks + 1) * 100) + fromIntegral kickerRank
    where   pairRanks = getPairRanks cards
            kickerRank = head [r | (r, s) <- cards, elem r pairRanks /= True]

getOnePairValue :: [(Int, Int)] -> Integer
getOnePairValue cards = fromIntegral ((pairRank + 1) * 10000000000000) + kickersValue
    where   pairRank = head (getPairRanks cards)
            kickersValue = sumByRank [(r, s) | (r, s) <- cards, r /= pairRank]

isQuads :: [(Int, Int)] -> Bool
isQuads cards = length [r | (r, _) <- cards, countCardsWithRank cards r == 4] == 4

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

sumByRank :: [(Int, Int)] -> Integer
sumByRank cards = sum [getCardRankMultiplier(card) | card <- cards]

isStraight :: [(Int, Int)] -> Bool
isStraight cards
    | isWheel cards = True
    | otherwise = if elem (minRank + 4) ranks &&  elem (minRank + 3) ranks &&  elem (minRank + 2) ranks &&  elem (minRank + 1) ranks then True else False
    where   ranks = getRanks cards
            minRank = minimum ranks

isWheel :: [(Int, Int)] -> Bool
isWheel cards = if elem 12 ranks && elem 0 ranks && elem 1 ranks && elem 2 ranks && elem 3 ranks then True else False
    where ranks = getRanks cards

getRanks cards = [r | (r, s) <- cards]

getSuits cards = [s | (r, s) <- cards]

highCards cards = [x | x <- cards, fst x == maximum [fst y | y <- cards]]

cardRankValueMap = zip ranks [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000, 1000000000000]

getCardRankMultiplier :: (Int, Int) -> Integer
getCardRankMultiplier (rankToMatch, _) = getRankMultiplier rankToMatch

getRankMultiplier :: Int -> Integer
getRankMultiplier rankToMatch = head [x | (r, x) <- cardRankValueMap, r == rankToMatch]

-- Hand Identification Tests:
testStraightFlush = handIdentification [(5, 2), (6, 2), (7, 2), (8, 2), (9, 2)] == ("Straight Flush", 8, 9) && handIdentification [(3, 3), (1, 3), (2, 3), (12, 3), (0, 3)] == ("Straight Flush", 8, 3)
testQuads = handIdentification [(0, 1), (0, 2), (0, 3), (0, 4), (1, 1)] == ("Quads", 7, 101)
testBoat = handIdentification [(0, 1), (0, 2), (0, 3), (1, 1), (1, 2)] == ("Boat", 6, 101)
testFlush = handIdentification [(5, 1), (1, 1), (9, 1), (12, 1), (7, 1)] == ("Flush", 5, 1001010100010)
testStraight = handIdentification [(5, 0), (6, 1), (7, 2), (8, 2), (9, 2)] == ("Straight", 4, 9)
testTrips = handIdentification [(5, 0), (5, 1), (5, 2), (8, 2), (9, 2)] == ("Trips", 3, 60001100000000)
testTwoPair = handIdentification [(5, 0), (5, 1), (4, 2), (4, 2), (9, 2)] == ("Two Pair", 2, 60509)
testOnePair = handIdentification [(5, 0), (5, 1), (4, 2), (3, 2), (9, 2)] == ("One Pair", 1, 60001000011000)
testHighCard = handIdentification [(8, 2), (1, 1), (9, 2), (11, 0), (6, 0)] == ("High Card", 0, 101101000010)

testTooManyCards = handIdentification [(0,0), (1,0), (2,0), (3,0), (4,0), (5, 0)] == ("There are five cards in a poker hand...you have supplied 6", 0, 0)
testTooFewCards = handIdentification [(0,0), (1,0), (2,0), (3,0)] == ("There are five cards in a poker hand...you have supplied 4", 0, 0)

testCountSuits = countUniqueSuits [(1,0),(1,0),(1,0),(1,0),(2,3)] == 2
testIsStraight = isStraight [(1,0),(1,0),(1,0),(1,0),(2,3)] == False && isStraight [(4, 0), (7, 0), (3, 1), (5, 2), (6, 3)] == True && isStraight [(12, 0), (0, 0), (1, 1), (2, 2), (3, 3)] == True
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


