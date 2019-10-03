-- module PokerHandAnalyzer (
--     deck    
--     , handIdentification
--     , fromCardString
--     , fromCardStrings
-- ) where

import Data.List

-- Simple deck of cards
ranks = [0..12]
suits = [0..3]

-- card :: Int -> Int -> (Int, Int)
-- card r s = (r, s)

deck = [(r, s) | r <- ranks, s <- suits]


-- Constants
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

-- Use a Sum-free sequence for ranking cards: https://en.wikipedia.org/wiki/Sum-free_sequence
cardRankValueMap = zip ranks [2^n | n <- [0..12]]
pairedCardRankValueMap = zip ranks [2^n | n <- [13..25]]

wheelCardValueSum = 4111
wheelCardKickerValue = 8
straightCardValueSums = [wheelCardValueSum, 31, 62, 124, 248, 496, 992, 1984, 3968, 7936]


-- Hand identification
handIdentification :: [(Int, Int)] -> (String, Int)
handIdentification cards@[a,b,c,d,e] 
    | isAFlush == True && isAStraight == True = (straightFlush, straightFlushValue + getStraightKickerValue cards sumOfCardValues)
    | isFourOfAKind rankCounts == True = (fourOfAKind, getFourOfAKindValue rankCounts)
    | isFullHouse rankCounts == True = (fullHouse, getFullHouseValue rankCounts)
    | isAFlush == True = (flush, flushValue + sumOfCardValues)
    | isAStraight == True = (straight, straightValue + getStraightKickerValue cards sumOfCardValues)
    | isThreeOfAKind rankCounts == True = (threeOfAKind, getSetValue rankCounts)
    | isTwoPair rankCounts == True = (twoPair, getTwoPairValue rankCounts)
    | isOnePair rankCounts == True = (onePair, getOnePairValue rankCounts)
    | otherwise = (highCard, highCardValue + sumOfCardValues)
    where   sumOfCardValues = sumCardValues(cards)
            isAFlush = isFlush cards
            isAStraight = isStraight sumOfCardValues
            rankCounts = groupByRank cards

-- Error: wrong number of cards
handIdentification (x:xs) = ("There are five cards in a poker hand...received " ++ show (length (x:xs)), 0)

-- Hand Identification supporting functions:
isFlush :: [(Int, Int)] -> Bool
isFlush cards = all (== (head suits)) suits
    where suits = [s | (_, s) <- cards]

isStraight :: Int -> Bool
isStraight sumOfCardValues = elem sumOfCardValues straightCardValueSums

getStraightKickerValue :: [(Int, Int)] -> Int -> Int
getStraightKickerValue cards sumOfCardValues = if sumOfCardValues == wheelCardValueSum then wheelCardKickerValue else getRankValue (maximum (getRanks cards))

groupByRank :: [(Int, Int)] -> [(Int, Int)]
groupByRank cards = nub [(r1, length [r | (r, _) <- cards, r == r1]) | (r1, _) <- cards]

isFourOfAKind :: [(Int, Int)] -> Bool
isFourOfAKind rankCounts = if length rankCounts == 2 then (snd (head rankCounts)) `elem` [1, 4] else False

getFourOfAKindValue :: [(Int, Int)] -> Int
getFourOfAKindValue rankCounts = fourOfAKindValue + getPairedRankValue quadRank + getRankValue kickerRank
    where   quadRank = head [r | (r, c) <- rankCounts, c == 4]
            kickerRank = head [r | (r, c) <- rankCounts, c == 1]

isFullHouse :: [(Int, Int)] -> Bool
isFullHouse rankCounts = if length [c | (_, c) <- rankCounts, c == 3] == 1 && length [c | (_, c) <- rankCounts, c == 2] == 1 then True else False
                        
getFullHouseValue :: [(Int, Int)] -> Int
getFullHouseValue rankCounts = fullHouseValue + getPairedRankValue setRank + getRankValue kickerRank
    where   setRank = head [r | (r, c) <- rankCounts, c == 3]
            kickerRank = head [r | (r, c) <- rankCounts, c == 2]

isThreeOfAKind :: [(Int, Int)] -> Bool
isThreeOfAKind rankCounts = if length [c | (_, c) <- rankCounts, c == 3] == 1 && length [c | (_, c) <- rankCounts, c == 1] == 2 then True else False

getSetValue :: [(Int, Int)] -> Int
getSetValue rankCounts = threeOfAKindValue + getPairedRankValue setRank + valueOfKickers
    where   setRank = head [r | (r, c) <- rankCounts, c == 3]
            valueOfKickers = sum [getRankValue r | (r, c) <- rankCounts, c == 1]

isTwoPair :: [(Int, Int)] -> Bool
isTwoPair rankCounts = if length [c | (_, c) <- rankCounts, c == 2] == 2 then True else False

getTwoPairValue :: [(Int, Int)] -> Int
getTwoPairValue rankCounts = twoPairValue + valueOfPairs + valueOfKickers
    where   valueOfPairs = sum [getPairedRankValue r | (r, c) <- rankCounts, c == 2]
            valueOfKickers = sum [getRankValue r | (r, c) <- rankCounts, c == 1]

isOnePair :: [(Int, Int)] -> Bool
isOnePair rankCounts = if length [c | (_, c) <- rankCounts, c == 2] == 1 && length [c | (_, c) <- rankCounts, c == 1] == 3 then True else False

getOnePairValue :: [(Int, Int)] -> Int
getOnePairValue rankCounts = onePairValue + getPairedRankValue pairRank + valueOfKickers
    where   pairRank = head [r | (r, c) <- rankCounts, c == 2]
            valueOfKickers = sum [getRankValue r | (r, c) <- rankCounts, c == 1]
                                                                        
sumCardValues :: [(Int, Int)] -> Int
sumCardValues cards = sum (map (\(r, _) -> getRankValue r) cards)

getRanks cards = [r | (r, s) <- cards]

getRankValue :: Int -> Int
getRankValue rankToMatch = head [x | (r, x) <- cardRankValueMap, r == rankToMatch]

getPairedRankValue :: Int -> Int
getPairedRankValue rankToMatch = head [x | (r, x) <- pairedCardRankValueMap, r == rankToMatch]

-- Hand Identification Tests:
testStraightFlush = handIdentification [(5, 2), (6, 2), (7, 2), (8, 2), (9, 2)] == (straightFlush,  straightFlushValue + 512)
testWheelFlush = handIdentification [(3, 3), (1, 3), (2, 3), (12, 3), (0, 3)] == (straightFlush,  straightFlushValue + 8)
testQuads = handIdentification [(0, 1), (0, 2), (0, 3), (0, 4), (1, 1)] == (fourOfAKind, fourOfAKindValue +  8192 + 2)
testBoat = handIdentification [(0, 1), (0, 2), (0, 3), (1, 1), (1, 2)] == (fullHouse, fullHouseValue + 8192 + 2)
testFlush = handIdentification [(5, 1), (1, 1), (9, 1), (12, 1), (7, 1)] == (flush, flushValue + 32 + 2 + 512 + 4096 + 128)
testStraight = handIdentification [(5, 0), (6, 1), (7, 2), (8, 2), (9, 2)] == (straight, straightValue + 512)
testWheel = handIdentification [(3, 0), (2, 1), (1, 2), (0, 2), (12, 2)] == (straight, straightValue + 8)
testTrips = handIdentification [(5, 0), (5, 1), (5, 2), (8, 2), (9, 2)] == (threeOfAKind, threeOfAKindValue + 262144 + 256 + 512)
testTwoPair = handIdentification [(5, 0), (5, 1), (4, 2), (4, 2), (9, 2)] == (twoPair, twoPairValue + 262144 + 131072 + 512)
testOnePair = handIdentification [(5, 0), (5, 1), (4, 2), (3, 2), (9, 2)] == (onePair, onePairValue + 262144 + 16 + 8 + 512)
testHighCard = handIdentification [(8, 2), (1, 1), (9, 2), (11, 0), (6, 0)] == (highCard, highCardValue + 256 + 2 + 512 + 2048 + 64)

testTooManyCards = handIdentification [(0,0), (1,0), (2,0), (3,0), (4,0), (5, 0)] == ("There are five cards in a poker hand...received 6", 0)
testTooFewCards = handIdentification [(0,0), (1,0), (2,0), (3,0)] == ("There are five cards in a poker hand...received 4", 0)

testAllHandIdentification = testStraightFlush && testWheelFlush && testQuads && testBoat && testFlush && testStraight && testWheel && testTrips && testTwoPair && testOnePair && testHighCard && testTooManyCards && testTooFewCards


-- Convert to strings for presentation
rankStringMap = zip ranks "23456789TJQKA"
suitStringMap = zip suits "CDHS"

cardString :: (Int, Int) -> String
cardString (cardRank, cardSuit) = head [[rankString, suitString] | 
    (r, rankString) <- rankStringMap, 
    (s, suitString) <- suitStringMap, 
    r == cardRank, 
    s == cardSuit]

cardStrings :: [(Int, Int)] -> [String]
cardStrings cards = [cardString(x) | x <- cards]

fromCardString :: String -> (Int, Int)
fromCardString [rankString, suitString] = head [(r, s) | 
    (r, mappedRankString) <- rankStringMap,
    (s, mappedSuitString) <- suitStringMap,
    mappedRankString == rankString,
    mappedSuitString == suitString]

fromCardStrings :: [String] -> [(Int, Int)]
fromCardStrings cardStringList = [fromCardString cardStringInstance | cardStringInstance <- cardStringList]

--String Conversion Tests
testCardString = cardString(5, 2) == "7H"
testCardStrings = cardStrings [(0,0),(0,1)] == ["2C","2D"] && cardStrings deck == ["2C","2D","2H","2S","3C","3D","3H","3S","4C","4D","4H","4S","5C","5D","5H","5S","6C","6D","6H","6S","7C","7D","7H","7S","8C","8D","8H","8S","9C","9D","9H","9S","TC","TD","TH","TS","JC","JD","JH","JS","QC","QD","QH","QS","KC","KD","KH","KS","AC","AD","AH","AS"]
testFromCardString = fromCardString "7S" == (5, 3)
testFromCardStrings = fromCardStrings (cardStrings deck) == deck

testAllStringConversions = testCardString && testCardStrings && testFromCardString && testFromCardStrings
