module Rule where

data Hand = Rock | Paper | Scissors deriving (Eq, Show)
data FightResult = Win | Lose | Draw deriving (Eq, Show)

winningHands :: [(Hand, Hand)]
winningHands = zip orderedHands $ drop 1 $ cycle orderedHands
    where orderedHands = [Paper, Rock, Scissors]

fight :: Hand -> Hand -> FightResult
fight self opponent
    | self == opponent = Draw
    | (self, opponent) `elem` winningHands = Win
    | otherwise = Lose
