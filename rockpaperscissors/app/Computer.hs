module Computer where

import Control.Monad.Random.Lazy
import Rule

randomHandInt :: RandomGen g => Rand g Int
randomHandInt = getRandomR(1,3)

randomHand :: IO Hand
randomHand = fmap getHand $ evalRandIO randomHandInt
    where 
        getHand n = case n of
                        1 -> Rock
                        2 -> Paper
                        _ -> Scissors
