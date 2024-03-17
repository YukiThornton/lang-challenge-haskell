module Main where

import Rule
import Computer (randomHand)

main :: IO ()
main = do
    putStrLn "Enter r for ✊, p for ✋ or s for ✌\n"
    game

game :: IO ()
game = do
        putStrLn "Rock, paper, scissors, shoot!\n"
        computerHand <- randomHand
        maybeHand <- fmap handFromLine $ getLine
        case maybeHand of
            Just hand -> do
                let result = fight hand computerHand
                putStrLn ("(You) " ++ (handToEmoji hand) ++ " VS " ++ (handToEmoji computerHand) ++ " (Computer)")
                putStrLn (resultText result)
                case result of
                    Draw -> game
                    _ -> putStrLn("Come again!")
            Nothing -> putStrLn("Interesting choice!")
                       >> putStrLn("Bye.")

handFromLine :: String -> Maybe Hand
handFromLine "r" = Just Rock
handFromLine "p" = Just Paper
handFromLine "s" = Just Scissors
handFromLine _ = Nothing

handToEmoji :: Hand -> String
handToEmoji Rock = "✊"
handToEmoji Paper = "✋"
handToEmoji Scissors = "✌"

resultText :: FightResult -> String
resultText Win = "You Win !!!\n"
resultText Draw = "Draw !\n"
resultText Lose = "Hahaha\n"
