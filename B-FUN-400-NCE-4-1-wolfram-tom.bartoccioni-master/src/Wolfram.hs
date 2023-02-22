module Wolfram
    ( wolfram
    ) where

import System.Environment
import System.Exit
import Data.Char
import Data.List

isThisNumber :: [Char] -> Bool
isThisNumber [] = True
isThisNumber (x:xs)
    | (x == '-') || isDigit x = isThisNumber xs
    | otherwise = False

makeParsing :: ((Int, Int, Int, Int, Int), (String, String), (Int, Int)) -> [String] -> ((Int, Int, Int, Int, Int), (String, String), (Int, Int))
makeParsing ((rule, start, line, window, move), (myLine, lastLine), (lineNb, lineLength)) [] = ((rule, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
makeParsing ((rule, start, line, window, move), (myLine, lastLine), (lineNb, lineLength)) (x:xs)
    | x == "--rule" = if isThisNumber (head xs) then makeParsing ((read (head xs) :: Int, start, line, window, move), (myLine, lastLine), (lineNb, lineLength)) (tail xs) else ((0, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | x == "--start" = if isThisNumber (head xs) then makeParsing ((rule, read (head xs) :: Int, line, window, move), (myLine, lastLine), (lineNb, lineLength)) (tail xs) else ((0, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | x == "--lines" = if isThisNumber (head xs) then makeParsing ((rule, start, read (head xs) :: Int, window, move), (myLine, lastLine), (lineNb, lineLength)) (tail xs) else ((0, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | x == "--window" = if isThisNumber (head xs) then makeParsing ((rule, start, line, read (head xs) :: Int, move), (myLine, lastLine), (lineNb, lineLength)) (tail xs) else ((0, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | x == "--move" = if isThisNumber (head xs) then makeParsing ((rule, start, line, window, (read (head xs) :: Int) + 1), (myLine, lastLine), (lineNb, lineLength)) (tail xs) else ((0, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | otherwise = ((0, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))

takeThreeChar :: String -> String
takeThreeChar str
    = head str : head (tail str) : head (tail (tail str)) : ""

doRule :: ((Int, Int, Int, Int, Int), (String, String), (Int, Int)) -> ((Int, Int, Int, Int, Int), (String, String), (Int, Int))
doRule ((30, start, line, window, move), (myLine, lastLine), (lineNb, lineLength)) = doRule30 ((30, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
doRule ((90, start, line, window, move), (myLine, lastLine), (lineNb, lineLength)) = doRule90 ((90, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
doRule ((110, start, line, window, move), (myLine, lastLine), (lineNb, lineLength)) = doRule110 ((110, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))

doRule30 :: ((Int, Int, Int, Int, Int), (String, String), (Int, Int)) -> ((Int, Int, Int, Int, Int), (String, String), (Int, Int))
doRule30 ((30, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | lineLength == lineNb * 2 + 1 = ((30, start, line, window, move), ([], myLine ++ "  "), (lineNb + 1, lineLength))
    | null myLine = doRule30 ((30, start, line, window, move), ("  ", lastLine), (lineNb, 0))
    | takeThreeChar lastLine == "***" = doRule30 ((30, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "** " = doRule30 ((30, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "* *" = doRule30 ((30, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "*  " = doRule30 ((30, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == " **" = doRule30 ((30, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == " * " = doRule30 ((30, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "  *" = doRule30 ((30, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "   " = doRule30 ((30, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))

doRule90 :: ((Int, Int, Int, Int, Int), (String, String), (Int, Int)) -> ((Int, Int, Int, Int, Int), (String, String), (Int, Int))
doRule90 ((90, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | lineLength == lineNb * 2 + 1 = ((90, start, line, window, move), ([], myLine ++ "  "), (lineNb + 1, lineLength))
    | null myLine = doRule90 ((90, start, line, window, move), ("  ", lastLine), (lineNb, 0))
    | takeThreeChar lastLine == "***" = doRule90 ((90, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "** " = doRule90 ((90, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "* *" = doRule90 ((90, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "*  " = doRule90 ((90, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == " **" = doRule90 ((90, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == " * " = doRule90 ((90, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "  *" = doRule90 ((90, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "   " = doRule90 ((90, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))

doRule110 :: ((Int, Int, Int, Int, Int), (String, String), (Int, Int)) -> ((Int, Int, Int, Int, Int), (String, String), (Int, Int))
doRule110 ((110, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | lineLength == lineNb * 2 + 1 = ((110, start, line, window, move), ([], myLine ++ "  "), (lineNb + 1, lineLength))
    | null myLine = doRule110 ((110, start, line, window, move), ("  ", lastLine), (lineNb, 0))
    | takeThreeChar lastLine == "***" = doRule110 ((110, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "** " = doRule110 ((110, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "* *" = doRule110 ((110, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "*  " = doRule110 ((110, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == " **" = doRule110 ((110, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == " * " = doRule110 ((110, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "  *" = doRule110 ((110, start, line, window, move), (myLine ++ "*", tail lastLine), (lineNb, lineLength + 1))
    | takeThreeChar lastLine == "   " = doRule110 ((110, start, line, window, move), (myLine ++ " ", tail lastLine), (lineNb, lineLength + 1))

doSpace :: Int -> IO()
doSpace x
    | x < 1 = putStr ""
    | otherwise = do
    putStr " "
    doSpace (x - 1)

cutPrint :: String -> Int-> Int -> String
cutPrint str x y
    | x < 1 && y < 1 = str
    | x > 0 = cutPrint (tail str) (x - 1) y
    | y > 0 = cutPrint (init str) x (y - 1)

doWolfram :: ((Int, Int, Int, Int, Int), (String, String), (Int, Int)) -> IO()
doWolfram ((rule, start, line, window, move), (myLine, lastLine), (lineNb, lineLength))
    | rule == 0  = do
        putStrLn "Invalid arguments"
        exitWith (ExitFailure 84)
    | lineNb == line + start + 1 = putStr ""
    | lineNb < start + 1 = doWolfram (doRule ((rule, start, line, window, move), ([], lastLine), (lineNb, 0)))
    | otherwise  = do
        doSpace ((window `div` 2) + move - lineNb)
        if (lineNb `div` ((window `div` 2) + move)) > 0 && ((lineNb - (1 + (window `mod` 2))) `div` ((window `div` 2) - move)) > 0 then putStr (cutPrint lastLine (2 - move + (lineNb - (window `div` 2))) (2 + move + ((lineNb - (1 + (window `mod` 2))) - (window `div` 2))))
        else if (lineNb `div` ((window `div` 2) + move)) > 0 && move < 0 then putStr (cutPrint lastLine (2 + (lineNb - ((window `div` 2) + move))) 2)
        else if ((lineNb - (1 + (window `mod` 2))) `div` ((window `div` 2) - move)) > 0 && move > 0 then putStr (cutPrint lastLine 2 (2 + ((lineNb - (1 + (window `mod` 2))) - ((window `div` 2) - move))))
        else putStr (cutPrint lastLine 2 2)
        doSpace ((window `div` 2) - move - (lineNb - (1 + (window `mod` 2))))
        putStr "\n"
        doWolfram (doRule ((rule, start, line, window, move), ([], lastLine), (lineNb, 0)))

wolfram :: IO()
wolfram = do
    args <- getArgs
    if even (length args) && not (null args) then return () else exitWith (ExitFailure 84)
    let tmpArgs = makeParsing ((0, 0, -1, 80, 1), ([], "  *  "), (1, 1)) args
    doWolfram tmpArgs