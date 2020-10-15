module Main where

import System.IO
---------------- Initialization ------------------
-- module Initialization where

type Game = [Round]
type Round = [Player]
type Player = (String,Int,Int)
-- player_name, prediction, won_round

-- calculates the amount of rounds for the game, based on the number of players
num_rounds :: [String] -> Int
num_rounds player_list = 14 + length player_list

rounds_left :: [String] -> Game -> Int
rounds_left player_list game = (num_rounds player_list) - length game

-- user interaction
play_round :: [String] -> Int -> IO Round
play_round player_list cards_amount = do
  putStrLn ("----------CANTIDAD DE CARTAS: " ++ show cards_amount ++ "----------\n")
  putStrLn "\n--------------PREDICCIONES!--------------\n"
  predicted_rounds <- (prediction_round player_list cards_amount 0)
  putStrLn "\nJugando, presione enter para finalizar ronda"
  getLine
  putStrLn "-----------------GANADAS!----------------\n"
  won_round predicted_rounds
  
prediction_round :: [String] -> Int -> Int -> IO Round
prediction_round [] cards_amount accumulated_predictions = return []
prediction_round (x:xs) cards_amount accumulated_predictions = do 
  putStrLn ("Ingrese la prediccion del jugador " ++ show x)
  player_prediction <- getLine
  if (length xs == 0) && (cards_amount == accumulated_predictions + (read player_prediction :: Int))
    then do
      putStrLn "No puede quedar la misma cantidad de predecidas que cartas\n"
      putStrLn "Siga participando"
      prediction_round (x:xs) cards_amount accumulated_predictions
    else 
      do
      rounds_predicted <- prediction_round xs cards_amount (accumulated_predictions + (read player_prediction :: Int))
      return ((x,(read player_prediction :: Int),0):rounds_predicted)
  
won_round :: Round -> IO Round
won_round [] = return []
won_round ((a,b,c):abcs) = do 
  putStrLn ("Ingrese la cantidad de manos que gano el jugador " ++ a)
  player_hands <- getLine 
  rounds_won <- won_round abcs
  return ((a,b,(read player_hands :: Int)):rounds_won)

-- recursion to create the game structure
game_loop :: [String] -> Game -> Int -> IO ()
game_loop (y:ys) game 0 = putStrLn "\n------------JUEGO TERMINADO!-------------"
game_loop (y:ys) game x = do
  putStrLn "\n-----------------------------------------\n"
  putStrLn ("Ronda " ++ show ((length game) + 1))
  next_round <- play_round (y:ys) (number_of_cards_round game (y:ys))
  print_scores (get_partial_score (next_round:game))
  game_loop (ys++[y]) (next_round:game) (x-1)  

number_of_cards_round :: Game -> [String] -> Int
number_of_cards_round game player_list = 
-- si length game < 7 entonces length game + 1
  if length game < 7
    then (length game) + 1
    else 
      -- si 7 <= length game <= 7 + length player_list entonces 7
      if length game <= (7 + length player_list)
        then 7
        else
          -- si length game > 7 + length player_list entonces rounds_left player_list game
          rounds_left player_list game


print_scores :: [(String, Int)] -> IO ()
print_scores points_list = do
  putStrLn (table_lines (length (pretty_names points_list)) '_')
  putStrLn (pretty_names points_list)
  putStrLn (table_lines (length (pretty_names points_list)) '=')
  putStrLn (pretty_points points_list)
  putStrLn (table_lines (length (pretty_names points_list)) '─')
  putStrLn "\n"

pretty_names :: [(String, Int)] -> String
pretty_names [] = "|"
pretty_names (player:list) = ("| " ++ (fst player) ++ " ") ++ (pretty_names list)

pretty_points :: [(String, Int)] -> String
pretty_points [] = "|"
pretty_points (player:list) = ("| " ++ (show (snd player)) ++ (get_spaces (length (fst player) - length (show (snd player)))) ++ " ") ++ (pretty_points list)

get_spaces :: Int -> String
get_spaces 0 = ""
get_spaces x = " " ++ (get_spaces (x-1)) 

table_lines :: Int -> Char -> String
table_lines 0 char = ""
table_lines x char = char : (table_lines (x-1) char)

-- returns a list of tuples with the player name and score so far
get_partial_score :: Game -> [(String, Int)]
get_partial_score game = map (\name -> (name,(get_player_score game name))) (get_player_names game)

-- returns a list of player names for a certain game 
get_player_names :: Game -> [String]
get_player_names game = map (\(x,y,z) -> x) (head game)

-- returns the current score for a certain player in a certain game
get_player_score :: Game -> String -> Int
get_player_score game name = foldr (\round -> (+) (get_round_player_score round name)) 0 game

-- returns the score of a round (should be previously set) for a player
get_round_player_score :: Round -> String -> Int
get_round_player_score round name = get_won (get_player_tuple round name) + (bonification_points (get_player_tuple round name))
  
-- calculates the amount of extra points (based on if the prediction was right)
bonification_points :: Player -> Int
bonification_points tuple = if get_predicted tuple == get_won tuple then (if (get_predicted tuple) > 3 then 20 else 10) else 0 

-- returns the player values from a round and the player name
get_player_tuple :: Round -> String -> Player
get_player_tuple round name = head (filter (\(x,y,z) -> x == name) round)

-- returns the player's prediction
get_predicted :: Player -> Int
get_predicted = \(x,y,z) -> y

-- returns the hands won by the player
get_won :: Player -> Int
get_won = \(x,y,z) -> z

------------------ Game ------------------
get_players :: [String] -> IO [String]
get_players player_list = do
  name <- getLine
  if name == ""
    then return player_list
    else 
      if name_present player_list name
        then do
          putStrLn "-----Nombre repetido, ingrese otro-------"
          get_players player_list
        else
          get_players (name:player_list)

-- generic_input :: (String -> Bool) -> ([String] -> String -> Bool) -> String -> [String] -> IO [String]
-- generic_input base_condition wrong_condition error_message elements = do
--   input <- getLine
--   if base_condition input
--     then return elements
--     else 
--       if (wrong_condition elements input)
--         then do
--           putStrLn error_message
--           generic_input base_condition wrong_condition error_message elements
--         else generic_input base_condition wrong_condition error_message (input:elements)


name_present :: [String] -> String -> Bool
name_present player_list name = (length (filter (\x -> x == name) player_list)) > 0

main :: IO ()
main = do
  putStrLn "\n------Bienvenidos al anotador piola------\n"
  putStrLn "Quienes juegan?"
  putStrLn "Escribir los nombres separados por enter"
  player_list <- get_players []
  -- player_list <- generic_input (\x -> x=="") (name_present) ("El nombre esta repetido, ingrese otro") []
  game_loop player_list [] (num_rounds player_list)


