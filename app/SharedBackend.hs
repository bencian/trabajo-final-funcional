module SharedBackend where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import Data.Binary

  import System.IO
  import System.Directory
  import Data.Char
  import Data.List

  import Shared

  type Game = ([Round],[String])
  type Round = [Player]
  type Player = (String,Int,Int)

  create_new_game :: UI [String] -> UI Game
  create_new_game player_list = do
    players <- player_list
    return ([],players)

  get_player_list :: [Round] -> [String]
  get_player_list game = map (\(x,y,z) -> x) (head game)

  create_round :: [String] -> [String] -> Round
  create_round [] [] = []
  create_round (player:players) (value:values) = (create_player player (read value :: Int)):(create_round players values)
  
  create_player :: String -> Int -> Player
  create_player name prediction = (name,prediction,0)

  number_of_cards_round :: [Round] -> Int
  number_of_cards_round game = 
  -- si length game < 7 entonces length game
    if length game < 7
      then (length game) + 1
      else 
        -- si 7 <= length game <= 7 + length player_list entonces 7
        if length game <= (7 + length (get_player_list game))
          then 7
          else
            -- si length game > 7 + length player_list entonces rounds_left player_list game
            rounds_left game

  rounds_left :: [Round] -> Int
  rounds_left game = (num_rounds (get_player_list game)) - length game

  num_rounds :: [String] -> Int
  num_rounds player_list = 14 + length player_list

  insert_values_into_round :: Round -> [String] -> Round
  insert_values_into_round [] [] = []
  insert_values_into_round ((name,predicted,won):player_tuples) (value:values) = (name,predicted,(read value :: Int)):(insert_values_into_round player_tuples values) 

  get_player_score :: [Round] -> String -> Int
  get_player_score game name = foldr (\round -> (+) (get_round_player_score round name)) 0 game

  get_round_player_score :: Round -> String -> Int
  get_round_player_score round name = get_won (get_player_tuple round name) + (bonification_points (get_player_tuple round name))

  get_player_tuple :: Round -> String -> Player
  get_player_tuple round name = head (filter (\(x,y,z) -> x == name) round)

  bonification_points :: Player -> Int
  bonification_points tuple = if get_predicted tuple == get_won tuple then (if (get_predicted tuple) > 3 then 20 else 10) else 0 

  get_predicted :: Player -> Int
  get_predicted = \(x,y,z) -> y

  get_won :: Player -> Int
  get_won = \(x,y,z) -> z

  valid_card_inputs :: [String] -> Int -> (Int -> Int -> Bool) -> Bool
  valid_card_inputs values_list number_of_cards equality_function = (no_empty_inputs values_list) && (valid_numeric_inputs values_list number_of_cards) && (equality_function (sum (map (\x -> read x :: Int) values_list)) number_of_cards)

  error_message :: String
  error_message = "Error en los inputs, intente nuevamente"

  if' :: Bool -> a -> a -> a
  if' True  x _ = x
  if' False _ y = y

  no_empty_inputs :: [String] -> Bool
  no_empty_inputs = all (/= [])

  valid_numeric_inputs :: [String] -> Int -> Bool
  valid_numeric_inputs inputs cards = all (\input -> (all isDigit input) && (input_between_0_and_cards cards input)) inputs

  input_between_0_and_cards :: Int -> String -> Bool
  input_between_0_and_cards cards input = ((read input :: Int) >= 0) && ((read input :: Int) <= cards)

  last_round_won_values_sum :: [Round] -> Int
  last_round_won_values_sum [] = 0
  last_round_won_values_sum game = sum (map get_won (head (reverse game)))

  is_game_over :: [Round] -> Bool
  is_game_over [] = False
  is_game_over game = (is_current_round_over game) && (rounds_left game == 0)

  is_current_round_over :: [Round] -> Bool
  is_current_round_over [] = True
  is_current_round_over game = last_round_won_values_sum (game) /= 0

  no_repeated_names :: [String] -> Bool
  no_repeated_names names = no_repeats names names

  no_repeats :: [String] -> [String] -> Bool
  no_repeats [] _ = True
  no_repeats (name:names) list = (unique_name list name) && (no_repeats names list)

  unique_name :: [String] -> String -> Bool
  unique_name player_list name = (length (filter (\x -> x == name) player_list)) == 1

  main_path :: String
  main_path = "saved_games/"

  file_prefix :: String
  file_prefix = "game_"

  file_suffix :: String
  file_suffix = ".pdrd"

  save_file :: String -> Game -> IO ()
  save_file game_title game = encodeFile (main_path ++ file_prefix ++ game_title ++ file_suffix) (show game)

  open_file :: String -> IO Game
  open_file filename = do 
    file_string <- decodeFile (main_path ++ filename) :: IO String
    return (read file_string :: Game)

  delete_file :: String -> IO ()
  delete_file filename = removeFile (main_path ++ filename)

  load_game_list :: IO [String]
  load_game_list = do
    list_all_files <- (getDirectoryContents main_path)
    return (filter correct_file list_all_files)

  correct_file :: String -> Bool
  correct_file file_name = (isSuffixOf file_suffix file_name) && (isPrefixOf file_prefix file_name)
  
  remove_affixes :: String -> String
  remove_affixes file_string = stripSuffix (from_just (stripPrefix file_prefix file_string))

  stripSuffix :: String -> String
  stripSuffix file_string = reverse (from_just (stripPrefix (reverse file_suffix) (reverse file_string)))