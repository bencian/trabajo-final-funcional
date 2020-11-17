module SharedBackend where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.IO

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

  valid_values :: [String] -> Int -> Bool
  valid_values values_list number_of_cards = sum (map (\x -> read x :: Int) values_list) /= number_of_cards

  if' :: Bool -> a -> a -> a
  if' True  x _ = x
  if' False _ y = y