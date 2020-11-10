module SharedBackend where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.IO

  type Game = [Round]
  type Round = [Player]
  type Player = (String,Int,Int)

  create_new_game :: UI [String] -> UI Game
  create_new_game player_list = do
    players <- player_list
    return ((map (create_player) players):[])

  get_player_list :: Game -> [String]
  get_player_list game = map (\(x,y,z) -> x) (head game)

  create_player :: String -> Player
  create_player name = (name,0,0)

  number_of_cards_round :: Game -> Int
  number_of_cards_round game = 
  -- si length game < 7 entonces length game
    if length game < 7
      then (length game)
      else 
        -- si 7 <= length game <= 7 + length player_list entonces 7
        if length game <= (7 + length (get_player_list game))
          then 7
          else
            -- si length game > 7 + length player_list entonces rounds_left player_list game
            rounds_left game

  rounds_left :: Game -> Int
  rounds_left game = (num_rounds (get_player_list game)) - length game

  num_rounds :: [String] -> Int
  num_rounds player_list = 14 + length player_list