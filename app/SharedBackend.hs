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
