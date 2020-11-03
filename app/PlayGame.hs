module PlayGame (render_play_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared

  render_play_game :: UI String -> UI [String] -> (Window -> UI ()) -> Window -> UI ()
  render_play_game game_title player_list setup w = void $ do
    return w # set title "Jugando"
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "play_game.css"

    title <- game_title

    getBody w #+ main_div ([greet title, board player_list] ++ button_container main_menu_button)
    
    redirect_to_button "main_menu" setup w
  
  greet :: String -> UI Element
  greet name =
    UI.h2 #+ [string name ] # set UI.class_ "text-center text-light"

  board :: UI [String] -> UI Element
  board players_list = do
    players <- players_list
    let table = UI.table # set UI.id_ "board_table" # set UI.class_ "table table-bordered table-dark"
    table #+ board_header players

  board_header :: [String] -> [UI Element]
  board_header players =
    let round_header = UI.td # set UI.id_ "round_header" #+ [string "Ronda"]
    in [UI.tr # set UI.id_ "board_headers" #+ (round_header:players_td_list players)]

  players_td_list :: [String] -> [UI Element]
  players_td_list players = map (\name -> UI.td #+ [string name]) players