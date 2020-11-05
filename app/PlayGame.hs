module PlayGame (render_play_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared
  import SharedBackend

  render_play_game :: UI String -> UI Game -> (Window -> UI ()) -> Window -> UI ()
  render_play_game game_title ui_game setup w = void $ do
    return w # set title "Jugando"
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "play_game.css"

    title <- game_title
    game <- ui_game
    getBody w #+ main_div ([greet title, board game] ++ button_container main_menu_button)
    
    redirect_to_button "main_menu" setup w
  
  greet :: String -> UI Element
  greet name =
    UI.h2 #+ [string name ] # set UI.class_ "text-center text-light"

  board :: Game -> UI Element
  board game = do
    let
      table = UI.table # set UI.id_ "board_table" # set UI.class_ "table table-bordered table-dark"
      players = get_player_list game
    table #+ (board_header players ++ create_inputs players)

  board_header :: [String] -> [UI Element]
  board_header players =
    let round_header = UI.td # set UI.id_ "round_header" #+ [string "Ronda"]
    in [UI.tr # set UI.id_ "board_headers" #+ (round_header:players_td_list players)]

  players_td_list :: [String] -> [UI Element]
  players_td_list players = map (\name -> UI.td #+ [string name]) players

  create_inputs :: [String] -> [UI Element]
  create_inputs players = 
    let button = [ UI.button # set UI.text "Predicciones" # set UI.class_ "btn btn-sm" # set UI.id_ "player_action" ]
    in [UI.tr # set UI.id_ "game_inputs" #+ (UI.td #+ button:(create_inputs_row players))]
    
  create_inputs_row :: [String] -> [UI Element]
  create_inputs_row = map (\_ -> UI.td #+ [UI.input])