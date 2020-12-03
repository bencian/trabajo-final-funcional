module LoadGame (render_load_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared
  import SharedBackend
  import PlayGame

  render_load_game :: (Window -> UI ()) -> Window -> UI ()
  render_load_game setup w = void $ do
    return w # set title "Otro Juego"
    UI.addStyleSheet w "podrida.css"
    UI.addStyleSheet w "bootstrap.css"

    games <- (liftIO load_game_list)
    
    getBody w #+ main_div ((game_list games) ++ button_container main_menu_button)
    
    redirect_to_button "main_menu" setup w

  game_list :: [String] -> [UI Element]
  game_list filtered_game_list = [ UI.ul # set UI.id_ "game_list" #. "list-group" #+ (game_elements filtered_game_list)]

  game_elements :: [String] -> [UI Element]
  game_elements [] = []
  game_elements (game:games) = (game_element game):(game_elements games)

  game_element :: String -> UI Element
  game_element string = UI.li #. "list-group-item" # set UI.text string

    -- (render_play_game game_title (create_new_game player_list) setup) w