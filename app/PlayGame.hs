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

    getBody w #+ main_div ([greet title] ++ players_container player_list ++ button_container main_menu_button)
    
    redirect_to_button "main_menu" setup w
  
  greet :: String -> UI Element
  greet name =
    UI.h2 #+ [string name ] # set UI.class_ "text-center text-light"

  players_container :: UI [String] -> [UI Element]
  players_container player_list = [UI.h4 # set UI.text "Jugadores: ", players_ul player_list]

  players_ul :: UI [String] -> UI Element
  players_ul players_list = do
    players <- players_list
    UI.ul # set UI.id_ "players_ul" #+ map (\name -> UI.li #+ [string name]) players