module PlayGame (render_play_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared

  render_play_game :: UI String -> UI [String] -> (Window -> UI ()) -> Window -> UI ()
  render_play_game game player_list setup w = void $ do
    return w # set title "Jugando"
    UI.addStyleSheet w "podrida.css"
    UI.addStyleSheet w "bootstrap.css"
    
    -- getBody w #+ main_div ((button_container main_menu_button) ++ [greet game])   
    
    redirect_to_button "main_menu" setup w
  
  greet :: String -> UI Element
  greet name =
    UI.h1 #+ [string name ] # set UI.class_ "text-center text-light"