module NewGame (render_new_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared

  render_new_game :: (Window -> UI ()) -> Window -> UI ()
  render_new_game setup w = void $ do
    return w # set title "Nuevo Juego"
    UI.addStyleSheet w "podrida.css"
    UI.addStyleSheet w "bootstrap.css"
    
    getBody w #+ main_div (button_container main_menu_button)
    
    redirect_to_button "main_menu" setup w