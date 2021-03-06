import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad

import System.FilePath
import System.IO

import NewGame
import LoadGame
import Shared

-- | Main entry point.
main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = do
  -- active elements
  return w # set title "Podrida"
  UI.addStyleSheet w "podrida.css"
  UI.addStyleSheet w "bootstrap.css"

      
  getBody w #+ main_div ((greet "Bienvenido al anotador piola!"):(button_container make_buttons))

  redirect_to_button "new_game" (render_new_game setup) w
  redirect_to_button "load_game" (render_load_game setup) w

make_buttons :: [UI Element]
make_buttons = 
  [ UI.button # set UI.text "Cargar juego" #. "btn_init_page btn col-4" # set UI.id_ "load_game"
  , UI.button # set UI.text "Nuevo juego" #. "btn_init_page btn col-4" # set UI.id_ "new_game"
  ]

getStaticDir :: IO FilePath
getStaticDir = return "static"

