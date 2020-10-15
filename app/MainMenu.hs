import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad

import System.FilePath
import System.IO

import Statistics
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

      
  getBody w #+ main_div (greet:(button_container make_buttons))

  new_game_button <- getElementById w "new_game"
  load_game_button <- getElementById w "load_game"
  statistics_button <- getElementById w "statistics"
  main_div_element <- getElementById w "main_div"

  on UI.click (from_just new_game_button) $ const $ do
    delete (from_just main_div_element)
    render_new_page w render_new_game
  on UI.click (from_just load_game_button) $ const $ do
    delete (from_just main_div_element)
    render_new_page w render_load_game
  on UI.click (from_just statistics_button) $ const $ do
    delete (from_just main_div_element)
    render_new_page w render_load_game

greet :: UI Element
greet =
  UI.h1 #+ [string "Bienvenido al anotador piola!" ] # set UI.class_ "text-center text-light"

make_buttons :: [UI Element]
make_buttons = 
  [ UI.button # set UI.text "Cargar juego" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "load_game"
  , UI.button # set UI.text "Nuevo juego" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "new_game"
  , UI.button # set UI.text "Estadísticas" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "statistics"
  ]

getStaticDir :: IO FilePath
getStaticDir = return "static"

render_new_page :: Window -> (Window -> UI ()) -> UI ()
render_new_page w render_page = void $ do
  return w # set title "Cargar Juego"
  UI.addStyleSheet w "podrida.css"
  UI.addStyleSheet w "bootstrap.css"

  getBody w #+ main_div (button_container main_menu_button)

  main_menu_button <- getElementById w "main_menu"
  main_div_element <- getElementById w "main_div"

  on UI.click (from_just main_menu_button) $ const $ do
    delete (from_just main_div_element)
    setup w

  render_page w