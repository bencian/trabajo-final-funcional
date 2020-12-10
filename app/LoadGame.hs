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
    return w # set title "Cargar Juego"
    UI.addStyleSheet w "podrida.css"
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "load_game.css"

    games <- (liftIO load_game_list)

    let 
      game_list :: [String] -> [UI Element]
      game_list filtered_game_list = [ UI.ul # set UI.id_ "game_list" #. "list-group" #+ (game_elements filtered_game_list)]

      game_elements :: [String] -> [UI Element]
      game_elements [] = []
      game_elements (game:games) = (game_element game):(game_elements games)

      game_element :: String -> UI Element
      game_element filename = UI.li #. "d-flex justify-content-between align-items-center list-group-item" #+ ((game_name (remove_affixes filename)):(game_buttons filename))

      game_buttons :: String -> [UI Element]
      game_buttons filename = [(load_game_button filename), (delete_game_button filename)]

      load_game_button :: String -> UI Element
      load_game_button filename = do
        button <- UI.button #. "btn btn-primary load_button" # set UI.text "Jugar!"
        on UI.click button $ const $ do
          main_div_element <- getElementById w "main_div"
          delete (from_just main_div_element)
          (render_play_game (return (remove_affixes filename)) (liftIO (open_file filename)) setup) w
        return button

      delete_game_button :: String -> UI Element
      delete_game_button filename = do
        button <- UI.button #. "btn btn-danger delete_button" # set UI.text "Borrar!"
        on UI.click button $ const $ do
          liftIO (delete_file filename)
          main_div_element <- getElementById w "main_div"
          delete (from_just main_div_element)
          (render_load_game setup) w
        return button

    getBody w #+ main_div ([greet "Cargar juego"] ++ (game_list games) ++ button_container main_menu_button)

    redirect_to_button "main_menu" setup w

  game_name :: String -> UI Element
  game_name title = UI.span # set UI.text title