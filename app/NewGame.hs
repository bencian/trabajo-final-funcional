module NewGame (render_new_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO
  import Data.IORef

  import PlayGame
  import Shared

  render_new_game :: (Window -> UI ()) -> Window -> UI ()
  render_new_game setup w = void $ do
    return w # set title "Nuevo Juego"
    UI.addStyleSheet w "podrida.css"
    UI.addStyleSheet w "bootstrap.css"
    
    inputs <- liftIO $ newIORef []
    add_game_button_element <- add_player_button
    remove_game_button_element <- remove_player_button
    getBody w #+ main_div (button_container (main_menu_button ++ play_game_button) ++ game_name_div ++ (new_game_form (map element [add_game_button_element, remove_game_button_element])))
    
    game_name_element <- getElementById w "title"
    form_div_element <- getElementById w "form_div"

    let
      player_list :: UI [String]
      player_list = mapM (get value) =<< liftIO (readIORef inputs)

      game_title =  get value (from_just game_name_element)

      addInput :: UI ()
      addInput = do
        add_element <- UI.input
        liftIO $ modifyIORef inputs ( ++ [add_element])

      removeInput :: UI ()
      removeInput = liftIO $ modifyIORef inputs init

      redo_layout :: UI ()
      redo_layout = void $ do
        elements <- liftIO (readIORef inputs)
        (element (from_just form_div_element)) # set children elements
        
    
    addInput
    redo_layout
    redirect_to_button "main_menu" setup w
    redirect_to_button "play_game" (render_play_game game_title player_list setup) w 

    on UI.click add_game_button_element $ \_ -> addInput >> redo_layout
    on UI.click remove_game_button_element $ \_ -> removeInput >> redo_layout

  new_game_form :: [UI Element] -> [UI Element]
  new_game_form xs = 
      [ UI.h4 # set UI.text "Nombres de los jugadores", UI.div # set UI.id_ "form_div" ] ++ xs

  game_name_div :: [UI Element]
  game_name_div = [UI.h4 # set UI.text "Nombre del juego: ", UI.input # set UI.id_ "title"]
  
  add_player_button :: UI Element
  add_player_button = UI.button # set UI.text "Agregar" # set UI.class_ "btn_init_page btn" # set UI.id_ "add_player"

  remove_player_button :: UI Element
  remove_player_button = UI.button # set UI.text "Eliminar" # set UI.class_ "btn_init_page btn" # set UI.id_ ("remove_player")

  -- start_game :: Game
  -- start_game w = do
  --   element <- getElementById w "player_name"
  --   player_name <- get value (from_just element)
  --   return player_name
    
