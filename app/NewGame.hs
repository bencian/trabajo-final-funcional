module NewGame (render_new_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO
  import Data.IORef

  import PlayGame
  import Shared
  import SharedBackend

  render_new_game :: (Window -> UI ()) -> Window -> UI ()
  render_new_game setup w = void $ do
    return w # set title "Nuevo Juego"
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "new_game.css"
    
    inputs <- liftIO $ newIORef []

    add_player_button <- add_player_btn
    remove_player_button <- remove_player_btn

    getBody w #+ main_div (game_name_form ++ add_game_players_form add_player_button remove_player_button ++ add_button_container)
    
    game_name_element <- getElementById w "game_title"
    form_div_element <- getElementById w "form_div"

    let
      player_list :: UI [String]
      player_list = mapM (get value) =<< liftIO (readIORef inputs)

      game_title = get value (from_just game_name_element)

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
    redirect_to_button "play_game" (render_play_game game_title (create_new_game player_list) setup) w 

    on UI.click add_player_button $ \_ -> addInput >> redo_layout
    on UI.click remove_player_button $ \_ -> removeInput >> redo_layout

  game_name_form :: [UI Element]
  game_name_form = [UI.h3 # set UI.text "Nombre del juego " # set UI.id_ "game_name", UI.input # set UI.id_ "game_title"]

  game_players_form :: [Element] -> [UI Element]
  game_players_form buttons =
      [ UI.h3 # set UI.text "Nombres de los jugadores" # set UI.id_ "players_title", add_remove_btns_div buttons, UI.div # set UI.id_ "form_div" ]

  add_remove_btns_div :: [Element] -> UI Element
  add_remove_btns_div buttons = UI.div # set UI.id_ "add_and_remove_buttons" #+ map element buttons
  
  add_player_btn :: UI Element
  add_player_btn = UI.button # set UI.text "Agregar" # set UI.class_ "btn btn-success" # set UI.id_ "add_player"

  remove_player_btn :: UI Element
  remove_player_btn = UI.button # set UI.text "Eliminar" # set UI.class_ "btn btn-danger" # set UI.id_ ("remove_player")

  add_button_container :: [UI Element]
  add_button_container = button_container (main_menu_button ++ play_game_button)

  add_game_players_form :: Element -> Element -> [UI Element]
  add_game_players_form add_player_button remove_player_button = game_players_form [add_player_button, remove_player_button]
