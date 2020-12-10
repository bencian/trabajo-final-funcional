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
      validate :: String -> [String] -> Bool
      validate game_title players = (game_title /= "") && (no_empty_inputs players) && (no_repeated_names players)
      
      player_list :: UI [String]
      player_list = mapM (get value) =<< liftIO (readIORef inputs)

      game_title = get value (from_just game_name_element)

      add_input :: UI ()
      add_input = do
        add_element <- UI.input
        liftIO $ modifyIORef inputs ( ++ [add_element])

      remove_input :: UI ()
      remove_input = liftIO $ modifyIORef inputs init

      redo_layout :: UI ()
      redo_layout = void $ do
        elements <- liftIO (readIORef inputs)
        (element (from_just form_div_element)) # set children elements

    add_input
    redo_layout
    redirect_to_button "main_menu" setup w
    
    screen_button <- getElementById w "play_game"
    main_div_element <- getElementById w "main_div"

    on UI.click (from_just screen_button) $ const $ do
      players <- player_list
      title <- game_title
      if validate title players
      then do 
        delete (from_just main_div_element)
        (render_play_game game_title (create_new_game player_list) setup) w
      else do
        (element (from_just main_div_element)) #+ [UI.p #. "text-danger" # set UI.text "Hay nombres repetidos o inputs en blanco"]
        redo_layout
    -- redirect_to_button "play_game" (render_play_game game_title hardcoded_game setup) w

    on UI.click add_player_button $ \_ -> add_input >> redo_layout
    on UI.click remove_player_button $ \_ -> remove_input >> redo_layout

  game_name_form :: [UI Element]
  game_name_form = [greet "Nombre del juego", UI.input # set UI.id_ "game_title"]

  game_players_form :: [Element] -> [UI Element]
  game_players_form buttons =
      [ UI.h3 # set UI.text "Nombres de los jugadores" # set UI.id_ "players_title", add_remove_btns_div buttons, UI.div # set UI.id_ "form_div" ]

  add_remove_btns_div :: [Element] -> UI Element
  add_remove_btns_div buttons = UI.div # set UI.id_ "add_and_remove_buttons" #+ map element buttons
  
  add_player_btn :: UI Element
  add_player_btn = UI.button # set UI.text "Agregar" #. "btn btn-success" # set UI.id_ "add_player"

  remove_player_btn :: UI Element
  remove_player_btn = UI.button # set UI.text "Eliminar" #. "btn btn-danger" # set UI.id_ ("remove_player")

  add_button_container :: [UI Element]
  add_button_container = button_container (main_menu_button ++ play_game_button)

  add_game_players_form :: Element -> Element -> [UI Element]
  add_game_players_form add_player_button remove_player_button = game_players_form [add_player_button, remove_player_button]

  -- hardcoded_game = do
  --   return [[("a",0,1),("b",0,2),("c",0,3)],[("a",1,0),("b",2,0),("c",3,0)],[("a",1,1),("b",2,2),("c",3,3)]]
