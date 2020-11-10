module PlayGame (render_play_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared
  import SharedBackend

  render_play_game :: UI String -> UI Game -> (Window -> UI ()) -> Window -> UI ()
  render_play_game game_title ui_game setup w = void $ do
    return w # set title "Jugando"
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "play_game.css"

    -- inputs <- liftIO $ newIORef []
    title <- game_title
    game <- ui_game
    getBody w #+ main_div ([greet title, board_div] ++ action_buttons ++ button_container main_menu_button)
    
    confirm_predicted_rounds <- getElementById w "confirm_predicted_rounds"
    confirm_won_rounds <- getElementById w "confirm_won_rounds"
    board_div_element <- getElementById w "board_div"

    let
      input_list :: UI [String]
      input_list = mapM (get value) =<< getElementsByTagName w "input"
      
      -- confirm_prediction :: UI ()
      -- confirm_prediction = do  

      -- confirm_winnings :: UI ()

      redo_layout :: UI ()
      redo_layout = void $ do
        -- (insert_values (game values_list))
        board_element <- board #+ (table_elements game)
        element (from_just board_div_element) # set children [board_element]
        

    redo_layout

    redirect_to_button "main_menu" setup w
  
    on UI.click (from_just confirm_predicted_rounds) $ const $ do
      -- insert_predicted (game values_list)
      redo_layout
    -- on UI.click (from_just confirm_won_rounds) $ \_ -> confirm_winnings >> redo_layout
    --   values_list <- input_list
    --   -- insert_winnings (game values_list)
    --   redo_layout

  greet :: String -> UI Element
  greet name =
    UI.h2 #+ [string name ] # set UI.class_ "text-center text-light"

  action_buttons :: [UI Element]
  action_buttons = [confirm_predicted_button, confirm_won_button]

  confirm_predicted_button =
    UI.button # set UI.text "Predicciones" # set UI.class_ "btn btn-sm" # set UI.id_ "confirm_predicted_rounds"

  confirm_won_button =
    UI.button # set UI.text "Ganadas" # set UI.class_ "btn btn-sm" # set UI.id_ "confirm_won_rounds"

  board_div :: UI Element
  board_div =  UI.div # set UI.id_ "board_div"
  
  board :: UI Element
  board = UI.table # set UI.id_ "board_table" # set UI.class_ "table table-bordered table-dark"

  board_header :: Game -> [UI Element]
  board_header game =
    let round_header = UI.td # set UI.id_ "round_header" #+ [string "Número de cartas"]
    in [UI.tr # set UI.id_ "board_headers" #+ (round_header:players_td_list (get_player_list game))]

  players_td_list :: [String] -> [UI Element]
  players_td_list players = map (\name -> UI.td #+ [string name]) players

  create_inputs :: Game -> [UI Element]
  create_inputs game = 
    [UI.tr # set UI.id_ "game_inputs" #+ ((UI.td # set UI.id_ "board_headers" # set UI.text (show (number_of_cards_round game))):(create_inputs_row (get_player_list game)))]
    
  create_inputs_row :: [String] -> [UI Element]
  create_inputs_row = map (\(x:xs) -> UI.td #+ [UI.input # set UI.id_ (show x)])

  table_elements :: Game -> [UI Element]
  table_elements game = (board_header game) ++ (create_inputs game)