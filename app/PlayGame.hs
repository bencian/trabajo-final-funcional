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

      redo_layout :: Game -> UI ()
      redo_layout game = void $ do
        board_element <- board #+ (table_elements game)
        element (from_just board_div_element) # set children [board_element]
        

    redo_layout game

    redirect_to_button "main_menu" setup w
  
    on UI.click (from_just confirm_predicted_rounds) $ const $ do
      values_list <- input_list
      -- if else validation
      (element (from_just confirm_predicted_rounds)) #. "hide btn btn-sm"
      (element (from_just confirm_won_rounds)) #. "btn btn-sm"
      redo_layout (insert_predicted game values_list)

    -- on UI.click (from_just confirm_won_rounds) $ const $ do
    --   values_list <- input_list
    --   (element (from_just confirm_predicted_rounds)) #. "btn btn-sm"
    --   (element (from_just confirm_won_rounds)) #. "hide btn btn-sm"
    --   redo_layout (insert_winnings (game values_list))

  greet :: String -> UI Element
  greet name =
    UI.h2 #+ [string name ] #. "text-center text-light"

  action_buttons :: [UI Element]
  action_buttons = [confirm_predicted_button, confirm_won_button]

  confirm_predicted_button =
    UI.button # set UI.text "Predicciones" #. "btn btn-sm" # set UI.id_ "confirm_predicted_rounds"

  confirm_won_button =
    UI.button # set UI.text "Ganadas" #. "hide btn btn-sm" # set UI.id_ "confirm_won_rounds"

  board_div :: UI Element
  board_div =  UI.div # set UI.id_ "board_div"
  
  board :: UI Element
  board = UI.table # set UI.id_ "board_table" #. "table table-bordered table-dark"

  board_header :: Game -> [UI Element]
  board_header game =
    let round_header = UI.td # set UI.id_ "round_header" #+ [string "Número de cartas"]
    in [UI.tr # set UI.id_ "board_headers" #+ (round_header:players_td_list (snd game))]

  players_td_list :: [String] -> [UI Element]
  players_td_list players = map (\name -> UI.td #+ [string name]) players

  create_inputs :: Game -> [UI Element]
  create_inputs game = 
    [UI.tr # set UI.id_ "game_inputs" #+ ((UI.td # set UI.id_ "board_headers" # set UI.text (show (number_of_cards_round (fst game)))):(create_inputs_row (snd game)))]
    
  create_inputs_row :: [String] -> [UI Element]
  create_inputs_row = map (\(x:xs) -> UI.td #+ [UI.input # set UI.id_ (show x)])

  table_elements :: Game -> [UI Element]
  table_elements game = (board_header game) ++ (create_table_rows (reverse (fst game))) ++ (create_inputs game)

  create_table_rows :: [Round] -> [UI Element]
  create_table_rows [] = []
  create_table_rows (round:game) = (create_table_rows game) ++ [UI.tr #+ ([ UI.td # set UI.text (show (number_of_cards_round game)) ] ++ (create_columns round))]

  create_columns :: Round -> [UI Element]
  create_columns [] = []
  create_columns ((x,y,z):round) = (UI.td #+ [ UI.p # set UI.text (show y), UI.p # set UI.text (show z)]):(create_columns round)

  -- insert_winnings :: Game -> [String] -> Game
  
  insert_predicted :: Game -> [String] -> Game
  insert_predicted (rounds, players) values = ((create_round players values):rounds, players)

