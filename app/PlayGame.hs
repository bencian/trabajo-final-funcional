module PlayGame (render_play_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO
  import Data.IORef

  import Shared
  import SharedBackend

  render_play_game :: UI String -> UI Game -> (Window -> UI ()) -> Window -> UI ()
  render_play_game game_title ui_game setup w = void $ do
    return w # set title "Jugando"
    UI.addStyleSheet w "bootstrap.css"
    UI.addStyleSheet w "play_game.css"

    title <- game_title
    game <- ui_game
    mutable_game <- liftIO $ newIORef game
    
    getBody w #+ main_div ([greet title, board_div] ++ action_buttons ++ button_container main_menu_button)
    
    confirm_predicted_rounds <- getElementById w "confirm_predicted_rounds"
    confirm_won_rounds <- getElementById w "confirm_won_rounds"
    board_div_element <- getElementById w "board_div"

    let
      input_list :: UI [String]
      input_list = mapM (get value) =<< getElementsByTagName w "input"

      redo_layout :: IORef Game -> String -> String -> UI ()
      redo_layout ioref_game current_action_string error_string = void $ do
        game <- liftIO (readIORef ioref_game)
        board_element <- board #+ (table_elements game current_action_string)
        element (from_just board_div_element) # set children [board_element]
        (element (from_just board_div_element)) #+ [UI.p #. "text-danger" # set UI.text error_string]

      correct_inputs :: Game -> Maybe Element -> Maybe Element -> String -> UI ()
      correct_inputs modified_game show_button hide_button action_text = do
        (element (from_just show_button)) #. "btn btn-sm"
        (element (from_just hide_button)) #. "hide btn btn-sm"
        liftIO $ writeIORef mutable_game (modified_game)
        redo_layout mutable_game action_text ""

      incorrect_inputs :: String -> UI ()
      incorrect_inputs message = do
        redo_layout mutable_game message error_message
        

    redo_layout mutable_game "Ingresar predicciones para 1" ""

    redirect_to_button "main_menu" setup w
  
    on UI.click (from_just confirm_predicted_rounds) $ const $ do
      values_list <- input_list
      game_value <- liftIO $ readIORef mutable_game
      if' (valid_card_inputs values_list (number_of_cards_round (fst game_value)) (/=)) (correct_inputs (insert_predicted game_value values_list) confirm_won_rounds confirm_predicted_rounds (won_message (number_of_cards_round (fst game_value)))) (incorrect_inputs (prediction_message (number_of_cards_round (fst game_value))))
      
    on UI.click (from_just confirm_won_rounds) $ const $ do
      values_list <- input_list
      game_value <- liftIO $ readIORef mutable_game
      -- add else if in the correct branch to end the game
      if' (valid_card_inputs values_list (number_of_cards_round (tail (fst game_value))) (==)) (correct_inputs (insert_winnings game_value values_list) confirm_predicted_rounds confirm_won_rounds (prediction_message (number_of_cards_round (fst game_value)))) (incorrect_inputs (won_message (number_of_cards_round (tail (fst game_value)))))

  greet :: String -> UI Element
  greet name =
    UI.h2 #+ [string name ] #. "text-center text-light"

  action_buttons :: [UI Element]
  action_buttons = [confirm_predicted_button, confirm_won_button]

  confirm_predicted_button =
    UI.button # set UI.text "Confirmar predicciones" #. "btn btn-sm" # set UI.id_ "confirm_predicted_rounds"

  confirm_won_button =
    UI.button # set UI.text "Confirmar ganadas" #. "hide btn btn-sm" # set UI.id_ "confirm_won_rounds"

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

  create_inputs :: Game -> String -> [UI Element]
  create_inputs game string = 
    [UI.tr # set UI.id_ "game_inputs" #+ ((UI.td # set UI.id_ "input_header" # set UI.text string):(create_inputs_row (snd game)))]
    
  create_inputs_row :: [String] -> [UI Element]
  create_inputs_row = map (\(x:xs) -> UI.td #+ [UI.input # set UI.id_ (show x)])

  table_elements :: Game -> String -> [UI Element]
  table_elements game string = (board_header game) ++ (create_table_rows (reverse (fst game))) ++ (create_scores game) ++ (create_inputs game string)

  create_scores :: Game -> [UI Element]
  create_scores game = [ UI.tr # set UI.id_ "game_scores" #+ ((UI.td # set UI.id_ "score_headers" # set UI.text "Puntaje: "):(create_scores_row game)) ]

  create_scores_row :: Game -> [UI Element]
  create_scores_row game = map (partial_score_td (fst game)) (snd game)

  partial_score_td :: [Round] -> String -> UI Element
  partial_score_td game name = UI.td #+ [UI.p # set UI.text (show (get_player_score game name))]

  create_table_rows :: [Round] -> [UI Element]
  create_table_rows [] = []
  create_table_rows (round:game) = (create_table_rows game) ++ [UI.tr #+ ([ UI.td # set UI.text (show (number_of_cards_round game)) ] ++ (create_columns round))]

  create_columns :: Round -> [UI Element]
  create_columns [] = []
  create_columns ((x,y,z):round) = (UI.td #+ [ UI.p # set UI.text ("Predictas: "++(show y)), UI.p # set UI.text ("Ganadas: "++(show z))]):(create_columns round)

  insert_winnings :: Game -> [String] -> Game
  insert_winnings (rounds, players) values = ((reverse (((insert_values_into_round (head (reverse rounds)) values):(tail (reverse rounds))))), players)
  
  insert_predicted :: Game -> [String] -> Game
  insert_predicted (rounds, players) values = (rounds ++ [create_round players values], players)

  prediction_message :: Int -> String
  prediction_message cards = "Ingresar predicciones para " ++ show cards

  won_message :: Int -> String
  won_message cards = "Ingresar ganadas para " ++ show cards
