module Shared where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  type Game = [Round]
  type Round = [Player]
  type Player = (String,Int,Int)

  main_menu_button :: [UI Element]
  main_menu_button =
      [ UI.button # set UI.text "Volver" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "main_menu" ]

  play_game_button :: [UI Element]
  play_game_button =
      [ UI.button # set UI.text "Jugar" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "play_game" ]

  button_container :: [UI Element] -> [UI Element]
  button_container make_buttons =
    [ UI.div # set UI.class_ "container-fluid" #+ (button_row make_buttons) ]

  button_row :: [UI Element] -> [UI Element]
  button_row make_buttons =
    [ UI.div # set UI.class_ "row text-center" #+ (make_buttons ++ [UI.br, UI.br]) ]

  main_div :: [UI Element] -> [UI Element]
  main_div elements = 
    [ UI.div # set UI.id_ "main_div" #+ elements ]
  
  from_just :: Maybe a -> a
  from_just (Just a) = a
  from_just Nothing = error "Oops, you goofed up, fool."

  redirect_to_button text screen_function w = do
    screen_button <- getElementById w text
    main_div_element <- getElementById w "main_div"

    on UI.click (from_just screen_button) $ const $ do
      delete (from_just main_div_element)
      screen_function w