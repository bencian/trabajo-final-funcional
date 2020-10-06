import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad

import System.FilePath
import System.IO

-- :(
-- import Statistics
-- import NewGame
-- import LoadGame

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

        
    body <- getBody w #+ greet #+ button_container

    new_game_button <- getElementById w "new_game"
    load_game_button <- getElementById w "load_game"
    statistics_button <- getElementById w "statistics"

    on UI.click (fromJust new_game_button) $ const $ do
        delete body
        render_new_game w
    on UI.click (fromJust load_game_button) $ const $ do
        delete body
        render_load_game w
    on UI.click (fromJust statistics_button) $ const $ do
        delete body
        render_statistics w

greet :: [UI Element]
greet =
    [ UI.h1 #+ [string "Bienvenido al anotador piola!" ] # set UI.class_ "text-center text-light" ]

button_container :: [UI Element]
button_container =
    [ UI.div # set UI.class_ "container-fluid" #+ button_row ]

button_row :: [UI Element]
button_row =
    [ UI.div # set UI.class_ "row text-center" #+ make_buttons ]

make_buttons :: [UI Element]
make_buttons = 
    [ UI.button # set UI.text "Cargar juego" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "load_game"
    , UI.button # set UI.text "Nuevo juego" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "new_game"
    , UI.button # set UI.text "Estadísticas" # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "statistics"
    ]

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

getStaticDir :: IO FilePath
getStaticDir = return "static"

render_new_game :: Window -> UI ()
render_new_game w = void $ do
    return w # set title "Nuevo Juego"

render_load_game :: Window -> UI ()
render_load_game w = void $ do
    return w # set title "Cargar Juego"

render_statistics :: Window -> UI ()
render_statistics w = void $ do
    return w # set title "Estadisticas"