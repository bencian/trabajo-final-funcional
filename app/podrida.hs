import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Monad

import System.FilePath
import System.IO

-- | Main entry point.
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    -- active elements
    return w # set title "Podrida"
    UI.addStyleSheet w "podrida.css"
    UI.addStyleSheet w "bootstrap.css"

    getBody w #+ greet #+ button_container

greet :: [UI Element]
greet =
    [ UI.h1 #+ [string "Bienvenido al anotador piola!" ] # set UI.class_ "text-center text-light" ]

button_container :: [UI Element]
button_container =
    [ UI.div # set UI.class_ "container-fluid" #+ button_row ]

button_row :: [UI Element]
button_row =
    [ UI.div # set UI.class_ "row text-center" #+ buttons ]

buttons :: [UI Element]
buttons = 
    [ UI.a #+ [string "Cargar juego"] # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "load_game"
    , UI.a #+ [string "Nuevo juego"] # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "new_game"
    , UI.a #+ [string "Estadísticas"] # set UI.class_ "btn_init_page btn col-4" # set UI.id_ "statistics"
    ]

getStaticDir :: IO FilePath
getStaticDir = return "static"