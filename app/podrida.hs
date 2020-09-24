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
    
    getBody w #+
        [UI.div #. "wrap" #+ greet]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Bienvenido al anotador piola!"]
    , UI.div #+ [string "Para jugar, ingrese 1."]
    ]

getStaticDir :: IO FilePath
getStaticDir = return "static"