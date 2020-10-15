module LoadGame (render_load_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  import Shared

  render_load_game :: Window -> UI ()
  render_load_game w = void $ do
    return w # set title "Otro Juego"
    prueba_p <- UI.p # set UI.text "Prueba otro" # set UI.id_ "Prueba_load_game"
    getBody w #+ [element prueba_p]
  
  
  