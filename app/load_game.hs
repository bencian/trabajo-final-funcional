module LoadGame (render_load_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  render_load_game :: Window -> UI ()
  render_load_game w = do
    return w # set title "Cargar Juego"