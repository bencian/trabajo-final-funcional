module Statistics (render_statistics) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO
  
  render_statistics :: Window -> UI ()
  render_statistics w = do
    return w # set title "Estadisticas"