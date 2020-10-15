module NewGame (render_new_game) where
  import qualified Graphics.UI.Threepenny as UI
  import Graphics.UI.Threepenny.Core

  import Control.Monad

  import System.FilePath
  import System.IO

  render_new_game :: Window -> UI ()
  render_new_game w = void $ do
    return w # set title "Nuevo Juego"
    prueba_p <- UI.p # set UI.text "Prueba otro" # set UI.id_ "Prueba_new_game"
    getBody w #+ [element prueba_p]
