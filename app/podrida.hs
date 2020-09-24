import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


-- | Main entry point.
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    -- active elements
    return w # set title "Podrida"
    return ()