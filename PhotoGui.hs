module PhotoGui where

import Graphics.UI.Gtk

main = do
  initGUI       -- is start
  window <- windowNew
  set window  [ windowDefaultWidth := 500
              , windowDefaultHeight := 200
              -- , containerChild := treeview
              ]
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
  return ()