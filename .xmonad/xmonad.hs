import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import qualified XMonad.StackSet as W


myWorkspaces =
    [ "file", "mail", "chat", "term", "code" , "work", "play", "song", "etc." ]

myTerminal = "urxvt"

myModMask = mod4Mask -- set mod key to windows key

myLayoutHook = avoidStruts
               $ smartBorders
               -- $ onWorkspace "draw" gimpLayout
               $ onWorkspace "chat" pidginLayout
               $ (Tall 1 (3/100) (1/2) ||| Full)
    where
      pidginLayout = reflectHoriz
                     $ withIM (0.15) (Role "buddy_list") Grid

myManageHook =
    composeAll
    [ manageHook gnomeConfig
    , isFullscreen --> doFullFloat -- make full-screen windows work
      -- launch certain programs only on certain workspaces
    , className =? "Pidgin"    --> doF (W.shift "chat")
    , className =? "Skype"     --> doF (W.shift "chat")
    , className =? "Rhythmbox" --> doF (W.shift "song")
    ]

main = xmonad $ gnomeConfig
       { manageHook = myManageHook
--       , modMask = myModMask
       , layoutHook = myLayoutHook
       , terminal   = myTerminal
       , workspaces = myWorkspaces
       }
