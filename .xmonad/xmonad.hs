import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W


myTerminal = "urxvt"
myFocusedBorderColor = "#B00000"
myNormalBorderColor  = "#202020"

myModMask = mod4Mask -- set mod key to windows key
--myModMask = mod1Mask -- set mod key to alt key

myLayoutHook = avoidStruts
               $ smartBorders
               $ onWorkspace "9" pidginLayout
               $ (Tall 1 (3/100) (1/2) ||| Full)
    where
      pidginLayout = reflectHoriz
                     $ withIM (0.15) (Role "buddy_list") Grid

myManageHook = composeAll
    [ manageHook gnomeConfig
    , isFullscreen --> doFullFloat -- make full-screen windows work
      -- launch certain programs only on certain workspaces
    , className =? "Pidgin"    --> doF (W.shift "9")
    , className =? "Skype"     --> doF (W.shift "9")
    ]

main = xmonad $ defaultConfig
       { manageHook = myManageHook
       , modMask = myModMask
       , layoutHook = myLayoutHook
       , terminal = myTerminal
       , focusedBorderColor = myFocusedBorderColor
       , normalBorderColor = myNormalBorderColor
       }
       `additionalKeys`  -- see /usr/include/X11/keysymdef.h
       [ ((myModMask, xK_0), sendMessage ToggleStruts)
       , ((0, xK_Print), spawn "gnome-screenshot")
       , ((myModMask, xK_grave), toggleWS)
       , ((myModMask, xK_Print), spawn "gnome-screenshot -w")
       , ((myModMask .|. shiftMask, xK_Print), spawn "gnome-screenshot -a")
       ]
