import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop

myModMask = mod4Mask -- set mod key to windows key
---myModMask = mod1Mask -- set mod key to alt key
myTerminal = "urxvt"
myFocusedBorderColor = "firebrick"
myNormalBorderColor = "dim gray"

myLayoutHook = avoidStruts
               $ smartBorders
               $ (Tall 1 (3/100) (1/2) ||| Full)

myManageHook = composeAll
    [ manageHook gnomeConfig
    , isFullscreen --> doFullFloat -- make full-screen windows work
    , className =? "Xfce4-notifyd" --> doIgnore
      -- launch certain programs only on certain workspaces
    , className =? "Skype" --> doF (W.shift "9")
    , className =? "SkypeTab" --> doF (W.shift "9")

    ]

main = xmonad $ defaultConfig
       { manageHook = myManageHook
       , handleEventHook = fullscreenEventHook
       , modMask = myModMask
       , layoutHook = myLayoutHook
       , terminal = myTerminal
       , focusedBorderColor = myFocusedBorderColor
       , normalBorderColor = myNormalBorderColor
       }
       `additionalKeys`  -- see /usr/include/X11/keysymdef.h
       [
         ((myModMask, xK_grave), toggleWS)
       ]
