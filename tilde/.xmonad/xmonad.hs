import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Gnome
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W


myModMask = mod4Mask -- set mod key to windows key
---myModMask = mod1Mask -- set mod key to alt key


myLayoutHook = avoidStruts
               $ smartBorders
               $ (Tall 1 (4/100) (1/2) ||| Full)

myManageHook = composeAll
    [ manageHook gnomeConfig
    , isFullscreen --> doFullFloat -- make full-screen windows work
    , className =? "HipChat" --> doF (W.shift "9")
    ]


main = xmonad $ ewmh defaultConfig
       { manageHook = myManageHook
       , handleEventHook = fullscreenEventHook
       , modMask = myModMask
       , layoutHook = myLayoutHook
       , terminal = "urxvt"
       , focusedBorderColor = "firebrick"
       , normalBorderColor = "dim gray"
       }
       `additionalKeys`  -- see /usr/include/X11/keysymdef.h
       [
         ((myModMask, xK_grave), toggleWS)
       ]
