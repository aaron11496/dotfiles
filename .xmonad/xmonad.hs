import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import Data.Bits
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import Data.Monoid

rb_pause = "rhythmbox-client --no-present --no-start --play-pause"
rb_next  = "rhythmbox-client --no-present --no-start --next"
rb_prev = "rhythmbox-client --no-present --no-start --previous"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_z), spawn "gnome-session-save --gui --logout-dialog")
    , ((modMask, xK_x), spawn rb_pause)
    ]

myWorkspaces = [ "1-base", "2-mail", "3-chat", "4-term", "5-code" , "6-music"
               , "7", "8", "9" ]

-- launch certain programs only on certain workspaces
myManageHook = composeAll [ className =? "Pidgin"    --> doF (W.shift "3-chat")
                          , className =? "Skype"     --> doF (W.shift "3-chat")
                          , className =? "Rhythmbox" --> doF (W.shift "6-music")
                          ]

myConfig = defaultConfig
           { terminal    = "urxvt"
           , modMask     = mod4Mask -- set the mod key to the windows key
           , workspaces  = myWorkspaces
           --, keys        = myKeys
           , layoutHook  = avoidStruts $ smartBorders $ layoutHook defaultConfig
           , manageHook  = myManageHook <+> manageHook defaultConfig
           }

myPP = xmobarPP { ppCurrent = xmobarColor "#00A000" ""
                , ppVisible = xmobarColor "#C0C0C0" ""
                , ppHidden = xmobarColor "#909090" ""
                , ppHiddenNoWindows = xmobarColor "#606060" ""
                , ppUrgent = xmobarColor "orange" ""
                , ppSep = "  "
                , ppWsSep = " "
                , ppTitle = xmobarColor "#00A000" ""
                }

-- toggle xmobar on and off with mod-b
toggleStatusbarKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< statusBar "xmobar" myPP toggleStatusbarKey myConfig
