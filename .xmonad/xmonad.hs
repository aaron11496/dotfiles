import System.IO
import qualified Data.Map as M
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W


-- workspaces names
myWorkspaces =
    [ "1-base", "2-mail", "3-chat", "4-term", "5-code" , "6-music"
    , "7", "8", "9" ]

-- keyboard shortcuts
rb_pause = "rhythmbox-client --no-present --play-pause"
rb_next  = "rhythmbox-client --no-present --next"
rb_prev  = "rhythmbox-client --no-present --previous"

myKeys x =
    [ ((modMask x, xK_x), spawn rb_pause)
    , ((modMask x, xK_z), spawn rb_prev)
    , ((modMask x, xK_c), spawn rb_next)
    ]

myKeyMap x = M.union (keys defaultConfig x) (M.fromList (myKeys x))

--
myLayoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig

-- launch certain programs only on certain workspaces
myManageHook =
    composeAll
    [ className =? "Pidgin"    --> doF (W.shift "3-chat")
    , className =? "Skype"     --> doF (W.shift "3-chat")
    , className =? "Rhythmbox" --> doF (W.shift "6-music")
    ]

-- logging for xmobar to use
myLogHook h = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn h }

-- xmobar styling
myPP =
    xmobarPP
    { ppCurrent = xmobarColor "#00B000" ""
    , ppVisible = xmobarColor "#B0B000" ""
    , ppHidden = xmobarColor "#B0B0B0" ""
    , ppHiddenNoWindows = xmobarColor "#606060" ""
    , ppUrgent = xmobarColor "orange" ""
    , ppSep = "   "
    , ppWsSep = " "
      -- This centers the window title, but fails when there's no window
    , ppTitle = wrap "}" "{" . xmobarColor "#00A000" ""
    }

-- set mod to windows-key (default is left-alt)
myModMask = mod4Mask


main = do
  xmproc <- spawnPipe "xmobar"  -- start xmobar
  xmonad $ defaultConfig
             { manageHook = myManageHook <+> manageHook defaultConfig
             , layoutHook = myLayoutHook
	     --, borderWidth = myBorderWidth
	     --, normalBorderColor = myNormalBorderColor
	     --, focusedBorderColor = myFocusedBorderColor
	     , keys = myKeyMap
	     , logHook = myLogHook xmproc
             , modMask = myModMask
             , terminal = "urxvt"
	     , workspaces = myWorkspaces
             --, focusFollowsMouse = False
	     }
