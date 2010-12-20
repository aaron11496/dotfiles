import XMonad
import XMonad.Util.EZConfig
--import XMonad.Util.EZConfig
--import XMonad.Config.Gnome

main = xmonad $ defaultConfig 
        { terminal = "terminator"
        , modMask = mod4Mask -- set the mod key to the windows key
        }
        `additionalKeysP` 
                 [ ("M-m", spawn "echo")
                 , ("M-<Backspace>", withFocused hide) -- N.B. this is an absurd thing to do
		         , ("M-S-q", spawn "gnome-session-save --gui --logout-dialog")
                 ]
