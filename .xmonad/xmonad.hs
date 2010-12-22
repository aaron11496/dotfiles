import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import Control.Arrow
import Data.Bits
import qualified Data.Map as M
import Data.Monoid

main = xmonad $ defaultConfig
        { terminal      = "terminator"
        , modMask       = mod4Mask -- set the mod key to the windows key
        , workspaces  = ["1","2","3","4","5","6","7","8","9"]
        } `additionalKeysP` myKeys

myKeys =
       [("M-z", spawn "gnome-session-save --gui --logout-dialog")]