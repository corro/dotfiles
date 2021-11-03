import XMonad
import Control.Monad (liftM2)
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.FixedColumn
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Util.Replace
import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "kitty"

myWorkSpaces = ["1:web", "2:file", "3:coding", "4:info", "5:media",
                "6", "7", "8:chat", "9:mail" ]

myLayout = showWName $ avoidStruts $ smartBorders $ perWS
    where
        perWS = onWorkspace "1:web"     tallFirst  $
                onWorkspace "2:file"    myFile     $
                onWorkspace "3:coding"  myCoding   $
                onWorkspace "4:info"    myCoding   $
                onWorkspace "5:media"   fullFirst  $
                onWorkspace "8:chat"    myChat     $
                onWorkspace "9:mail"    myMail     $
                                        tallFirst
        tallFirst = myTall ||| Mirror myTall ||| Full
        fullFirst = Full ||| myTall ||| Mirror myTall
        myTall    = Tall 1 0.03 0.7
        myCoding  = FixedColumn 1 20 84 10 ||| Full
        myConsole = Grid ||| myTall ||| Mirror myTall
        myFile    = TwoPane (3/100) (1/2)
        myMail    = Tall 1 0.03 0.4 ||| Full
        myChat    = reflectHoriz $ withIM (18/100) (Role "buddy_list") Grid ||| Full

myLayoutHook = dwmStyle shrinkText defaultTheme myLayout

myLogHook = fadeInactiveLogHook 0.93

myManageHook = composeAll
    [ className =? "Firefox"         --> shiftView "1:web"
    , className =? "Vlc"             --> shiftView "5:media"
    , className =? "Clementine"      --> shiftView "5:media"
    , className =? "Pidgin"          --> shiftView "8:chat"
    , className =? "Thunderbird"     --> doShift   "9:mail"
    , className =? "Xfrun4"          --> doCenterFloat
    , className =? "Speedcrunch"     --> doCenterFloat
    , className =? "Gimp"            --> doFloat
    , className =? "Xfce4-notifyd"   --> doIgnore
    , className =? "conky-semi"      --> doIgnore
    , className =? "Wrapper"         --> doIgnore
    , className =? "Keepassx"        --> doFloat
    , className =? "VirtualBox"      --> doCenterFloat
    , className =? "Wicd-client.py"  --> doCenterFloat
    , appName   =? "Download"        --> doCenterFloat
    , title     =? "File Operation Progress" --> doCenterFloat
    , title     =? "Panda"           --> doFloat
    , isFullscreen                   --> doFullFloat
    , isDialog                       --> doCenterFloat
    ]
    <+> manageHook desktopConfig <+> manageSpawn
    where
        shiftView = doF . liftM2 (.) W.greedyView W.shift

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
         [((modm .|. mask, key), f sc)
             | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
             , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

main = do
    replace
    xmonad desktopConfig {
      terminal = myTerminal
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ff0000"
    , borderWidth = 2
    , workspaces  = myWorkSpaces
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , keys = myKeys <+> keys defaultConfig
    , logHook = myLogHook >> ewmhDesktopsLogHook >> takeTopFocus
    , handleEventHook = ewmhDesktopsEventHook >> fullscreenEventHook
    }
