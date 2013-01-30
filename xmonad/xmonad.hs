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
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "xfce4-terminal"

myWorkSpaces = ["1:web", "2:file", "3:coding", "4:console", "5:media",
                "6", "7", "8:chat", "9:mail" ]

myLayout = showWName $ avoidStruts $ smartBorders $ perWS
    where
        perWS = onWorkspace "1:web"     tallFirst  $
                onWorkspace "2:file"    myFile     $
                onWorkspace "3:coding"  myCoding   $
                onWorkspace "4:console" myConsole  $
                onWorkspace "5:media"   fullFirst  $
                onWorkspace "8:chat"    myChat     $
                onWorkspace "9:mail"    myMail     $
                                        tallFirst
        tallFirst = myTall ||| Mirror myTall ||| Full
        fullFirst = Full ||| myTall ||| Mirror myTall
        myTall    = Tall 1 0.03 0.7
        myCoding  = FixedColumn 1 20 84 10
        myConsole = Grid ||| myTall ||| Mirror myTall
        myFile    = TwoPane (3/100) (1/2)
        myMail    = Tall 1 0.03 0.5
        myChat    = reflectHoriz $ withIM (18/100) (Role "buddy_list") Grid

myLayoutHook = dwmStyle shrinkText defaultTheme myLayout

myLogHook = fadeInactiveLogHook 0.93

myManageHook = composeAll
    [ className =? "Firefox"         --> shiftView "1:web"
    , className =? "Vlc"             --> shiftView "5:media"
    , className =? "Clementine"      --> shiftView "5:media"
    --, className =? "Evince"          --> shiftView "5:media"
    , className =? "Pidgin"          --> shiftView "8:chat"
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
    , title     =? "Panda"           --> doFloat
    , isFullscreen                   --> doFullFloat
    , isDialog                       --> doCenterFloat
    ]
    <+> manageHook desktopConfig <+> manageSpawn
    where
        shiftView = doF . liftM2 (.) W.greedyView W.shift

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
    , logHook = myLogHook >> ewmhDesktopsLogHook >> takeTopFocus
    , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
    , handleEventHook = ewmhDesktopsEventHook
    }
