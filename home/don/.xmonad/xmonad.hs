import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.UpdatePointer
import XMonad.Actions.GridSelect
import XMonad.Hooks.SetWMName
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import Graphics.X11.ExtraTypes.XF86
import System.IO
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 3

myModMask = mod4Mask

myWorkspaces = ["1:TERM","2:WEB","3:CODE","4:PDF","5:MUSIC","6:REMOTE","7:ETC"] ++ map show [8..9] ++ ["NSP"] 

myNormalBorderColor = "#242424"
myFocusedBorderColor = "#EA3C53"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|.    shiftMask, xK_Return    ), spawn $ XMonad.terminal conf)
    , ((modm,       xK_p                    ), spawn "rofi -show run")
    , ((modm .|.    shiftMask, xK_c         ), kill)
    , ((modm,       xK_space                ), sendMessage NextLayout)
    , ((modm .|.    shiftMask, xK_space     ), setLayout $ XMonad.layoutHook conf)
    , ((modm,       xK_n                    ), refresh)
    , ((modm,       xK_Tab                  ), windows W.focusDown)
    , ((modm,       xK_j                    ), windows W.focusDown)
    , ((modm,       xK_k                    ), windows W.focusUp)
    , ((modm,       xK_m                    ), withFocused minimizeWindow)
    , ((modm .|.    shiftMask, xK_m         ), sendMessage RestoreNextMinimizedWin)
    , ((modm,       xK_f                    ), (sendMessage $ Toggle FULL))
    , ((modm,       xK_Return               ), windows W.swapMaster)
    , ((modm .|.    shiftMask, xK_j         ), windows W.swapDown )
    , ((modm .|.    shiftMask, xK_k         ), windows W.swapUp )
    , ((modm,       xK_h                    ), sendMessage Shrink)
    , ((modm,       xK_l                    ), sendMessage Expand)
    , ((modm,       xK_t                    ), withFocused $ windows . W.sink)
    , ((modm,       xK_comma                ), sendMessage (IncMasterN 1))
    , ((modm,       xK_period               ), sendMessage (IncMasterN (-1)))
    , ((modm,       xK_Right                ), moveTo Next (WSIs hiddenNotNSP))
    , ((modm,       xK_Left                 ), moveTo Prev (WSIs hiddenNotNSP))
    , ((modm .|.    shiftMask, xK_Right     ), shiftTo Next (WSIs hiddenNotNSP))
    , ((modm .|.    shiftMask, xK_Left      ), shiftTo Prev (WSIs hiddenNotNSP))
    , ((modm .|.    shiftMask, xK_q         ), io (exitWith ExitSuccess))
    , ((modm,       xK_q                    ), spawn "xmonad --recompile; xmonad --restart")
    ]

    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

hiddenNotNSP :: X (WindowSpace -> Bool)
hiddenNotNSP = do
    hs <- gets $ map W.tag . W.hidden . windowset
    return (\w -> (W.tag w) /= "NSP" && (W.tag w) `elem` hs)

addKeys = [ ("<XF86AudioLowerVolume>" ,spawn "amixer -D pulse sset Master 5%-")
          , ("<XF86AudioRaiseVolume>" ,spawn "amixer -D pulse sset Master 5%+" )
          , ("<XF86AudioMute>" ,spawn "amixer -D pulse sset Master mute" )
          ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster ))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster ))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster ))
    ]

myLayout = smartBorders $ avoidStruts $ minimize (mkToggle (NOBORDERS ?? FULL ?? EOT) (tiled ||| Mirror tiled ||| Full))
    where
        tiled   = gaps [(U,5), (R,5), (L,5), (R,5)] $ spacing 5 $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

myManageHook = composeAll
    [ resource =? "desktop_window" --> doIgnore ]
    <+> (isFullscreen --> doFullFloat)
    <+> manageDocks

myEventHook = mempty

myLogHook xmproc = dynamicLogWithPP xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppCurrent = xmobarColor "#67C8FF" "" . wrap "[" "]"
                       , ppTitle = xmobarColor "#88FF88" "" . shorten 100
                       }
                       >> updatePointer (0.75, 0.75) (0.75, 0.75)
                       >> (fadeOutLogHook $ fadeIf ((className =? "URxvt")) 0.93)

--

myConfig xmproc = (defaults xmproc) `additionalKeysP` addKeys

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/.xmobarrc"
    xmonad $ ewmh $ myConfig xmproc

defaults xmproc = def {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook xmproc
}
