import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeWindows as F
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

import XMonad.Config.Desktop
import XMonad.Hooks.SetWMName

import Data.Monoid
import qualified Data.Map as M

import XMonad.Util.Run (safeSpawn)

import System.Environment (getEnvironment)

import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.StackSet as W

myExtraWorkspaces = [(xK_0, "0"), (xK_minus, "-"), (xK_equal, "=")]
myWorkspaces = ["1:dev", "2:home", "3:web", "4:chat", "5:email", "6", "7", "8", "9", "0", "-", "="] ++ (map snd myExtraWorkspaces)

myManageHook = composeAll [
                className =? "Gimp"      --> doFloat,
                className =? "Vncviewer" --> doFloat,
                resource =? "stalonetray" --> doIgnore,
                isFullscreen             --> doFullFloat
               ]

myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, key), (windows $ W.greedyView ws)) | (key,ws) <- myExtraWorkspaces ]
        ++
    [ ((modm, xK_p), spawn "dmenu_run"),
      ((modm, xK_quotedbl), spawn "~/bin/term light"),
      ((modm, xK_l), spawn "xlock -echokeys -echokey '*' -usefirst -delay 10000 -mode galaxy -erasedelay 0") ]


toggleFullFloat = withFocused (\w -> do
                                 { floats <- gets (W.floating . windowset);
                                   if M.member w floats
                                   then withFocused $ windows . W.sink
                                   else withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f 
                                 } )

-- this will be useful once I have a moniter setup
togglevga :: X ()
togglevga = do
  screencount <- LIS.countScreens
  if screencount > 1
   then spawn "xrandr --output VGA1 --off"
   else spawn "xrandr --output HDMI-2 --auto --right-of eDP-1"

myStartupHook :: X ()
myStartupHook = do setWMName "LG3D" 
                   spawn "~/bin/autostart"

main = do xmproc <- spawnPipe "/usr/bin/xmobar /home/dlangevi/.xmobarrc > my.log"
          xmonad $ docks $ defaultConfig 
                  { manageHook  = myManageHook <+> manageHook defaultConfig
                  , terminal = "~/bin/term"
                  , workspaces  = myWorkspaces
                  , layoutHook  = smartBorders $ avoidStruts $ layoutHook defaultConfig 
	          , borderWidth = 2
                  , XMonad.keys        = \c -> myKeys c `M.union` XMonad.keys defaultConfig c
                  , normalBorderColor  = "#646464"
                  , focusedBorderColor = "lightgreen"
                  , logHook     = dynamicLogWithPP $ xmobarPP 
                                                      { ppOutput = hPutStrLn xmproc
                                                      , ppCurrent = xmobarColor "#ABABAB" "" . wrap "[" "]"
                                                      , ppTitle = xmobarColor "darkgreen" "" . shorten 100
                                                      }
		  , startupHook = gnomeRegister >> startupHook desktopConfig 
                  }

-- | Register xmonad with gnome. 'dbus-send' must be in the $PATH with which
-- xmonad is started.
--
-- This action reduces a delay on startup only only if you have configured
-- gnome-session>=2.26: to start xmonad with a command as such:
--
-- > gconftool-2 -s /desktop/gnome/session/required_components/windowmanager xmonad --type string
gnomeRegister :: MonadIO m => m ()
gnomeRegister = io $ do
    x <- Prelude.lookup "DESKTOP_AUTOSTART_ID" `fmap` getEnvironment
    spawn "~/bin/autostart"
    whenJust x $ \sessionId -> safeSpawn "dbus-send"
            ["--session"
            ,"--print-reply=literal"
            ,"--dest=org.gnome.SessionManager"
            ,"/org/gnome/SessionManager"
            ,"org.gnome.SessionManager.RegisterClient"
            ,"string:xmonad"
            ,"string:"++sessionId]


