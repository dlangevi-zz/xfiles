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
import Data.Map as M

import XMonad.Util.Run (safeSpawn)

import System.Environment (getEnvironment)

import qualified XMonad.StackSet as W

myWorkspaces = ["1:dev", "2:home", "3:web", "4:chat", "5:email", "6", "7", "8", "9", "0", "-", "="]

myManageHook = composeAll [
                className =? "Gimp"      --> doFloat,
                className =? "Vncviewer" --> doFloat,
                isFullscreen             --> doFullFloat
               ]
{--myKeys = 
        [ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
        ++
        [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
        ++
        [ ("M-l", spawn "gnome-screensaver-command -l") ]
        ++
        [ ("M-c", spawn "chromium") ]
        ++
        [ ("M-v", spawn "chromium --incognito") ]
        ++
        [ ("M-S-l", spawn "gnome-screensaver-command -l") ]
        ++
        [ ("M-<F4>", toggleFullFloat) ]
        ++
        [ ("M-<F6>", spawn "xbacklight -dec 10") ]
        ++
        [ ("M-<F7>", spawn "xbacklight -inc 10") ]
        ++        
        [ ("M-<F8>", spawn "amixer set Master toggle") ]
        ++
        [ ("M-<F9>", spawn "amixer set Master 5- unmute") ]
        ++
        [ ("M-<F10>", spawn "amixer set Master 5+ unmute") ] --}

myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), spawn "dmenu_run") ]



toggleFullFloat = withFocused (\w -> do
                                 { floats <- gets (W.floating . windowset);
                                   if M.member w floats
                                   then withFocused $ windows . W.sink
                                   else withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f 
                                 } )

myStartupHook :: X ()
myStartupHook = do setWMName "LG3D" 
                   spawn "~/bin/autostart"

main = do xmproc <- spawnPipe "/usr/bin/xmobar /home/dlangevi/.xmobarrc"
          xmonad $ docks $ defaultConfig 
                  { manageHook  = myManageHook <+> manageHook defaultConfig
                  , terminal = "urxvt"
                  , workspaces  = myWorkspaces
                  , layoutHook  = smartBorders $ avoidStruts $ layoutHook defaultConfig 
	          , borderWidth = 2
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

{--themain = do xmproc <- spawnPipe "xmobar"
          --xmonad $ defaultConfig 
          xmonad $ defaultConfig
                  --{ terminal  = "~/bin/termInit"
                  { terminal  = "xterm"
    		      , borderWidth = 2
                  , workspaces  = myWorkspaces
                  --, startupHook = myStartupHook
		  , XMonad.keys = myKeys <+> XMonad.keys desktopConfig
                  , logHook     = dynamicLogWithPP $ xmobarPP 
                                                      { ppOutput = hPutStrLn xmproc
                                                      , ppCurrent = xmobarColor "#ABABAB" "" . wrap "[" "]"
                                                      , ppTitle = xmobarColor "darkgreen" "" . shorten 100
                                                      }
                  } 

--}









-- $usage
-- To use this module, start with the following @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad
-- > import XMonad.Config.Gnome
-- >
-- > main = xmonad gnomeConfig
--
-- For examples of how to further customize @gnomeConfig@ see "XMonad.Config.Desktop".


-- | Launch the "Run Application" dialog.  gnome-panel must be running for this
-- to work.
gnomeRun :: X ()
gnomeRun = withDisplay $ \dpy -> do
    rw <- asks theRoot
    gnome_panel <- getAtom "_GNOME_PANEL_ACTION"
    panel_run   <- getAtom "_GNOME_PANEL_ACTION_RUN_DIALOG"

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw gnome_panel 32 panel_run 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

