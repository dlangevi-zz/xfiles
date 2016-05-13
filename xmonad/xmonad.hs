import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeWindows as F
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

import XMonad.Config.Gnome
import XMonad.Hooks.SetWMName

import Data.Monoid
import Data.Map as M

import qualified XMonad.StackSet as W

myWorkspaces = ["1:dev", "2:home", "3:web", "4:chat", "5:email", "6", "7", "8", "9", "0", "-", "="]

myManageHook = composeAll [
                className =? "Gimp"      --> doFloat,
                className =? "Vncviewer" --> doFloat,
                isFullscreen             --> doFullFloat
               ]
myKeys = 
        [ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
        ++
        [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
        ++
        [ ("M-l", spawn "gnome-screensaver-command -l") ]
{--        ++
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

toggleFullFloat = withFocused (\w -> do
                                 { floats <- gets (W.floating . windowset);
                                   if M.member w floats
                                   then withFocused $ windows . W.sink
                                   else withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f 
                                 } )

myStartupHook :: X ()
myStartupHook = do setWMName "LG3D" 
                   spawn "~/bin/autostart"

main = do xmproc <- spawnPipe "xmobar"
          xmonad $ defaultConfig 
          -- xmonad $ gnomeConfig
                  { terminal  = "~/bin/termInit"
    		      , borderWidth = 2
                  , workspaces  = myWorkspaces
                  , normalBorderColor  = "#646464"
                  , focusedBorderColor = "darkgreen"
                  , startupHook = myStartupHook
                  , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig
                  , layoutHook  = smartBorders $ avoidStruts $ layoutHook defaultConfig
                  , logHook     = dynamicLogWithPP $ xmobarPP 
                                                      { ppOutput = hPutStrLn xmproc
                                                      , ppCurrent = xmobarColor "#ABABAB" "" . wrap "[" "]"
                                                      , ppTitle = xmobarColor "darkgreen" "" . shorten 100
                                                      }
                  } `additionalKeysP` myKeys

