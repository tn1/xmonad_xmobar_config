import XMonad

import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import XMonad.Actions.GridSelect

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor

import qualified XMonad.StackSet        as W

import System.Exit

myTerminal                = "urxvtc"

myWorkspaces              = ["1:main", "2:emacs", "3:net", "4:media", "5:other"]

myNormalBorderColor       = "#101010"
myFocusedBorderColor      = "#aaaaaa"
myBorderWidth             = 1

myModMask                 = mod1Mask


myGSConfig                = defaultGSConfig { gs_font = "xft:Dejavu:size=12"
                                            , gs_cellheight = 50
                                            , gs_cellwidth = 120
                                            , gs_originFractX = 0.20
                                            , gs_originFractY = 0.85 }

myKeys = \c -> mkKeymap c $
       [ ("M-<Return>",   spawn $ terminal c)
       , ("M-S-q",        spawn "xmonad --recompile && xmonad --restart")
       , ("M-S-<Esc>",    io (exitWith ExitSuccess))
       , ("M-<Space>",    sendMessage NextLayout)
       , ("M-b",          sendMessage ToggleStruts)

       , ("M-[",          windows W.focusUp)
       , ("M-]",          windows W.focusDown)
       , ("M-a",          windows W.shiftMaster)

       , ("M-q",          kill)
       , ("M-g",          goToSelected $ myGSConfig)

       , ("M-w",          spawn "LANG=ru_RU.utf8 firefox")
       , ("M-e",          spawn "emacs")

       , ("C-M-l",        spawn "slock")
       , ("M-<Esc>",      spawn "~/script/halt_menu.sh")
       , ("M-m",          spawn "~/script/dfm.sh")
       , ("M-p",          spawn "dmenu_run -fn \"-xos4-terminus-medium-r-normal--12-120-72-72-c-60-*-*\" -l 3  -nb \"#cccccc\" -nf \"#111111\" -sb \"#111111\" -sf \"#cccccc\"")
       , ("M-<Print>",    spawn "scrot '%Y-%m-%d_%H-%M-%S.png' -e 'mv $f ~/other/shots/'")
       ] ++
       [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces c) (map show [1..5])
        , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]

myManageHook = composeAll
             [ className =? "Emacs"     --> doF (W.shift "2:emacs")
             , className =? "Firefox"   --> doF (W.shift "3:net")
             , className =? "MPlayer"   --> doFloat
             , className =? "feh"       --> doFloat
             ]


myLayoutHook = onWorkspace "3:net" myFull $ standartLayouts
             where
                standartLayouts = avoidStruts (myTiled ||| myFull)
                myTiled  = Tall 1 (5/100) (1/2)
                myFull   = Full

myLogHook xmobar = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmobar
          , ppCurrent	= xmobarColor "#111111" "#cccccc" . shorten 15
          , ppTitle	= take 0
          , ppLayout	= take 0
          }

main = do
     xmobar     <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

     xmonad  $ defaultConfig {
            terminal                    = myTerminal
            , modMask                   = myModMask
            , workspaces       		= myWorkspaces
            , keys			= myKeys
            , normalBorderColor		= myNormalBorderColor
            , focusedBorderColor        = myFocusedBorderColor
            , borderWidth		= myBorderWidth
            , startupHook		= setDefaultCursor xC_left_ptr
            , manageHook		= manageDocks <+> myManageHook
            , layoutHook                = smartBorders $ myLayoutHook
            , logHook                   = myLogHook xmobar
            }