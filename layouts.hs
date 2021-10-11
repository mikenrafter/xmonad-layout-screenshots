import XMonad
import qualified XMonad.StackSet as W
import System.IO (hPutStrLn, Handle)

import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

--{- imports
--{- A-F
import XMonad.Layout.Accordion
import XMonad.Layout.AutoMaster
import XMonad.Layout.AvoidFloats
import XMonad.Layout.BinaryColumn
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Circle
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.ComboP
import XMonad.Layout.Cross
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Dishes
import XMonad.Layout.MultiDishes
import XMonad.Layout.DragPane
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Drawer
import XMonad.Layout.Dwindle (Dwindle(..), Chirality(CW))
import XMonad.Layout.DwmStyle
import XMonad.Layout.FixedColumn
import XMonad.Layout.Fullscreen
--}
--{- G-L
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.GridVariants (SplitGrid(..))
import qualified XMonad.Layout.GridVariants as GV
import XMonad.Layout.Groups
import XMonad.Layout.Groups.Examples
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.Groups.Wmii
import XMonad.Layout.Hidden
import qualified XMonad.Layout.HintedGrid
import XMonad.Layout.HintedTile (HintedTile(..), Alignment(TopLeft))
import qualified XMonad.Layout.HintedTile as HT
import XMonad.Layout.IM
import XMonad.Layout.IfMax
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutBuilder
-- import XMonad.Layout.LayoutBuilderP
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutScreens
import XMonad.Layout.LimitWindows
--}
--{- M-R
import XMonad.Layout.MagicFocus
import XMonad.Layout.Magnifier
import XMonad.Layout.Master
import XMonad.Layout.Maximize
import XMonad.Layout.MessageControl
import XMonad.Layout.Minimize
import XMonad.Layout.Monitor
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle.TabBarDecoration
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.OnHost
import XMonad.Layout.OneBig
import XMonad.Layout.PerScreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizeScreen
import XMonad.Layout.Roledex
--}
--{- S-Z
import XMonad.Layout.ShowWName
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SortedLayout
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.Square
import XMonad.Layout.StackTile
import XMonad.Layout.StateFull
import XMonad.Layout.Stoppable
import XMonad.Layout.SubLayouts
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.TallMastersCombo hiding ((|||))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.TwoPanePersistent
import XMonad.Layout.VoidBorders
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.ZoomRow
--}
--}

--{- main
main = do
 pipe <- spawnPipe mbar
 xmonad $ docks $ ewmh def
  {
 -- manageHook = mmanageHook
 -- handleEventHook = mhandleEventHook
    logHook = mlogHook pipe
 -- startupHook = mstartupHook
  , layoutHook  = mlayoutHook

  , workspaces         = mworkspaces
  , focusFollowsMouse  = False
  , clickJustFocuses   = True
  , modMask     = super
  , terminal    = mterminal

  , borderWidth        = borderW
  , normalBorderColor  = mFG2
  , focusedBorderColor = mFG
  }
  `removeKeysP` [ a | (a,b) <- mkeys]
  `additionalKeysP` mkeys
--}
--{- loghook
mlogHook pipe = do
 dynamicLogWithPP $ xmobarPP
   { ppOutput = \x -> hPutStrLn pipe x
   , ppHidden          = \_ -> ""
   , ppHiddenNoWindows = \_ -> ""
   , ppExtras = []
   -- remove all except layout name, and ws (# of windows)
   , ppOrder  = \(ws:lay:title:extra) -> [ws
    -- remove "Spacing " -- I couldn't be bothered to do this the right way
   , (tail . tail . tail . tail . tail . tail . tail . tail) lay]
   }
--}

mkeys :: [(String, X ())]
mkeys = 
  [ ("M-l",       windows W.focusUp)
  , ("M-h",       windows W.focusDown)
  , ("M-<Tab>",   sendMessage NextLayout)
  , ("M-S-<Tab>", sendMessage FirstLayout)
  ]

mbar = "xmobar /home/v0id/.xmonad/xmobar-layout.conf"

borderW :: Dimension
borderW = 3
mspacing i = spacingRaw True (Border (i*3) (i*3) (i*2) (i*2)) True (Border i i i i) True

-- # of windows to be displayed on these workspaces
mworkspaces = ["1", "2", "5"]

mlayoutHook = avoidStruts $ (spacingWithEdge 5) layouts
super = mod4Mask


mterminal = "alacritty -e fish"
mFG2 = "#888888"
mFG  = "#eeeeee"
mBG  = "#222222"

tabTheme =
 def
 { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=16"
 , activeColor         = mFG
 , activeBorderColor   = mBG
 , activeTextColor     = mBG

 , inactiveColor       = mBG
 , inactiveBorderColor = mBG
 , inactiveTextColor   = mFG
 }

--layouts :: LayoutClass l => l Window
layouts = 
    --{- A-F
    ( Accordion )
  ||| ( BinaryColumn 1.0 32 )
  ||| ( centerMaster Grid )
  ||| ( Circle )
  ||| ( Column 1.6 )
  ||| ( combineTwo (TwoPane 0.03 0.5)  Accordion Accordion )
  ||| ( combineTwoP (TwoPane 0.03 0.5) Accordion Accordion (ClassName "Librewolf") )
  ||| ( simpleCross )
  ||| ( tabbed shrinkText tabTheme ) -- + DecorationMadness
  ||| ( Dishes 2 $ 1/6 )
  ||| ( MultiDishes 2 3 $ 1/6 )
  ||| ( dragPane Horizontal 0.1 0.5 )
  ||| ( ( simpleDrawer 0.01 0.3 (ClassName "Librewolf") ) `onTop` (Tall 1 0.03 0.5) )
  ||| ( Dwindle R CW 1.5 1.1 )
  ||| ( dwmStyle shrinkText tabTheme Grid )
  ||| ( FixedColumn 1 20 80 10 )
      --}
      --{- G-L
  ||| ( gaps [(U,18), (R,23)] $ Tall 1 (3/100) (1/2) )
  ||| ( Grid ) -- hintedGrid changes this
  ||| ( SplitGrid GV.L 2 3 (2/3) (16/10) (5/100) )
  ||| ( GridRatio (4/3) )
  ||| ( HintedTile 1 (3/100) (1/2) TopLeft HT.Tall )
  ||| ( withIM (1/7) (ClassName "Librewolf") Grid )
  ||| ( IfMax 2 Full (Tall 1 (3/100) (1/2)) )
  ||| ( imageButtonDeco shrinkText tabTheme (Tall 1 (3/100) (1/2)) )
  ||| ( (layoutN 2 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) $ simpleTabbed) $ layoutAll (relBox 0.5 0 1 1) $ simpleTabbed )
  ||| ( layoutHints (Tall 1 (3/100) (1/2)) )
      --}
      --{- M-R
  ||| ( magicFocus (Tall 1 (3/100) (1/2) ))
  ||| ( magnifier (Tall 1 (3/100) (1/2)) )
  ||| ( mastered (1/100) (1/2) $ Grid )
  ||| ( maximize (Tall 1 (3/100) (1/2)) )
  ||| ( ModifiedLayout (monitor { prop = ClassName "SomeClass" , rect = Rectangle 0 0 300 300 }) (Tall 1 (3/100) (1/2) ))
  ||| ( mosaic 2 [3,2] )
  ||| ( MosaicAlt M.empty )
  ||| ( mouseResizableTile )
  ||| ( multiCol [1] 4 0.01 0.5 )
  ||| ( named "(re)named-full" Full )
  ||| ( noBorders Full )
  ||| ( noFrillsDeco shrinkText tabTheme Grid )
  ||| ( OneBig (3/4) (3/4) )
  ||| ( reflectHoriz $ Tall 1 (3/100) (1/2) )
  ||| ( resizeHorizontal 40 Full ) -- ?
  ||| ( Roledex )
      --}
      --{- S-Z
  ||| ( simpleDeco shrinkText tabTheme (Tall 1 (3/100) (1/2) ))
  ||| ( simpleFloat )
  ||| ( Simplest )
  ||| ( simplestFloat )
  ||| ( sorted [ClassName "Librewolf"] Grid )
  ||| ( spiral (6/7) )
  ||| ( StackTile 1 (3/100) (1/2) )
  ||| ( StateFull )
  ||| ( simpleTabBar (Tall 1 (3/100) (1/2) ))
  ||| ( tabbed shrinkText tabTheme )
  ||| ( ThreeCol 1 (3/100) (1/2) )
  ||| ( TwoPane (3/100) (1/2) )
  ||| ( TwoPanePersistent Nothing (3/100) (1/2) )
    --}

 --{- manual
 -- ( autoMaster 1 (1/100) Grid )
 -- ( avoidFloats Full )
 -- ( emptyBSP )
 -- ( borderResize Grid )
 -- ( boringWindows )
 -- ( buttonDecoration )
 -- ( DraggingVisualizer )
 -- ( Fullscreen )
 -- ( Groups )
 -- ( Groups.Examples )
 -- ( Groups.Helpers )
 -- ( Groups.Wmii )
 -- ( minimize (Tall 1 (3/100) (1/2)) )
 -- ( ResizableTall 1 (3/100) (1/2) [] )
 -- ( ResizableThreeCol 1 (3/100) (1/2) [] )
 -- ( ShowWName )
 -- ( windowNavigation $ subTabbed $ boringWindows $ Tall 1 (3/100) (1/2)
 -- ( TallMastersCombo -- tmsCombineTwoDefault (Tall 0 (3/100) 0) simpleTabbed -- figure out later
 -- ( VoidBorders ) -- unsuitable?
 -- ( windowSwitcherDecoration shrinkText tabTheme (draggingVisualizer Grid) ) -- manual
 -- ( ZoomRow ) -- like magnifier, but manual?
 --}
 --{- disabled
 ---( DecorationAddons )                                                         -- utils
 ---( Hidden )                                                                   -- bad 4 scrot
 ---( IndependentScreens )                                                       -- bad 4 scrot
 ---( LayoutBuilderP )                                                           -- deprecated
 ---( LayoutCombinators )                                                        -- bad 4 scrot
 ---( LayoutModifier )                                                           -- bad 4 scrot
 ---( LayoutScreens )                                                            -- bad 4 scrot
 ---( LimitWindows )                                                             -- bad 4 scrot
 ---( MessageControl )                                                           -- bad 4 scrot
 ---( MultiToggle )                                                              -- bad 4 scrot
 ---( MultiToggle.Instances )                                                    -- bad 4 scrot
 ---( MultiToggle.TabBarDecoration )                                             -- bad 4 scrot
 ---( OnHost )                                                                   -- bad 4 scrot
 ---( PerScreen )                                                                -- bad 4 scrot
 ---( PerWorkspace )                                                             -- bad 4 scrot
 ---( PositionStoreFloat )                                                       -- bad 4 scrot
 ---( Renamed )                                                                  -- duplicate of named, in this instance
 ---( Spacing )                                                                  -- added globally, for visualization
 ---( Square )                                                                   -- you just hurt my brain
 ---( Stoppable )                                                                -- bad 4 scrots
 ---( ToggleLayouts )                                                            -- bad 4 scrots
 ---( TrackFloating )                                                            -- bad 4 scrots
 ---( WindowArranger )                                                           -- bad 4 scrots
 ---( WindowNavigation )                                                         -- bad 4 scrots
 ---( WorkspaceDir )                                                             -- bad 4 scrots
 --}
