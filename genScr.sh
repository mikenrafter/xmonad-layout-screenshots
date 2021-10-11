#!/bin/env fish
set list \
Accordion \
BinaryColumn \
CenteredMaster \
Circle \
Column \
Combo \
ComboP \
Cross \
Decoration \
DecorationMadness \
Dishes \
MultiDishes \
DragPane \
Drawer \
Dwindle \
DwmStyle \
FixedColumn \
Gaps \
Grid \
GridVariants \
HintedGrid \
HintedTile \
IM \
IfMax \
ImageButtonDecoration \
LayoutBuilder \
LayoutHints \
MagicFocus \
Magnifier \
Master \
Maximize \
Monitor \
Mosaic \
MosaicAlt \
MouseResizableTile \
MultiColumns \
Named \
NoBorders \
NoFrillsDecoration \
OneBig \
Reflect \
ResizeScreen \
Roledex \
SimpleDecoration \
SimpleFloat \
Simplest \
SimplestFloat \
SortedLayout \
Spiral \
StackTile \
StateFull \
SubLayouts \
TabBarDecoration \
Tabbed \
ThreeColumns \
TwoPane \
TwoPanePersistent \


mkdir $HOME/xm-scrot/

for item in $list
	echo $item
	wal --saturate 1 --iterative  -i ~/wall-disabled/DTwallpapers -b 222222
	sleep 1.25
	maim -uq | tee "$HOME/xm-scrot/$item-0-5.png"
	xdotool key super+l
	sleep 0.5
	maim -uq | tee "$HOME/xm-scrot/$item-1-5.png"
	xdotool key super+Tab
end
