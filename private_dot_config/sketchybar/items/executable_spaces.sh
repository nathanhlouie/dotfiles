#!/usr/bin/env bash

for i in {0..9}
do
  sid=$(($i+1))

  space=(
    associated_space=$sid
    icon.font="$ICON_FONT:Bold:16.0"
    icon.padding_left=10
    icon.padding_right=10
    icon.color=0xffc6a0f6
    background.image="$ASSET_DIR/background.png"
    background.drawing=on
    background.padding_left=0
    background.padding_right=0
    label.drawing=off
    script="$PLUGIN_DIR/spaces.sh"
  )  
  sketchybar \
    --add space space.$sid left \
    --set space.$sid "${space[@]}" \
    --subscribe space.$sid mouse.clicked 
done

separator=(
  icon=
  icon.font="$ICON_FONT:Bold:12.0"
  icon.padding_left=10
  icon.padding_right=10
  icon.color=0xffc6a0f6
  label.drawing=off
  associated_display=active
  background.padding_left=5
  background.padding_right=0
  click_script='yabai -m space --create && sketchybar --trigger space_change'
)

sketchybar \
  --add item separator left \
  --set separator "${separator[@]}"
