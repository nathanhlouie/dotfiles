#!/usr/bin/env bash

time=(
  icon.font="$ICON_FONT:Bold:17.0"
  icon.drawing=on
  background.color=0xffee99a0
  background.padding_right=0
  update_freq=10
  script="$PLUGIN_DIR/time.sh"
)

sketchybar \
  --add item time right \
  --set time "${time[@]}" \
  --subscribe time system_woke
