#!/usr/bin/env bash

wifi=(
  background.color=0xff8aadf4
  script="$PLUGIN_DIR/wifi.sh"
)

sketchybar \
  --add item wifi right \
  --set wifi "${wifi[@]}" \
  --subscribe wifi wifi_change system_woke 
