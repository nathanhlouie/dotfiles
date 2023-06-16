#!/usr/bin/env bash

battery=(
  background.color=0xffeed49f
  update_freq=120
  script="$PLUGIN_DIR/battery.sh"
)

sketchybar \
  --add item battery right \
  --set battery "${battery[@]}" \
  --subscribe battery power_source_change system_woke
