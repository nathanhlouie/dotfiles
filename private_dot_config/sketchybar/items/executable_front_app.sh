#!/usr/bin/env bash

FRONT_APP_SCRIPT='sketchybar --set $NAME label="$INFO"'

front_app=(
  icon.drawing=off
  label.font="$FONT:Bold:16.0"
  label.color=0xffcad3f5
  label.padding_left=5
  label.padding_right=0
  background.padding_left=0
  background.padding_right=0
  associated_display=active
  script="$FRONT_APP_SCRIPT"
)

sketchybar \
  --add item front_app left \
  --set front_app "${front_app[@]}" \
  --subscribe front_app front_app_switched
