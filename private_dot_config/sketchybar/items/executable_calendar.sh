#!/usr/bin/env bash

CALENDAR_APP_SCRIPT='sketchybar --set $NAME label="$(date "+%a %b %d")"'

calendar=(
  icon=
  background.color=0xffa6da95
  update_freq=30
  script="$CALENDAR_APP_SCRIPT"
)

sketchybar \
  --add item calendar right \
  --set calendar "${calendar[@]}" \
  --subscribe calendar system_woke
