#!/usr/bin/env bash

HOUR="$(date '+%H')"

if [ "$HOUR" == "" ]; then
  exit 0
fi

case "$HOUR" in
  00|12) ICON="󱑖"
  ;;
  01|13) ICON="󱑋"
  ;;
  02|14) ICON="󱑌"
  ;;
  03|15) ICON="󱑍"
  ;;
  04|16) ICON="󱑎"
  ;;
  05|17) ICON="󱑏"
  ;;
  06|18) ICON="󱑐"
  ;;
  07|19) ICON="󱑑"
  ;;
  08|20) ICON="󱑒"
  ;;
  09|21) ICON="󱑓"
  ;;
  10|22) ICON="󱑔"
  ;;
  11|23) ICON="󱑕"
  ;;
  *) ICON=""
esac

sketchybar --set $NAME icon="$ICON" label="$(date '+%H:%M')"
