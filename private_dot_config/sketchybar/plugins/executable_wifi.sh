#!/usr/bin/env bash

WIFI=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk 'NR==13 {print $2}')

if [ "$WIFI" = "" ]; then
  WIFI="Not Connected"
  ICON=󰖪
else
  ICON=󰖩
fi

sketchybar --set $NAME label="${WIFI}" icon="${ICON}"
