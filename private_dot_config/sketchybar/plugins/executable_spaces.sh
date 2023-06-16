#!/usr/bin/env bash

update() {
  if [ "$SELECTED" == "true" ]; then
    sketchybar --set $NAME icon= \
                           background.image.drawing=on
  else
    sketchybar --set $NAME icon= \
                           background.image.drawing=off
  fi
}

mouse_clicked() {
  if [ "$BUTTON" == "right" ]; then
    yabai -m space --destroy $SID
    sketchybar --trigger windows_on_spaces --trigger space_change
  else
    yabai -m space --focus $SID 2>/dev/null
  fi
}

case "$SENDER" in
  "mouse.clicked") mouse_clicked
  ;;
  *) update
  ;;
esac
