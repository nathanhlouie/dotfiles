#!/usr/bin/env bash

brew update

brew upgrade

brew upgrade --cask wezterm-nightly --no-quarantine --greedy-latest

brew cleanup

brew autoremove

brew doctor
