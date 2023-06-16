local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.skip_close_confirmation_for_processes_named = {
  "bash", "sh", "zsh", "fish"
}
config.window_decorations = "RESIZE"
config.use_fancy_tab_bar = false
config.tab_bar_at_bottom = true
config.tab_max_width = 24
config.window_background_opacity = 0.9
config.macos_window_background_blur = 28 

config.window_padding = {
  left = '2cell',
  right = '2cell',
  top = '1cell',
  bottom = '1cell',
}

config.keys = {
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
}

config.color_scheme = 'Catppuccin Macchiato'
config.font = wezterm.font_with_fallback {
  'Inconsolata',
  'Inconsolata Nerd Font'
}
config.font_size = 15.0

return config
