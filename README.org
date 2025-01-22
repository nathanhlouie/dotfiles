## dotfiles

Managed using [GNU Stow](https://www.gnu.org/s/stow/manual/stow.html)

```
stow . --target=$HOME
```

The font face on Emacs is unusually bold due to a quirk in OSX font rendering:

```
defaults write org.gnu.Emacs AppleFontSmoothing -int 0
```

To use `λ` as the prompt char, replace the `POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION` in your `.p10k.zsh` config
