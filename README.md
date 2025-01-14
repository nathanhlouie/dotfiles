## dotfiles

Managed using [GNU Stow](https://www.gnu.org/s/stow/manual/stow.html)

```
stow . --target=$HOME
```

The font face on Emacs is unusually bold due to a quirk in OSX font rendering:

```
defaults write org.gnu.Emacs AppleFontSmoothing -int 0
```
