if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
ZSH_THEME="powerlevel10k/powerlevel10k"

export DISABLE_UNTRACKED_FILES="true"
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
export ZSH_AUTOSUGGEST_MANUAL_REBIND=1

export ZSH="$HOME/.oh-my-zsh"
export ZSH_COMPDUMP="$ZSH/cache/.zcompdump-${HOST}-${ZSH_VERSION}"
source $ZSH/oh-my-zsh.sh

. "$HOME/.cargo/env"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  

export HOMEBREW_NO_ENV_HINTS=1
export HOMEBREW_NO_ANALYTICS=1
export GPG_TTY=$TTY
export LANG=en_US.UTF-8
export PATH="$PATH:/usr/local/opt/python@3.12/libexec/bin"
export PATH=$PATH:$HOME/Library/Python/3.12/bin/
export GOPATH=$HOME/.go
export PATH=$PATH:$(go env GOPATH)/bin

export ALTERNATE_EDITOR="emacsclient -c -a="""
export EDITOR="emacsclient -c -a="""
export VISUAL="emacsclient -c -a="""

brewup () {
	brew update
	brew upgrade
	brew upgrade --cask
	brew cleanup
	brew autoremove
	brew doctor
}

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
