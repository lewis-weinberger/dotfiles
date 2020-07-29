#!/bin/bash

if [[ "$-" != *"i"* ]]; then
    return
fi

export EDITOR=emacs
export BROWSER="/usr/bin/firejail /usr/bin/firefox"
export PLAN9=/usr/local/plan9
export MANPATH=$(manpath -g):$PLAN9/man
export PATH=$PATH:$PLAN9/bin
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/Applications/bin:$PATH
export PYTHONSTARTUP=$HOME/.pythonrc.py

# Del key
tput smkx
    
# Readline history search
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# Switch to fish
[ -x /bin/fish ] && SHELL=/bin/fish exec /bin/fish
