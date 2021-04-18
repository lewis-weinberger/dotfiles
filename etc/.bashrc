#!/bin/bash

if [[ "$-" != *"i"* ]]; then
    return
fi

export VISUAL=vis
export EDITOR=$VISUAL
export PLAN9=/usr/local/plan9

export MANPATH=$(manpath -g):$PLAN9/man

export PATH=$PATH:$PLAN9/bin
export PATH=$HOME/.cargo/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/go/bin:$PATH
export PATH=$HOME/Applications/bin:$PATH
export PATH=$HOME/factor:$PATH
export PATH=$HOME/.emacs.d/bin:$PATH
export PATH=$HOME/.roswell/bin:$PATH:$PLAN9/bin
export PATH=$HOME/zig:$PATH

export PS1="\e[0;34m\]$\e[0m\] "

os=`uname`
if [[ $os == "Darwin" ]]; then
    alias ls="ls -Gh"
elif [[ $os == "Linux" ]]; then
    alias ls="ls -h --color=auto"
fi

alias ll="ls -l"
alias la="ls -a"
alias lo="ls -o"
