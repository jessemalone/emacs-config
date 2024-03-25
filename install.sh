#!/bin/bash

if [[ -e ~/.emacs ]]; then
    cp ~/.emacs ~/.emacs.bak.$(date +%d%m%y-%H%m%S)
fi
cp .emacs ~/.emacs

cp ~/.bashrc ~/.bashrc.bak.$(date +%d%m%y-%H%m%S)
echo <<EOF >>~/.bashrc

vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'
EOF

