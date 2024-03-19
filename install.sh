#!/bin/bash

if [[ -e ~/.emacs ]]; then
    cp ~/.emacs ~/.emacs.bak.$(date +%d%m%y-%H%m%S)
fi
cp .emacs ~/.emacs
