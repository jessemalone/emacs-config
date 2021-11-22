#!/bin/bash

set -e

# Haskel language server
cabal update
cabal install haskell-language-server

# js/typescript
npm i -g typescript-language-server typescript

cp ~/.emacs ~/.emacs.bak.$(date +%d%m%y-%H%m%S)
cp .emacs ~/.emacs
