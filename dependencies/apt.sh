#!/bin/bash

sudo apt install libclang-dev llvm clang clangd bear lld libstdc++-12-dev libtool libtool-bin
go install golang.org/x/tools/gopls@latest
