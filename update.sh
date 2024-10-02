#!/bin/sh

# VIM
cp ~/.vimrc .

# Emacs
mkdir -p .emacs.d/
cp ~/.emacs.d/init.el .emacs.d/
cp -r ~/.emacs.d/snippets/ .emacs.d/
